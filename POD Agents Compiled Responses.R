surveydataset <- read.csv('POD Agents Compiled Responses.csv', head=T, sep=',')

drop.column <- c('LOS', 'Email.Address', 'ShortID', 'Total.Score', 'Status') # Drop these cols
surveydataset <- surveydataset[,!(names(surveydataset) %in% drop.column)]
surveydataset <- surveydataset[- grep("#N/A", surveydataset$Wave),] # Remove NA from lookup

##### Code block to install packages #####
is.installed <- function(x) {
    is.element(x, installed.packages()[,1])
} 

ifelse(!is.installed('ggplot2'), install.packages('ggplot2'), require(ggplot2))
ifelse(!is.installed('gridExtra'), install.packages('gridExtra'), require(gridExtra))
ifelse(!is.installed('colorspace'), install.packages('colorspace'), require(colorspace))
ifelse(!is.installed('rpart'), install.packages('rpart'), require(rpart))
ifelse(!is.installed('ada'), install.packages('ada'), require(ada))
ifelse(!is.installed('randomForest'), install.packages('randomForest'), require(randomForest))
ifelse(!is.installed('pROC'), install.packages('pROC'), require(pROC))
ifelse(!is.installed('rattle'), install.packages('rattle'), require(rattle))
ifelse(!is.installed('verification'), install.packages('verification'), require(verification))
ifelse(!is.installed('ROCR'), install.packages('ROCR'), require(ROCR))

seed.number <- 100
survey.dataset <- surveydataset
building <- TRUE; scoring  <- ! building


##### Build the 80-20 training/test datasets #####
set.seed(seed.number) 
survey.nobs <- nrow(survey.dataset)
survey.validate <- NULL
survey.sample <- survey.train <- sample(nrow(survey.dataset), 0.8*survey.nobs)
survey.test <- setdiff(setdiff(seq_len(nrow(survey.dataset)), survey.train), survey.validate)

# Set inputs and target
survey.target  <- "Q1"
survey.input <- c("Wave", "Color", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", 
                  "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16")
survey.categoric <- c("Wave", "Color", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7",
                      "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16")

survey.numeric <- NULL
survey.risk    <- NULL
survey.ident   <- NULL
survey.ignore  <- NULL
survey.weights <- NULL



##### Classification Decision Tree - Use information gain #####
set.seed(seed.number)
survey.rpart <- rpart(Q1 ~ ., 
                      data=survey.dataset[survey.train, c(survey.input, survey.target)],
                      method="class", parms=list(split="information"),
                      control=rpart.control(usesurrogate=0, maxsurrogate=0))

fancyRpartPlot(survey.rpart, sub=""); asRules(survey.rpart) # List the rules from the tree
print(survey.rpart); printcp(survey.rpart) # More detailed info on relative error and
# 10-fold cross-validation error with the lowest value of complexity 



##### Ada Boost - 200 interations using default Decision Tree options #####
set.seed(seed.number)
survey.ada <- ada(Q1 ~ .,
                  data=survey.dataset[survey.train,c(survey.input, survey.target)],
                  control=rpart.control(maxdepth=30, cp=0.010000,
                                        minsplit=20, xval=10),
                  iter=200) 

plot(survey.ada, kappa=T) # How much iteration to reduce error rate?
print(survey.ada); summary(survey.ada)

varplot(survey.ada, type='scores') # Relative importance of the variables 
print(sort(names(listAdaVarsUsed(survey.ada)))) # Variables actually used in tree construction
print(listAdaVarsUsed(survey.ada)) # Frequency of variables actually used



##### Random Forest #####
set.seed(seed.number)
survey.rf <- randomForest(Q1 ~ .,
                          data=survey.dataset[survey.sample,c(survey.input, survey.target)], 
                          ntree=1000,
                          mtry=4,
                          importance=T,
                          na.action=na.roughfix,
                          replace=F)

print(survey.rf); plot(survey.rf)
roc(survey.rf$y, as.numeric(survey.rf$predicted)) # Calculate the Area Under the Curve (AUC).
ci.auc(survey.rf$y, as.numeric(survey.rf$predicted)) # Calculate the AUC Confidence Interval.

# List the importance of the variables in scale
rn <- round(importance(survey.rf, scale=T), 2); rn[order(rn[,3], decreasing=TRUE),]
varImpPlot(survey.rf, main="Variable Importance Random Forest")

# Plot the error rate against the number of trees.
plot(survey.rf, main="Error Rates Random Forest")
legend("right", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)



#### Regression model ####
survey.glm <- glm(Q1 ~ .,
                  data=survey.dataset[survey.train, c(survey.input, survey.target)],
                  family=binomial(link="logit"))

anova(survey.glm, test="Chisq") # ANOVA
print(summary(survey.glm)) # Generate a textual view of the Linear model.

plot(survey.glm)

# Chi-square p-value:
round(dchisq(survey.glm$null.deviance-survey.glm$deviance, survey.glm$df.null-survey.glm$df.residual),2)

survey.glm$null.deviance-survey.glm$deviance # Null/Residual deviance difference
survey.glm$df.null-survey.glm$df.residual # degrees of freedom

logLik(survey.glm)[1] # Log likelihood
attr(logLik(survey.glm), "df") # degrees of freedom

#Pseudo R-Square (optimistic):
cor(survey.glm$y, survey.glm$fitted.values)



##### ROC Curve on test set #####

# Decision Tree
survey.pr.rpart <- predict(survey.rpart, newdata=survey.dataset[survey.test, c(survey.input, survey.target)])[,2]
no.miss.rpart   <- na.omit(survey.dataset[survey.test, c(survey.input, survey.target)]$Q1)
miss.list.rpart <- attr(no.miss.rpart, "na.action")
attributes(no.miss.rpart) <- NULL

if (length(miss.list.rpart))
{
    pred.rpart <- prediction(survey.pr.rpart[-miss.list.rpart], no.miss.rpart)
} else
{
    pred.rpart <- prediction(survey.pr.rpart, no.miss.rpart)
}

pe.rpart <- performance(pred.rpart, "tpr", "fpr")
au.rpart <- performance(pred.rpart, "auc")@y.values[[1]]
pd.rpart <- data.frame(fpr=unlist(pe.rpart@x.values), tpr=unlist(pe.rpart@y.values))
p.rpart <- ggplot(pd.rpart, aes(x=fpr, y=tpr))
p.rpart <- p.rpart + geom_line(colour="red")
p.rpart <- p.rpart + xlab("False Positive Rate") + ylab("True Positive Rate")
p.rpart <- p.rpart + ggtitle("ROC Curve Decision Tree Test Set")
p.rpart <- p.rpart + theme(plot.title=element_text(size=10))
p.rpart <- p.rpart + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p.rpart <- p.rpart + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au.rpart, 2)))

# Ada Boost
survey.pr.ada <- predict(survey.ada, newdata=survey.dataset[survey.test, c(survey.input, survey.target)], type="prob")[,2]
no.miss.ada   <- na.omit(survey.dataset[survey.test, c(survey.input, survey.target)]$Q1)
miss.list.ada <- attr(no.miss.ada, "na.action")
attributes(no.miss.ada) <- NULL

if (length(miss.list.ada))
{
    pred.ada <- prediction(survey.pr.ada[-miss.list.ada], no.miss.ada)
} else
{
    pred.ada <- prediction(survey.pr.ada, no.miss.ada)
}

pe.ada <- performance(pred.ada, "tpr", "fpr")
au.ada <- performance(pred.ada, "auc")@y.values[[1]]
pd.ada <- data.frame(fpr=unlist(pe.ada@x.values), tpr=unlist(pe.ada@y.values))
p.ada <- ggplot(pd.ada, aes(x=fpr, y=tpr))
p.ada <- p.ada + geom_line(colour="red")
p.ada <- p.ada + xlab("False Positive Rate") + ylab("True Positive Rate")
p.ada <- p.ada + ggtitle("ROC Curve Ada Boost Test Set")
p.ada <- p.ada + theme(plot.title=element_text(size=10))
p.ada <- p.ada + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p.ada <- p.ada + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au.ada, 2)))

# Random Forest
survey.pr.rf <- predict(survey.rf, 
                        newdata=na.omit(survey.dataset[survey.test, c(survey.input, survey.target)]), 
                        type="prob")[,2]
no.miss.rf   <- na.omit(na.omit(survey.dataset[survey.test, 
                                               c(survey.input, survey.target)])$Q1)
miss.list.rf <- attr(no.miss.rf, "na.action")
attributes(no.miss.rf) <- NULL

if (length(miss.list.rf))
{
    pred.rf <- prediction(survey.pr.rf[-miss.list.rf], no.miss.rf)
} else
{
    pred.rf <- prediction(survey.pr.rf, no.miss.rf)
}

pe.rf <- performance(pred.rf, "tpr", "fpr")
au.rf <- performance(pred.rf, "auc")@y.values[[1]]
pd.rf <- data.frame(fpr=unlist(pe.rf@x.values), tpr=unlist(pe.rf@y.values))
p.rf <- ggplot(pd.rf, aes(x=fpr, y=tpr))
p.rf <- p.rf + geom_line(colour="red")
p.rf <- p.rf + xlab("False Positive Rate") + ylab("True Positive Rate")
p.rf <- p.rf + ggtitle("ROC Curve Random Forest Test set")
p.rf <- p.rf + theme(plot.title=element_text(size=10))
p.rf <- p.rf + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p.rf <- p.rf + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au.rf, 2)))


# Logistic Regression
survey.pr.glm <- predict(survey.glm, type="response", 
                         newdata=survey.dataset[survey.test, c(survey.input, survey.target)])
no.miss.glm   <- na.omit(survey.dataset[survey.test, c(survey.input, survey.target)]$Q1)
miss.list.glm <- attr(no.miss.glm, "na.action")
attributes(no.miss.glm) <- NULL

if (length(miss.list.glm))
{
    pred.glm <- prediction(survey.pr.glm[-miss.list.glm], no.miss.glm)
} else
{
    pred.glm <- prediction(survey.pr.glm, no.miss.glm)
}

pe.glm <- performance(pred.glm, "tpr", "fpr")
au.glm <- performance(pred.glm, "auc")@y.values[[1]]
pd.glm <- data.frame(fpr=unlist(pe.glm@x.values), tpr=unlist(pe.glm@y.values))
p.glm <- ggplot(pd.glm, aes(x=fpr, y=tpr))
p.glm <- p.glm + geom_line(colour="red")
p.glm <- p.glm + xlab("False Positive Rate") + ylab("True Positive Rate")
p.glm <- p.glm + ggtitle("ROC Curve Logistic Regression Test Set")
p.glm <- p.glm + theme(plot.title=element_text(size=10))
p.glm <- p.glm + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p.glm <- p.glm + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au.glm, 2)))


grid.arrange(p.rpart, p.ada, p.rf, p.glm) # Plot all ROC



##### Obtain probability scores for test sets #####
survey.predict.rpart <- predict(survey.rpart, newdata=survey.dataset[survey.test, c(survey.input)])[,2]
survey.predict.ada <- predict(survey.ada, newdata=survey.dataset[survey.test, c(survey.input)], type="prob")[,2]
survey.predict.rf <- predict(survey.rf, newdata=na.omit(survey.dataset[survey.test, c(survey.input)]), type="prob")[,2]
survey.predict.glm <- predict(survey.glm, type="response", newdata=survey.dataset[survey.test, c(survey.input)])
sdata <- survey.dataset[survey.test,]

# Output the combined data
avg.prediction <- rowMeans(cbind(survey.predict.rpart, survey.predict.ada, 
                        survey.predict.rf, survey.predict.glm))
survey.predict.data <- cbind(sdata, avg.prediction,
                              survey.predict.rpart, survey.predict.ada,
                              survey.predict.rf, survey.predict.glm)
