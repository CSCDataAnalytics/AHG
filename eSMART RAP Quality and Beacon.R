library(rattle)

building <- TRUE
scoring  <- ! building
crv$seed <- 100

# Load an R data frame.
rapquality <- read.csv("eSMART RAP Quality and Beacon Scores.csv", sep=",", head=T)
rapquality <- rapquality[,-1]

levels <- c(1,0)
labels <- c("Incomplete", "Answered")
rapquality$How.did.the.incident.happen. <- factor(rapquality$How.did.the.incident.happen., 
                                                  levels=levels, labels=labels)
rapquality$What.is.being.done.to.fix.it. <- factor(rapquality$What.is.being.done.to.fix.it., 
                                                   levels=levels, labels=labels)
rapquality$How.can.it.be.prevented. <- factor(rapquality$How.can.it.be.prevented., 
                                              levels=levels, labels=labels)
rapquality$Who.is.responsible. <- factor(rapquality$Who.is.responsible., 
                                         levels=levels, labels=labels)
rapquality$When.will.it.start. <- factor(rapquality$When.will.it.start., 
                                         levels=levels, labels=labels)
rapquality$Needs.Improvement <- factor(rapquality$Needs.Improvement, 
                                       levels=levels, 
                                       labels=c("Needs Improvement", "Complete"))
rapquality$Beacon.SLA.Penalty.RAG <- factor(rapquality$Beacon.SLA.Penalty.RAG, 
                                            levels=levels, labels=c("Green", "Red"))
rapquality$Beacon.SLA.Met.RAG <- factor(rapquality$Beacon.SLA.Met.RAG, 
                                        levels=levels, labels=c("Green/Amber", "Red"))

rapqualitydataset <- rapquality

# Training and Cross Validation Sets
set.seed(100) 
rapqualitynobs <- nrow(rapqualitydataset) # 116 observations 
rapqualitysample <- rapqualitytrain <- sample(nrow(rapqualitydataset), 0.8*rapqualitynobs) # 92 observations
rapqualityvalidate <- sample(setdiff(seq_len(nrow(rapqualitydataset)), rapqualitytrain), 0.2*rapqualitynobs) # 23 observations
rapqualitytest <- NULL

# Set variables
rapqualityinput <- c("How.did.the.incident.happen.", "What.is.being.done.to.fix.it.", "How.can.it.be.prevented.", "Who.is.responsible.",
                     "When.will.it.start.", "Needs.Improvement")
rapqualitycategoric <- c("How.did.the.incident.happen.", "What.is.being.done.to.fix.it.", "How.can.it.be.prevented.", "Who.is.responsible.",
                         "When.will.it.start.", "Needs.Improvement")
rapqualityignore  <- c("Beacon.SLA.Penalty", "Beacon.SLA.Penalty.RAG", "Beacon.SLA.Met")
rapqualitytarget  <- "Beacon.SLA.Met.RAG"
rapqualityrisk    <- NULL
rapqualityident   <- NULL
rapqualitynumeric <- NULL
rapqualityweights <- NULL

################ Rpart ##############
require(rpart, quietly=TRUE)
set.seed(crv$seed)

# Build the Decision Tree model.
rapqualityrpart <- rpart(Beacon.SLA.Met.RAG ~ .,
                         data=rapqualitydataset[rapqualitytrain, c(rapqualityinput, rapqualitytarget)],
                         method="class",
                         parms=list(split="information"),
                         control=rpart.control(usesurrogate=0, 
                                               maxsurrogate=0))

# Generate a textual view of the Decision Tree model.
print(rapqualityrpart)
printcp(rapqualityrpart)

################ Ada Boost ################ 
require(ada, quietly=TRUE)

# Build the Ada Boost model.
set.seed(crv$seed)
rapqualityada <- ada(Beacon.SLA.Met.RAG ~ .,
                     data=rapqualitydataset[rapqualitytrain,c(rapqualityinput, rapqualitytarget)],
                     control=rpart.control(maxdepth=30,
                                           cp=0.010000,
                                           minsplit=20,
                                           xval=10),
                     iter=50)

print(rapqualityada)
round(rapqualityada$model$errs[rapqualityada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(rapqualityada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(rapqualityada))

################ Regression model ################  

# Build a Regression model.
rapqualityglm <- glm(Beacon.SLA.Met.RAG ~ .,
                     data=rapqualitydataset[rapqualitytrain, c(rapqualityinput, rapqualitytarget)],
                     family=binomial(link="logit"))

# Generate a textual view of the Linear model.
print(summary(rapqualityglm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(rapqualityglm)[1],
            attr(logLik(rapqualityglm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            rapqualityglm$null.deviance-rapqualityglm$deviance,
            rapqualityglm$df.null-rapqualityglm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(rapqualityglm$null.deviance-rapqualityglm$deviance,
                   rapqualityglm$df.null-rapqualityglm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
            cor(rapqualityglm$y, rapqualityglm$fitted.values)))
print(anova(rapqualityglm, test="Chisq"))

################ Neural Network ################ 
require(nnet, quietly=TRUE)

# Build the NNet model.
set.seed(100)
rapqualitynnet <- nnet(as.factor(Beacon.SLA.Met.RAG) ~ .,
                       data=rapqualitydataset[rapqualitysample,c(rapqualityinput, rapqualitytarget)],
                       size=15, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
            paste(rapqualitynnet$n, collapse="-"),
            length(rapqualitynnet$wts)))
cat(sprintf("Inputs: %s.\n",
            paste(rapqualitynnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
            names(attr(rapqualitynnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
            sum(residuals(rapqualitynnet) ^ 2)))
print(summary(rapqualitynnet))

library(neuralnet)
neuralmatrix <- model.matrix( ~ Beacon.SLA.Met.RAG +
                                 How.did.the.incident.happen. +
                                 What.is.being.done.to.fix.it. +
                                 How.can.it.be.prevented. +
                                 Who.is.responsible. + 
                                 When.will.it.start. + 
                                 Needs.Improvement,
                             data=rapqualitydataset)

neuralmodel <- neuralnet(Beacon.SLA.Met.RAGRed ~ 
                             How.did.the.incident.happen.Answered +
                             What.is.being.done.to.fix.it.Answered +
                             How.can.it.be.prevented.Answered +
                             Who.is.responsible.Answered + 
                             When.will.it.start.Answered + 
                             Needs.ImprovementComplete, 
                         hidden = 15, threshold = 0.01, linear.output=F, like=T,
                         data=neuralmatrix)
plot.nn(neuralmodel)

################ Plot Decision Tree ################ 
fancyRpartPlot(rapqualityrpart)

# List the rules from the tree using a Rattle support function.
asRules(rapqualityrpart)

################ Plot Ada Boosting Random Forest ################ 
varplot(rapqualityada, type="scores")

################ Plot Neural Net ################ 
summary(neuralmodel); neuralmodel
plot(neuralmodel)

################  Decision Tree Confusion Matrix ################ 
rapqualitypr <- predict(rapqualityrpart, newdata=rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)], type="class")

# Generate the confusion matrix showing counts.
table(rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG, rapqualitypr,
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.
pcme <- function(actual, cl)
{
    x <- table(actual, cl)
    tbl <- cbind(round(x/length(actual), 2),
                 Error=round(c(x[1,2]/sum(x[1,]),
                               x[2,1]/sum(x[2,])), 2))
    names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
    return(tbl)
};
pcme(rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG, rapqualitypr)

# Calculate the overall error percentage.
overall <- function(x)
{
    if (nrow(x) == 2) 
        cat((x[1,2] + x[2,1]) / sum(x)) 
    else
        cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(rapqualitypr, rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG,  
              dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.
avgerr <- function(x) 
    cat(mean(c(x[1,2], x[2,1]) / apply(x, 1, sum))) 
avgerr(table(rapqualitypr, rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG,  
             dnn=c("Predicted", "Actual")))

################ Ada Boost Confusion Matrix ################ 
rapqualitypr <- predict(rapqualityada, newdata=rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)])

# Generate the confusion matrix showing counts.
table(rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG, rapqualitypr,
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.
pcme <- function(actual, cl)
{
    x <- table(actual, cl)
    tbl <- cbind(round(x/length(actual), 2),
                 Error=round(c(x[1,2]/sum(x[1,]),
                               x[2,1]/sum(x[2,])), 2))
    names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
    return(tbl)
};
pcme(rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG, rapqualitypr)

# Calculate the overall error percentage.
overall <- function(x)
{
    if (nrow(x) == 2) 
        cat((x[1,2] + x[2,1]) / sum(x)) 
    else
        cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(rapqualitypr, rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG,  
              dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.
avgerr <- function(x) 
    cat(mean(c(x[1,2], x[2,1]) / apply(x, 1, sum))) 
avgerr(table(rapqualitypr, rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG,  
             dnn=c("Predicted", "Actual")))

################ GLM Confusion Matrix
rapqualitypr <- as.vector(ifelse(predict(rapqualityglm, type="response", newdata=rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]) > 0.5, "Red", "Green/Amber"))

# Generate the confusion matrix showing counts.
table(rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG, rapqualitypr,
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.
pcme <- function(actual, cl)
{
    x <- table(actual, cl)
    tbl <- cbind(round(x/length(actual), 2),
                 Error=round(c(x[1,2]/sum(x[1,]),
                               x[2,1]/sum(x[2,])), 2))
    names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
    return(tbl)
};
pcme(rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG, rapqualitypr)

# Calculate the overall error percentage.
overall <- function(x)
{
    if (nrow(x) == 2) 
        cat((x[1,2] + x[2,1]) / sum(x)) 
    else
        cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(rapqualitypr, rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG,  
              dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.
avgerr <- function(x) 
    cat(mean(c(x[1,2], x[2,1]) / apply(x, 1, sum))) 
avgerr(table(rapqualitypr, rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG,  
             dnn=c("Predicted", "Actual")))

################ Neural Net Confusion Matrix
rapqualitypr <- predict(rapqualitynnet, newdata=rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)], type="class")

# Generate the confusion matrix showing counts.
table(rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG, rapqualitypr,
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.
pcme <- function(actual, cl)
{
    x <- table(actual, cl)
    tbl <- cbind(round(x/length(actual), 2),
                 Error=round(c(x[1,2]/sum(x[1,]),
                               x[2,1]/sum(x[2,])), 2))
    names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
    return(tbl)
};
pcme(rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG, rapqualitypr)

# Calculate the overall error percentage.
overall <- function(x)
{
    if (nrow(x) == 2) 
        cat((x[1,2] + x[2,1]) / sum(x)) 
    else
        cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(rapqualitypr, rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG,  
              dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.
avgerr <- function(x) 
    cat(mean(c(x[1,2], x[2,1]) / apply(x, 1, sum))) 
avgerr(table(rapqualitypr, rapqualitydataset[rapqualityvalidate, c(rapqualityinput, rapqualitytarget)]$Beacon.SLA.Met.RAG,  
             dnn=c("Predicted", "Actual")))