# Code block to install packages
is.installed <- function(x) {
  is.element(x, installed.packages()[,1])
} 

#### Packages ####
ifelse(!is.installed('reshape2'), install.packages('reshape2'), require(reshape2))
ifelse(!is.installed('zoo'), install.packages('zoo'), require(zoo))
ifelse(!is.installed('gmodels'), install.packages('gmodels'), require(gmodels))
ifelse(!is.installed('rpart'), install.packages('rpart'), require(rpart))
ifelse(!is.installed('rpart.plot'), install.packages('rpart.plot'), require(rpart.plot))
ifelse(!is.installed('rattle'), install.packages('rattle'), require(rattle))
ifelse(!is.installed('tree'), install.packages('tree'), require(tree))
ifelse(!is.installed('party'), install.packages('party'), require(party))
ifelse(!is.installed('partykit'), install.packages('partykit'), require(partykit))

#### Beacon ####
beacon <- read.csv('Beacon Data.csv', head=T, sep=',')
beacon <- beacon[complete.cases(beacon), ] # Removes NAs

# Removes -Rollup-
beacon <- beacon[beacon$Account!='-Rollup-', ]
beacon <- beacon[beacon$Account!='-N/A-', ]
beacon <- beacon[beacon$Region!='-Rollup-', ]
beacon <- beacon[beacon$Region!='-N/A-', ]
beacon <- beacon[beacon$Industry!='-Rollup-', ]
beacon <- beacon[beacon$Industry!='-N/A-', ]
beacon <- beacon[beacon$GDN!='-Rollup-', ]
beacon <- beacon[beacon$GDN!='-N/A-', ]
beacon <- beacon[beacon$Portfolio!='-Rollup-', ]
beacon <- beacon[beacon$Portfolio!='-N/A-', ]

beacon <- beacon[!duplicated(beacon), ] # Removes duplicates if any

beacon$Account <- factor(beacon$Account)
beacon$MeasurePeriod <- as.Date(beacon$MeasurePeriod, format='%m/%d/%Y')
beacon$Month <- as.yearmon(beacon$MeasurePeriod)
beacon$Red <- ifelse(beacon$RAG.Rating == 'RED', 1, 0)
beacon$Red <- factor(beacon$Red, levels=c(1,0), labels=c('Red', 'Green/Amber'))
beacon$SLAReported <- ifelse(beacon$MeasureName == "SLA's Reported", 1, 0)
beacon$SLAMet <- ifelse(beacon$MeasureName == "SLA's Met", 1, 0)
beacon$SLAPenalty <- ifelse(beacon$MeasureName == "SLA Penalties (Thousands)", 1, 0)
beacon$MeasureName <- factor(beacon$MeasureName)

#### Chi-sq ####
attach(beacon)
CrossTable(x=Red, y=SLAReported, expected=T, chisq=T, format='SPSS', digits=0, prop.chisq = F, prop.t=F)
CrossTable(x=Red, y=SLAMet, expected=T, chisq=T, format='SPSS', digits=0, prop.chisq = F, prop.t=F)
CrossTable(x=Red, y=SLAPenalty, expected=T, chisq=T, format='SPSS', digits=0, prop.chisq = F, prop.t=F)

#### Logit Regression ####
model1 <- glm(Red ~ SLAReported, family='binomial')
summary(model1); anova(model1, test='Chisq')
round(exp(cbind(OddsRatio=coef(model1), confint.default(model1))),2)

model2 <- glm(Red ~ SLAReported + SLAMet + SLAPenalty, family='binomial')
summary(model2); anova(model2, test='Chisq')
round(exp(cbind(OddsRatio=coef(model2), confint.default(model2))),2)

model3 <- glm(Red ~ SLAReported + SLAMet, family='binomial')
summary(model3); anova(model3, test='Chisq')
round(exp(cbind(OddsRatio=coef(model3), confint.default(model3))),2)

model4 <- glm(Red ~ SLAReported * SLAMet, family='binomial')
summary(model4); anova(model4, test='Chisq')
round(exp(cbind(OddsRatio=coef(model4), confint.default(model4))),2)

#### SLA Reported ####
beacon.slareported <- subset(beacon, SLAReported==1)
beacon.slareported$MeasureName <- factor(beacon.slareported$MeasureName)
model5 <- glm(Red ~ Measure, family='binomial')
summary(model5); anova(model5, test='Chisq')
round(exp(cbind(OddsRatio=coef(model5), confint.default(model5))),2)

tree1 <- rpart(Red ~ Measure + MeasureName, method='class')
printcp(tree1)
plotcp(tree1)
summary(tree1)
plot(tree1); text(tree1, pretty=1)

tree2 <- tree(Red ~ Measure + MeasureName, data=beacon)
summary(tree2)
plot(tree2); text(tree2)
 
tree3 <- ctree(Red ~ Measure + MeasureName, data=beacon)
tree3
class(tree3)
plot(tree3)

tree4 <- ctree(RAG.Rating ~ Measure + MeasureName, data=beacon)
tree4
class(tree4)
plot(tree4)

tree5 <- ctree(Red ~ MeasureName)
plot(tree5)


table(Red ~ MeasureName)


detach(beacon)
