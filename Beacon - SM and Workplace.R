# Code block to install packages
is.installed <- function(x) {
  is.element(x, installed.packages()[,1])
} 

#### Packages ####
ifelse(!is.installed('reshape2'), install.packages('reshape2'), require(reshape2))
ifelse(!is.installed('zoo'), install.packages('zoo'), require(zoo))

beacon.sm_and_workplace <- read.csv('Beacon - SM and Workplace.csv', head=T, sep=",")
beacon.sm_and_workplace$MeasurePeriod <- as.Date(beacon.sm_and_workplace$MeasurePeriod, format="%m/%d/%Y")
beacon.sm_and_workplace$RAG.Rating <- factor(beacon.sm_and_workplace$RAG.Rating, levels=c("GREEN", "AMBER", "RED"))
beacon.sm_and_workplace$Red <- factor(beacon.sm_and_workplace$Red, levels=c(1,0), labels=c("Red", "Green/Amber"))
beacon.smwork.servicemanagement <- subset(beacon.sm_and_workplace, Portfolio=="Service Management")
beacon.smwork.workplace <- subset(beacon.sm_and_workplace, Portfolio=="Workplace")
beacon.sm <- merge(x=beacon.smwork.servicemanagement, y=beacon.smwork.workplace, by="Account")

# Service Management ~ Workplace
model1 <- glm(Red.x ~ Measure.y, data=beacon.sm, family="binomial")
summary(model1); anova(model1, test='Chisq')
round(exp(cbind(OddsRatio=coef(model1), confint.default(model1))),2)

model3 <- lm(Measure.x ~ Measure.y, data=beacon.sm)
summary(model3)

# Workplace ~ Service Management
model2 <- glm(Red.y ~ Measure.x, data=beacon.sm, family="binomial")
summary(model2); anova(model2, test='Chisq')
round(exp(cbind(OddsRatio=coef(model2), confint.default(model2))),2)

tree1 <- ctree(Red.x ~ Measure.y, data=beacon.sm)
tree1
plot(tree1)

tree2 <- ctree(Red.y ~ Measure.x, data=beacon.sm)
tree2
plot(tree2)