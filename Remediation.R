# Code block to install packages
is.installed <- function(x) {
  is.element(x, installed.packages()[,1])
} 

ifelse(!is.installed('tree'), install.packages('tree'), require(tree))
ifelse(!is.installed('party'), install.packages('party'), require(party))
ifelse(!is.installed('partykit'), install.packages('partykit'), require(partykit))
ifelse(!is.installed('rms'), install.packages('rms', dep=T), require(rms))
ifelse(!is.installed('effects'), install.packages('effects', dep=T), require(effects))


#### Remediation RAG Rating and Risk 1 ####
ragrisk <- read.csv("RAG Remediation and Risk 1.csv", head=T, sep=",")

tree1 <- ctree(Remediation.RAG.Status ~ Impact, data = ragrisk)
plot(tree1)

tree1a <- ctree(Remediation.RAG.Status ~ Prob, data = ragrisk)
plot(tree1a)

tree2 <- tree(Remediation.RAG.Status ~ Impact, data = ragrisk)
summary(tree2)
plot(tree2); text(tree2)

modelc1 <- glm(Red ~ Impact, family="binomial", data=ragrisk)
summary(modelc1)


#### Remediation RAG Rating and CSC Impact ####
ragcscimpact <- read.csv("RAG and CSC Impact.csv", head=T, sep=",")
ragcscimpact$Red <- factor(ragcscimpact$Red, levels=c("Yes","No"))
ragcscimpact$Financial <- factor(ragcscimpact$Financial, levels=c("No","Yes"))
ragcscimpact$Reputational <- factor(ragcscimpact$Reputational, levels=c("No","Yes"))
ragcscimpact$Operational <- factor(ragcscimpact$Operational, levels=c("No","Yes"))
ragcscimpact$Political <- factor(ragcscimpact$Political, levels=c("No","Yes"))
ragcscimpact$Contract.Threat <- factor(ragcscimpact$Contract.Threat, levels=c("No","Yes"))

ragcscimpact <- subset(ragcscimpact, Remediation.RAG.Status != "Blue")
summary(ragcscimpact); str(ragcscimpact)
table(ragcscimpact$Remediation.RAG.Status)

modelc2a <- lrm(Red ~ Financial + Reputational + Operational + Political + Contract.Threat, 
                data=ragcscimpact, x=T, y=T)
modelc2a.dist <- datadist(Financial, Political, Contract.Threat)
options(datadist=modelc2a.dist)

validate(modelc2a, method="boot", B=100, bw=T, rule="p", sls=0.05, type="individual")
round(exp(cbind(OddsRatio=coef(modelc2a), confint.default(modelc2a))),2)

modelc2b <- glm(Red ~ Financial + Reputational + Operational + Political + Contract.Threat, 
                data=ragcscimpact, family="binomial")
plot(Effect(focal.predictors = "Political", mod=modelc2b))
plot(allEffects(mod = modelc2b))
plot(modelc2b$coefficients)
summary(modelc2b)

modelc3b <- glm(Red ~ Financial + Reputational + Operational + Contract.Threat, 
                data=ragcscimpact, family="binomial")
modelc3a <- lrm(Red ~ Financial + Reputational + Operational + Contract.Threat, data=ragcscimpact, x=T, y=T)
summary(modelc3b); anova(modelc3a)
modelc3a
validate(modelc3a, method="boot", B=100, bw=T, rule="p", sls=0.05, type="individual")
table(Red=ragcscimpact$Red, Operational=ragcscimpact$Operational)
table(Red=ragcscimpact$Red, Financial=ragcscimpact$Financial)
table(Red=ragcscimpact$Red, Contract.Threat=ragcscimpact$Contract.Threat)
table(Red=ragcscimpact$Red, Reputational=ragcscimpact$Reputational)
table(Red=ragcscimpact$Red, Political=ragcscimpact$Political)
round(exp(cbind(OddsRatio=coef(modelc3a), confint.default(modelc3a))),2)
plot(allEffects(mod = modelc3b))
plot(Effect(focal.predictors = "Operational", mod=modelc3b))
plot(Effect(focal.predictors = "Financial", mod=modelc3b))
plot(Effect(focal.predictors = "Contract.Threat", mod=modelc3b))
hist(modelc3b$residuals)
plot(modelc3b, which=2)

round(allEffects(mod = modelc3b),2)
as.data.frame(predict.glm(object = modelc3b, type = 'response', se.fit=F))
rbind(ragcscimpact[33,1:2], ragcscimpact[45,1:2],ragcscimpact[61,1:2])
