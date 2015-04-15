library(gmodels)
library(rms)
library(vcd)
library(effects)


rap <- read.csv("eSMART RAP Quality.csv", head=T, sep=",")
summary(rap[,2:7])
rap$How.did.the.incident.happen <- factor(rap$How.did.the.incident.happen, levels=c(1,0), labels=c("Fault", "Complete"))
rap$What.is.being.done.to.fix.it <- factor(rap$What.is.being.done.to.fix.it, levels=c(1,0), labels=c("Fault", "Complete"))
rap$How.can.it.be.prevented <- factor(rap$How.can.it.be.prevented, levels=c(1,0), labels=c("Fault", "Complete"))
rap$Who.is.responsible <- factor(rap$Who.is.responsible, levels=c(1,0), labels=c("Fault", "Complete"))
rap$When.will.it.start <- factor(rap$When.will.it.start, levels=c(1,0), labels=c("Fault", "Complete"))
rap$Needs.Improvement <- factor(rap$Needs.Improvement, levels=c(1,0), labels=c("Fault", "Complete"))

CrossTable(y=rap$How.did.the.incident.happen, x=rap$What.is.being.done.to.fix.it, digits=2, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=T, format="SPSS")
CrossTable(y=rap$How.did.the.incident.happen, x=rap$How.can.it.be.prevented, digits=2, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=T, format="SPSS")
CrossTable(y=rap$How.did.the.incident.happen, x=rap$Who.is.responsible, digits=2, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=T, format="SPSS")
CrossTable(y=rap$How.did.the.incident.happen, x=rap$When.will.it.start, digits=2, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=T, format="SPSS")
CrossTable(y=rap$How.did.the.incident.happen, x=rap$Needs.Improvement, digits=2, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=T, format="SPSS")

rapmodel1 <- glm(How.did.the.incident.happen ~ 
                     (What.is.being.done.to.fix.it-1) +
                     (How.can.it.be.prevented-1) + 
                     (Who.is.responsible-1) + 
                     (When.will.it.start-1) + 
                     (Needs.Improvement-1), data=rap, x=T, y=T, family="binomial")
summary(rapmodel1)
anova(rapmodel1)
plot(allEffects(rapmodel1))

assocstats(table(rap$How.did.the.incident.happen, rap$What.is.being.done.to.fix.it))
assocstats(table(rap$How.did.the.incident.happen, rap$How.can.it.be.prevented))
assocstats(table(rap$How.did.the.incident.happen, rap$Who.is.responsible))
assocstats(table(rap$How.did.the.incident.happen, rap$When.will.it.start))
assocstats(table(rap$How.did.the.incident.happen, rap$Needs.Improvement))

rapmodel2 <- glm(Needs.Improvement ~ What.is.being.done.to.fix.it-1, data=rap, family="binomial")
summary(rapmodel2)
plot(allEffects(rapmodel2))

assocstats(table(rap$What.is.being.done.to.fix.it, rap$How.can.it.be.prevented))
CrossTable(y=rap$What.is.being.done.to.fix.it, x=rap$How.can.it.be.prevented, digits=2, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=T, format="SPSS")

prevent <- read.table("clipboard", head=T, sep="\t")
prevent$How.can.it.be.prevented <- factor(prevent$How.can.it.be.prevented, levels=c(1,0), labels=c("Incomplete", "Answered"))
prevent$Repeat <- ifelse(prevent$Repeat=="Repeat",1,0)
prevent$Repeat <- factor(prevent$Repeat, levels=c(1,0), labels=c("Repeat", "Single"))
prevent.model1 <- glm(Repeat ~ How.can.it.be.prevented-1, data=prevent, family="binomial")
summary(prevent.model1)
plot(allEffects(prevent.model1))
CrossTable(x=prevent$Repeat, y=prevent$How.can.it.be.prevented, digits=2, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=T, format="SPSS")
assocplot(table(prevent$How.can.it.be.prevented, prevent$Repeat))

ggplot(data=penalty, aes(x=Avg.Beacon.Penalty, y=Avg.eSMART.Penalty)) + geom_point(size=5, alpha=0.8, col="red") + 
    ggtitle("Correlation of Averages of eSMART and Beacon Penalties") + stat_smooth(method="lm") + 
    annotate("text", label="r = 0.65", x = 18, y = 1.5) 

