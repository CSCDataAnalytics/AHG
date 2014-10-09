# Code block to install packages
is.installed <- function(x) {
  is.element(x, installed.packages()[,1])
} 

#### Packages ####
ifelse(!is.installed('reshape2'), install.packages('reshape2'), require(reshape2))
ifelse(!is.installed('zoo'), install.packages('zoo'), require(zoo))
ifelse(!is.installed('rms'), install.packages('rms', dep=T), require(rms))
ifelse(!is.installed('corrplot'), install.packages('corrplot', dep=T), require(corrplot))
ifelse(!is.installed('effects'), install.packages('effects', dep=T), require(effects))
ifelse(!is.installed('gmodels'), install.packages('gmodels'), require(gmodels))
ifelse(!is.installed('ggplot2'), install.packages('ggplot2'), require(ggplot2))
ifelse(!is.installed('gridExtra'), install.packages('gridExtra'), require(gridExtra))
ifelse(!is.installed('stargazer'), install.packages('stargazer'), require(stargazer))
ifelse(!is.installed('lsr'), install.packages('lsr'), require(lsr))
ifelse(!is.installed('MASS'), install.packages('MASS'), require(MASS))
ifelse(!is.installed('vcd'), install.packages('vcd'), require(vcd))
ifelse(!is.installed('MASS'), install.packages('MASS'), require(MASS))

beacon.sm <- read.csv('Beacon - Service Management - SLA Met and Reported.csv', 
                      sep=',', head=T)
beacon.sm$MeasurePeriod <- as.Date(beacon.sm$MeasurePeriod, "%m/%d/%Y")
beacon.sm$Month <- as.character(as.yearmon(beacon.sm$MeasurePeriod))
beacon.sm.slamet <- subset(beacon.sm, MeasureName=="SLA's Met")
beacon.sm.slareported <- subset(beacon.sm, MeasureName=="SLA's Reported")

beacon.sm.slamet.jun <- subset(beacon.sm.slamet, Month=="Jun 2014")
beacon.sm.slamet.aprmay <- subset(beacon.sm.slamet, Month!="Jun 2014")
beacon.sm.slamet.avg <- aggregate(beacon.sm.slamet.aprmay, 
                                  by=list(beacon.sm.slamet.aprmay$Account), FUN=mean)

AprMayAvg <- beacon.sm.slamet.avg$Measure
beacon.sm.slamet.junandavg <- cbind(beacon.sm.slamet.jun, AprMayAvg=AprMayAvg)

# rms package
model1 <- ols(Measure ~ AprMayAvg, data=beacon.sm.slamet.junandavg, x=T, y=T)
model1
model1.testtrain <- validate(model1, method="boot", B=30, bw=FT, 
                            rule="AIC", force=NULL, type="residual", 
                            sls=0.05, aics=0, pr=F)
model1.testtrain
model1 <- lm(Measure ~ AprMayAvg, data=beacon.sm.slamet.junandavg)
with(beacon.sm.slamet.junandavg, plot(y=Measure, x=AprMayAvg))
abline(model1, col="red")
par(mfrow=c(1,2))
plot(model1)

model1.crossvalidate <- validate(model1, method="crossvalidation", 
                         B=10, bw=T, rule="AIC", force=NULL, type="residual", 
                         sls=0.05, aics=0, pr=F)
model1.crossvalidate

aprmay.dist <- datadist(beacon.sm.slamet.aprmay$Measure, beacon.sm.slamet.junandavg$AprMayAvg)
options(aprmay.dist="ddist")
Predict(model1, beacon.sm.slamet.junandavg$Measure, np = 5)

############# SLA Met, Reported and Penalty #############
beacon <- read.csv("Beacon - SLA Met, Reported, Penalty.csv", head=T, sep=",")
beacon <- subset(beacon, SLA.Reported.RAG != "NA")
model1 <- ols(SLA.Met.Measure ~ SLA.Reported.Measure, data=beacon, x=T, y=T)
summary.lm(model1)
validate(model1, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)
plot(allEffects(model1))
ggplot(beacon, aes(x=SLA.Reported.Measure, y=SLA.Met.Measure)) + 
  geom_point() + 
  ggtitle("Scatterplot of SLA Met and SLA Reported") + 
  stat_smooth(method="lm")

beacon.mon <- read.csv("Beacon Monthly - SLA Met and Reported Measures Only.csv", head=T, sep=",")
rownames(beacon.mon) <- beacon.mon$Account.GDN.Portfolio.MeasurePeriod
beacon.mon <- beacon.mon[complete.cases(beacon.mon),]
beacon.mon <- beacon.mon[, -1]

attach(beacon.mon)
names(beacon.mon)
model2 <- ols(Jul.SLA.Met.Measure ~ 
                Jan.SLA.Met.Measure + 
                Feb.SLA.Reported.Measure + 
                Apr.SLA.Met.Measure + 
                May.SLA.Reported.Measure + 
                Jul.SLA.Met.Measure + 
                Apr.May.Avg.SLA.Measure + 
                Jan.and.Feb.Avg.SLA.Met + 
                Apr.May.Avg.SLA.Reported + 
                Apr.May.Jun.Avg.SLA.Reported + 
                Jan.SLA.Reported.Measure + 
                Mar.SLA.Met.Measure + 
                Apr.SLA.Reported.Measure + 
                Jun.SLA.Met.Measure + 
                Jul.SLA.Reported.Measure + 
                Mar.Apr.Avg.SLA.Measure + 
                Apr.May.Jun.Avg.SLA.Measure + 
                Mar.Apr.Avg.SLA.Reported + 
                Jan.and.Feb.Avg.SLA.Reported + 
                Feb.SLA.Met.Measure + 
                Mar.SLA.Reported.Measure + 
                May.SLA.Met.Measure + 
                Jun.SLA.Reported.Measure + 
                May.Jun.Avg.SLA.Measure + 
                Feb.Mar.Avg.SLA.Measure + 
                May.Jun.Avg.SLA.Reported + 
                Feb.Mar.Avg.SLA.Reported, data = beacon.mon, x=T, y=T)
summary.lm(model2)
validate(model2, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model3 <- ols(Jul.SLA.Met.Measure ~ 
               Jan.SLA.Met.Measure + 
               Feb.SLA.Met.Measure + 
               Mar.SLA.Met.Measure + 
               Apr.SLA.Met.Measure + 
               May.SLA.Met.Measure + 
               Jun.SLA.Met.Measure, x=T, y=T)
summary.lm(model3)
validate(model3, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model4 <- ols(Jul.SLA.Met.Measure ~
               Jan.SLA.Reported.Measure + 
               Feb.SLA.Reported.Measure + 
               Mar.SLA.Reported.Measure + 
               Apr.SLA.Reported.Measure + 
               May.SLA.Reported.Measure + 
               Jun.SLA.Reported.Measure + 
               Jul.SLA.Reported.Measure, x=t, y=T)
summary.lm(model4)
validate(model4, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model5 <- ols(Jul.SLA.Met.Measure ~
               May.Jun.Avg.SLA.Measure + 
               Mar.Apr.Avg.SLA.Measure + 
               Apr.May.Jun.Avg.SLA.Measure + 
               May.Jun.Avg.SLA.Reported + 
               Mar.Apr.Avg.SLA.Reported + 
               Apr.May.Jun.Avg.SLA.Reported, x=T, y=T)
summary.lm(model5)
validate(model5, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model6 <- ols(Jul.SLA.Met.Measure ~
               May.Jun.Avg.SLA.Measure + 
               Mar.Apr.Avg.SLA.Measure + 
               Apr.May.Jun.Avg.SLA.Measure, x=T, y=T)
summary.lm(model6)
validate(model6, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model7 <- ols(Jul.SLA.Met.Measure ~
                         May.Jun.Avg.SLA.Reported + 
                         Mar.Apr.Avg.SLA.Reported + 
                         Apr.May.Jun.Avg.SLA.Reported, x=T, y=T)
summary.lm(model7)
validate(model7, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model8 <- ols(Jul.SLA.Met.Measure ~
               May.Jun.Avg.SLA.Measure + 
               Mar.Apr.Avg.SLA.Measure + 
               May.Jun.Avg.SLA.Reported + 
               Mar.Apr.Avg.SLA.Reported, x=T, y=T)
summary.lm(model8)
validate(model8, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model9 <- ols(Jul.SLA.Met.Measure ~
               May.SLA.Met.Measure + 
               Jun.SLA.Met.Measure + 
               May.SLA.Reported.Measure + 
               Jun.SLA.Reported.Measure, x=T, y=T)
summary.lm(model9)
validate(model9, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model10 <- ols(Jul.SLA.Met.Measure ~ 
                 May.Jun.Avg.SLA.Measure + 
                 May.Jun.Avg.SLA.Reported, 
               x=T, y=T)
model10; anova(model10); summary.lm(model10)
plot(allEffects(model10))
validate(model10, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

beacon.mon.reg <- cbind(Jul.SLA.Met.Measure, May.Jun.Avg.SLA.Measure, May.Jun.Avg.SLA.Reported)
corrplot(cor(beacon.mon.reg), method = "number")
qqnorm(model10$residuals); qqline(model10$residuals)

model11 <- ols(Jul.SLA.Met.Measure ~ 
                 May.Jun.Avg.SLA.Measure *
                 May.Jun.Avg.SLA.Reported,
               x=T,y=T)
model11; anova(model11); summary.lm(model11)
validate(model11, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model12 <- ols(Jun.SLA.Met.Measure ~ 
                Apr.May.Avg.SLA.Measure +
                Apr.May.Avg.SLA.Reported, x=T, y=T)
model12; anova(model12); summary.lm(model12)
validate(model11, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model13 <- ols(May.SLA.Met.Measure ~ 
                 Mar.Apr.Avg.SLA.Measure +
                 Mar.Apr.Avg.SLA.Reported)
model13; anova(model13); summary.lm(model13, x=T, y=T)
validate(model13, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model13a <- ols(May.SLA.Met.Measure ~ 
                 Mar.Apr.Avg.SLA.Measure, x=T, y=T)
model13a; anova(model13a); summary.lm(model13a)
validate(model13a, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model14 <- ols(Apr.SLA.Met.Measure ~ 
                 Feb.Mar.Avg.SLA.Measure +
                 Feb.Mar.Avg.SLA.Reported, x=T, y=T)
model14; anova(model14); summary.lm(model14)
validate(model14, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model14a <- ols(Apr.SLA.Met.Measure ~ 
                 Feb.Mar.Avg.SLA.Measure, x=T, y=T)
model14a; anova(model14a); summary.lm(model14a)
validate(model14a, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model15 <- ols(Mar.SLA.Met.Measure ~ 
                 Jan.and.Feb.Avg.SLA.Met +
                 Jan.and.Feb.Avg.SLA.Reported, x=T, y=T)
model15; anova(model15); summary.lm(model15)
validate(model15, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model16 <- ols(Jul.SLA.Met.Measure ~
                 May.Jun.Avg.SLA.Measure + 
                 Mar.Apr.Avg.SLA.Measure, x=T, y=T)
model16; anova(model16); summary.lm(model16)
validate(model16, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model16a <- ols(Jul.SLA.Met.Measure ~
                 May.Jun.Avg.SLA.Measure, x=T, y=T)
model16a; anova(model16a); summary.lm(model16a)
validate(model16a, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model16b <- ols(Jul.SLA.Met.Measure ~
                  Mar.Apr.Avg.SLA.Measure, x=T, y=T)
model16b; anova(model16b); summary.lm(model16b)
validate(model16b, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model17 <- ols(Jun.SLA.Met.Measure ~
                 Apr.May.Avg.SLA.Measure + 
                 Feb.Mar.Avg.SLA.Measure, x=T, y=T)
model17; anova(model17); summary.lm(model17)
validate(model17, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model17a <- ols(Jun.SLA.Met.Measure ~
                 Feb.Mar.Avg.SLA.Measure, x=T, y=T)
model17a; anova(model17a); summary.lm(model17a)
validate(model17, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model18 <- ols(May.SLA.Met.Measure ~
                 Mar.Apr.Avg.SLA.Measure + 
                 Jan.and.Feb.Avg.SLA.Met, x=T, y=T)
model18; anova(model18); summary.lm(model18)
validate(model18, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model18a <- ols(May.SLA.Met.Measure ~
                 Mar.Apr.Avg.SLA.Measure, x=T, y=T)
model18a; anova(model18a); summary.lm(model18a)
validate(model18a, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

model18b <- ols(May.SLA.Met.Measure ~
                  Jan.and.Feb.Avg.SLA.Met, x=T, y=T)
model18b; anova(model18b); summary.lm(model18b)
validate(model18b, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)


detach(beacon)

##### SLA Met, Client Probability, Client Relationship and Referencability #####
beacon2 <- read.csv("SLA Met, Client Prob, Clien Relationship, Referencability.csv", head=T, sep=",")
rownames(beacon2) <- beacon2$Account.MeasurePeriod
beacon2 <- beacon2[, -1]
attach(beacon2)

modela1 <- lm(SLA.Met ~ ., data=beacon2); summary(modela1)
modela2 <- lm(SLA.Met ~ Client.Probability.of.Renewal); summary(modela2)
modela3 <- lm(SLA.Met ~ Client.Relationship.Temperature); summary(modela3)
modela4 <- lm(SLA.Met ~ Referencability...GCRL); summary(modela4)
modela5 <- lm(SLA.Met ~ Referenceability); summary(modela5)
modela6 <- lm(SLA.Met ~ 
                Client.Relationship.Temperature + 
                Referencability...GCRL + 
                Referenceability); summary(modela6)

plot(allEffects(modela3))
detach(beacon2)

##### SLA Met, Client Probability, Client Relationship, Referencability, Financial #####
beacon3 <- read.csv("SLA Met, Client Prob, Clien Relationship, Referencability, Financial.csv", head=T, sep=",")
rownames(beacon3) <- beacon2$Account.MeasurePeriod
beacon3 <- beacon3[, -1]
attach(beacon3)

modelb1 <- lm(SLA.Met ~ ., data=beacon3); summary(modelb1)
modelb2 <- lm(SLA.Met ~ Financial.Performance...DCM....of.Monthly.Budget.YTD.); summary(modelb2)
modelb3 <- lm(SLA.Met ~ Financial.Performance...OI....of.Monthly.Budget.YTD.); summary(modelb3)
modelb4 <- lm(SLA.Met ~ Financial.Performance...Operating.Margin); summary(modelb4)
detach(beacon3)


##### SLA Met, Client Prob and Relationship, Referencability, Financial and Obligation Rate #####
beacon4 <- read.csv("SLA Met, Client Prob, Clien Relationship, Referencability, Financial, Contractual.csv", head=T, sep=",")
rownames(beacon4) <- beacon4$Account.MeasurePeriod
beacon4 <- beacon4[complete.cases(beacon4),]
beacon4 <- beacon4[, -1]
beacon4.jan <- subset(beacon4, Month=="Jan")
beacon4.feb <- subset(beacon4, Month=="Feb")
beacon4.mar <- subset(beacon4, Month=="Mar")
beacon4.apr <- subset(beacon4, Month=="Apr")
beacon4.may <- subset(beacon4, Month=="May")
beacon4.jun <- subset(beacon4, Month=="Jun")
beacon4.jul <- subset(beacon4, Month=="Jul")

attach(beacon4.jan)
CrossTable(y=SLA.Met.RAG.2, x=Client.Probability.of.Renewal.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Client.Relationship.Temperature.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referencability...GCRL.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referenceability.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")

attach(beacon4.feb)
CrossTable(y=SLA.Met.RAG.2, x=Client.Probability.of.Renewal.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Client.Relationship.Temperature.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referencability...GCRL.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referenceability.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")

attach(beacon4.mar)
CrossTable(y=SLA.Met.RAG.2, x=Client.Probability.of.Renewal.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Client.Relationship.Temperature.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referencability...GCRL.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referenceability.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")

attach(beacon4.apr)
CrossTable(y=SLA.Met.RAG.2, x=Client.Probability.of.Renewal.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Client.Relationship.Temperature.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referencability...GCRL.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referenceability.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")

attach(beacon4.may)
CrossTable(y=SLA.Met.RAG.2, x=Client.Probability.of.Renewal.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Client.Relationship.Temperature.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referencability...GCRL.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referenceability.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")

attach(beacon4.jun)
CrossTable(y=SLA.Met.RAG.2, x=Client.Probability.of.Renewal.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Client.Relationship.Temperature.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referencability...GCRL.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referenceability.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")

attach(beacon4.jul)
CrossTable(y=SLA.Met.RAG.2, x=Client.Probability.of.Renewal.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Client.Relationship.Temperature.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referencability...GCRL.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")
CrossTable(y=SLA.Met.RAG.2, x=Referenceability.RAG.2, 
           digits=2, expected=T, prop.t=F, chisq=T, format="SPSS")


detach(beacon4.jul)

#### SLA Met ~ Financials ####
financial <- read.csv("SLA Met and Financials.csv", sep=",", head=T)
colnames(financial) <- c('Account.MeasurePeriod',
                         "Month",
                         "SLA.Met.RAG",
                         "SLA.Met",
                         "Financial.DCM",
                         "Financial.OI",
                         "Financial.OM")

financial$Financial.DCM.Norm <- financial$Financial.DCM-mean(financial$Financial.DCM)/sd(financial$Financial.DCM)
financial$Financial.OI.Norm <- financial$Financial.OI-mean(financial$Financial.OI)/sd(financial$Financial.OI)
financial$Financial.OM.Norm <- financial$Financial.OM-mean(financial$Financial.OM)/sd(financial$Financial.OM)

financial$Financial.DCM.Log <- log(financial$Financial.DCM)
financial$Financial.OI.Log <- log(financial$Financial.OI)
financial$Financial.OM.Log <- log(financial$Financial.OM)


financial$Financial.DCM.Sqrt <- sqrt(financial$Financial.DCM)
financial$Financial.OI.Sqrt <- sqrt(financial$Financial.OI)
financial$Financial.OM.Sqrt <- sqrt(financial$Financial.OM)

financial$Financial.DCM.Inv <- 1/(financial$Financial.DCM)
financial$Financial.OI.Inv <- 1/(financial$Financial.OI)
financial$Financial.OM.Inv <- 1/(financial$Financial.OM)

financial$Financial.DCM.Square <- (financial$Financial.DCM)^2
financial$Financial.OI.Square <- (financial$Financial.OI)^2
financial$Financial.OM.Square <- (financial$Financial.OM)^2

financial$Financial.DCM.Exp <- exp(financial$Financial.DCM)
financial$Financial.OI.Exp <- exp(financial$Financial.OI)
financial$Financial.OM.Exp <- exp(financial$Financial.OM)

financial$SLA.Met.Log <- log(financial$SLA.Met)
financial$SLA.Met.Exp <- exp(financial$SLA.Met)
financial$SLA.Met.Square <- (financial$SLA.Met)^2
financial$SLA.Met.Inv <- 1/(financial$SLA.Met)
financial$SLA.Met.Sqrt <- sqrt(financial$SLA.Met)
financial$SLA.Met.Cube <- (financial$SLA.Met)^3
financial$SLA.Met.Z <- financial$SLA.Met-mean(financial$SLA.Met)/sd(financial$SLA.Met)

corra <- round(cor(financial, use="everything", method="pearson"), digits=2)
corrb <- round(cor(financial, use="everything", method="spearman"), digits=2)
corrc <- round(cor(financial, use="everything", method="kendall"), digits=2)
corrd <- cor(financial, use="everything", method=c("pearson", "kendall", "spearman"))
# Pearson's Product Moment Correlation
corra
# Spearman's Correlation
corrb
# Kendall's Tau
corrc
corrplot(modela1); corrplot(modela2); corrplot(modela3)

attach(financial)

summary(financial)

modela3 <- lm(SLA.Met.Log ~ Financial.DCM + Financial.OI + Financial.OM, data=financial)
modela4 <- boxcox(SLA.Met ~ Financial.DCM + Financial.OI + Financial.OM, data=financial)

hist1 <- qplot(SLA.Met)
hist2 <- qplot(Financial.DCM)
hist3 <- qplot(Financial.OI)
hist4 <- qplot(Financial.OM)
grid.arrange(hist1, hist2, hist3, hist4)

hist1a <- qplot(SLA.Met)
hist2a <- qplot(Financial.DCM.Norm)
hist3a <- qplot(Financial.OI.Norm)
hist4a <- qplot(Financial.OM.Norm)
grid.arrange(hist1a, hist2a, hist3a, hist4a)

hist1b <- qplot(SLA.Met.Log)
hist2b <- qplot(Financial.DCM.Log)
hist3b <- qplot(Financial.OI.Log)
hist4b <- qplot(Financial.OM.Log)
grid.arrange(hist1b, hist2b, hist3b, hist4b)

hist1c <- qplot(SLA.Met.Sqrt)
hist2c <- qplot(Financial.DCM.Sqrt)
hist3c <- qplot(Financial.OI.Sqrt)
hist4c <- qplot(Financial.OM.Sqrt)
grid.arrange(hist1c, hist2c, hist3c, hist4c)

hist1d <- qplot(SLA.Met.Inv)
hist2d <- qplot(Financial.DCM.Inv)
hist3d <- qplot(Financial.OI.Inv)
hist4d <- qplot(Financial.OM.Inv)
grid.arrange(hist1d, hist2d, hist3d, hist4d)

hist1e <- qplot(SLA.Met.Square)
hist2e <- qplot(Financial.DCM.Square)
hist3e <- qplot(Financial.OI.Square)
hist4e <- qplot(Financial.OM.Square)
grid.arrange(hist1e, hist2e, hist3e, hist4e)

hist1f <- qplot(SLA.Met.Exp)
hist2f <- qplot(Financial.DCM.Exp)
hist3f <- qplot(Financial.OI.Exp)
hist4f <- qplot(Financial.OM.Exp)
grid.arrange(hist1f, hist2f, hist3f, hist4f)

histslamet1 <- qplot(financial$SLA.Met.Log)
histslamet2 <- qplot(financial$SLA.Met.Exp)
histslamet3 <- qplot(financial$SLA.Met.Square)
histslamet4 <- qplot(financial$SLA.Met.Inv)
histslamet5 <- qplot(financial$SLA.Met.Sqrt)
histslamet6 <- qplot(financial$SLA.Met.Cube)
histslamet7 <- qplot(financial$SLA.Met.Z)
histslamet8 <- qplot(financial$SLA.Met)
grid.arrange(histslamet1, histslamet2, histslamet3, histslamet4, 
             histslamet5, histslamet6, histslamet7, histslamet8, ncol=4)

grid.arrange(hist2a, hist3a, hist4a, hist2b, hist3b, hist4b, ncol=3)
detach(financial)

financial2 <- financial[,2:4]
modelb1 <- round(cor(financial2, use="everything"), digits=2)
modelb1
corrplot(modelb1)

fin.model1 <- lrm(SLA.Met.RAG ~ Financial.DCM + Financial.OI + Financial.OM, x=T, y=T)
round(exp(cbind(OddsRatio=coef(fin.model1), confint.default(fin.model1))),2)
plot(allEffects(fin.model1))
