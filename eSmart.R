# Code block to install packages
is.installed <- function(x) {
  is.element(x, installed.packages()[,1]) } 

library(rms)
library(RTextTools)
library(effects)
ifelse(!is.installed('tree'), install.packages('tree'), require(tree))
ifelse(!is.installed('party'), install.packages('party'), require(party))
ifelse(!is.installed('partykit'), install.packages('partykit'), require(partykit))
ifelse(!is.installed('tm'), install.packages('tm'), require(tm))
ifelse(!is.installed('RWeka'), install.packages('RWeka'), require(RWeka))
ifelse(!is.installed('vcd'), install.packages('vcd'), require(vcd))
ifelse(!is.installed('gmodels'), install.packages('gmodels'), require(gmodels))


# Parallel Computing
library(parallel)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

#### eSmart Data ####
esmart <- read.csv("eSmart.csv", header=T, sep=",")
set.seed(68435)
esmart.sample <- esmart[sample(nrow(esmart), size = 10000, replace = F), ]

keyword <- as.data.frame(esmart.sample$Description)
keyword <- Corpus(DataframeSource(keyword))
keyword <- tm_map(keyword, stripWhitespace) # Whitespace
# keyword <- tm_map(keyword, tolower) # Convert to lowercase - temp broken after update
keyword <- tm_map(keyword, removePunctuation) # Remove punctuation
keyword <- tm_map(keyword, removeNumbers) # Remove numbers
keyword <- tm_map(keyword, removeWords, stopwords(kind='en')) # Remove stop words
keyword <- tm_map(keyword, removeWords, stopwords(kind='SMART')) # Remove stop words
keyword <- tm_map(keyword, stemDocument) # Stemming

twophrase.tokenizer <- function(x) { 
  NGramTokenizer(x, Weka_control(min = 2, max = 2)) }

dtm <- DocumentTermMatrix(keyword)
dtm.df <- as.data.frame(as.matrix(dtm))

tdm <- TermDocumentMatrix(keyword)
frequency <- sort(rowSums(as.matrix(tdm)), decreasing = T)
frequency <- data.frame(keyword = names(frequency), freq = frequency) 
head(frequency, n=15)

esmart.df <- cbind(esmart.sample, dtm.df) # Combine eSmart description and corpus

# Logistic Regression
esmart1 <- glm(Achieved2 ~ 
                 incident + 
                 availabl+ 
                 time+ 
                 resolut+ 
                 prioriti+ 
                 servic+ 
                 sever+ 
                 respons+ 
                 standard+ 
                 complet+ 
                 hour+ 
                 csl+ 
                 server+ 
                 applic+ 
                 day+ 
                 critic+ 
                 bsl+ 
                 silver+ 
                 restor+ 
                 resolv,
               data=esmart.df
               family="binomial")
summary(esmart1); anova(esmart1, test = "Chisq")
plot(allEffects(esmart1))

# Prediction via Logistic based on Significant Predictors
model2 <- glm(Fail ~ abandon + answer + busi + call + day + manag + midrang + 
                network + order + sev + sla + within, data = logit.df, family="binomial")
anova(model2, test = "Chisq"); summary(model2)
model2.effect <- allEffects(model2)
png(filename = "eSmart Keyword Effects Plot.png", width = 1000, height = 700)
plot(model2.effect)
dev.off()

### Conditional Inference Tree
esmart.tree1 <- ctree(Achieved2 ~ Industry + GDN + Portfolio, data=esmart)
plot(esmart.tree1); esmart.tree1
table(predict(esmart.tree1), esmart$Portfolio)
table(predict(esmart.tree1), esmart$GDN)
table(predict(esmart.tree1), esmart$Region)
table(predict(esmart.tree1), esmart$Industry)

table(esmart$Portfolio, esmart$Achieved2) # Portfolio
table(esmart$GDN, esmart$Achieved2) # GDN
table(esmart$Region, esmart$Achieved2) # Region
table(esmart$Industry, esmart$Achieved2) # Industry

esmart.tree2 <- ctree(Achieved2 ~ Industry, data=esmart)
plot(esmart.tree2); esmart.tree2
esmart.tree3 <- ctree(Achieved2 ~ GDN, data=esmart)
plot(esmart.tree3); esmart.tree3
esmart.tree4 <- ctree(Achieved2 ~ Portfolio, data=esmart)
plot(esmart.tree4); esmart.tree4
esmart.tree5 <- ctree(Achieved2 ~ GDN + Industry, data=esmart)
plot(esmart.tree5); esmart.tree5
esmart.tree6 <- ctree(Achieved2 ~ GDN + Portfolio, data=esmart)
plot(esmart.tree6); esmart.tree6
esmart.tree7 <- ctree(Achieved2 ~ Industry + Portfolio, data=esmart)
plot(esmart.tree7); esmart.tree7


#### eSmart Logistic Regression - Industry and Portfolio ####
esmartdata <- read.csv("eSmart Logit.csv", sep=",", head=T)
esmartdata$Achieved2 <- factor(esmartdata$Achieved2, levels=0:1, labels=c("No", "Yes"))

esmartdata$Industry <- factor(esmartdata$Industry, levels=c(0,1,2,3,4), 
                              labels=c("Financial Services",
                                       "Manufacturing",
                                       "Diversified",
                                       "International Public Sector",
                                       "Healthcare"))
esmartdata$Portfolio <- factor(esmartdata$Portfolio, levels=0:10, 
                              labels=c("Platform",
                                       "Applications",
                                       "Data Center",
                                       "Workplace",
                                       "Service Management",
                                       "Account",
                                       "Program & Project Mngt",
                                       "Cyber",
                                       "Business Process Services",
                                       "Consulting",
                                       "Cloud"))

esmartdata$Industry.Financial <- factor(esmartdata$Industry.Financial, levels=0:1, labels=c("No", "Yes"))
esmartdata$Industry.Manufacturing <- factor(esmartdata$Industry.Manufacturing, levels=0:1, labels=c("No", "Yes"))
esmartdata$Industry.Diversified <- factor(esmartdata$Industry.Diversified, levels=0:1, labels=c("No", "Yes"))
esmartdata$Industry.Public <- factor(esmartdata$Industry.Public, levels=0:1, labels=c("No", "Yes"))
esmartdata$Industry.Healthcare <- factor(esmartdata$Industry.Healthcare, levels=0:1, labels=c("No", "Yes"))

esmartdata$Portfolio.Platform <- factor(esmartdata$Portfolio.Platform, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.Apps <- factor(esmartdata$Portfolio.Apps, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.DataCenter <- factor(esmartdata$Portfolio.DataCenter, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.Workplace <- factor(esmartdata$Portfolio.Workplace, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.ServiceMgmt <- factor(esmartdata$Portfolio.ServiceMgmt, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.Account <- factor(esmartdata$Portfolio.Account, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.ProjMgmt <- factor(esmartdata$Portfolio.ProjMgmt, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.Cyber <- factor(esmartdata$Portfolio.Cyber, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.BPS <- factor(esmartdata$Portfolio.BPS, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.Consulting <- factor(esmartdata$Portfolio.Consulting, levels=0:1, labels=c("No", "Yes"))
esmartdata$Portfolio.Cloud <- factor(esmartdata$Portfolio.Cloud, levels=0:1, labels=c("No", "Yes"))

industry <- as.data.frame.matrix(table(Industry=esmartdata$Industry, Achieved2=esmartdata$Achieved2))
industry$Yes80 <- (industry$No / .2) * .8
portfolio <- as.data.frame.matrix(table(Portfolio=esmartdata$Portfolio, Achieved2=esmartdata$Achieved2))
portfolio$Yes80 <- (portfolio$No / .2) * .8

esmartmod1a <- glm(Achieved2 ~ Industry-1 + Portfolio-1, data=esmartdata, family="binomial")
esmartmod1b <- lrm(Achieved2 ~ Industry-1 + Portfolio-1, data=esmartdata, x=T, y=T)
summary(esmartmod1a); esmartmod1b
round(exp(cbind(OddsRatio=coef(esmartmod1a), confint.default(esmartmod1a))),2)
summary(esmartmod1a, test="Chisq"); anova(esmartmod1b)
plot(Effect(focal.predictors = "Industry", mod=esmartmod1a))
plot(Effect(focal.predictors = "Portfolio", mod=esmartmod1a))

esmartmod2a <- glm(esmartdata$Achieved2 ~ esmartdata$Industry.Financial + 
                     esmartdata$Industry.Manufacturing + 
                     esmartdata$Industry.Diversified +
                     esmartdata$Industry.Public +
                     esmartdata$Industry.Healthcare + 
                     esmartdata$Portfolio.Platform + 
                     esmartdata$Portfolio.Apps + 
                     esmartdata$Portfolio.DataCenter + 
                     esmartdata$Portfolio.Workplace + 
                     esmartdata$Portfolio.ServiceMgmt + 
                     esmartdata$Portfolio.Account + 
                     esmartdata$Portfolio.ProjMgmt + 
                     esmartdata$Portfolio.Cyber + 
                     esmartdata$Portfolio.BPS + 
                     esmartdata$Portfolio.Consulting + 
                     esmartdata$Portfolio.Cloud,
                   family="binomial")

summary(esmartdata)
summary(esmartmod2a)

#### Random sample of 80% on Achieved2 = Yes ####
industry <- as.data.frame.matrix(table(Industry=esmartdata$Industry, Achieved2=esmartdata$Achieved2))
industry$Yes80 <- (industry$No / .2) * .8
portfolio <- as.data.frame.matrix(table(Portfolio=esmartdata$Portfolio, Achieved2=esmartdata$Achieved2))
portfolio$Yes80 <- (portfolio$No / .2) * .8

industry
portfolio

table(esmartdata$Industry, esmartdata$Achieved2) # Industry
table(esmartdata$Portfolio, esmartdata$Achieved2) # Portfolio

colSums(industry); colSums(portfolio)

esmart.no <- subset(esmartdata, Achieved2 == "No")
esmart.yes <- subset(esmartdata, Achieved2 == "Yes")

esmart.yes <- esmart.yes[sample(nrow(esmart.yes), size = 4376, replace = F), ]
esmartnew <- rbind(esmart.no, esmart.yes)

esmart.yes.industry.financial <- subset(esmart.yes, Industry == "Financial Services")
esmart.yes.industry.manufacturing <- subset(esmart.yes, Industry == "Manufacturing")
esmart.yes.industry.diversified <- subset(esmart.yes, Industry == "Diversified")
esmart.yes.industry.public <- subset(esmart.yes, Industry == "International Public Sector")
esmart.yes.industry.healthcare <- subset(esmart.yes, Industry == "Healthcare")

esmart.no.industry.financial <- subset(esmart.no, Industry == "Financial Services")
esmart.no.industry.manufacturing <- subset(esmart.no, Industry == "Manufacturing")
esmart.no.industry.diversified <- subset(esmart.no, Industry == "Diversified")
esmart.no.industry.public <- subset(esmart.no, Industry == "International Public Sector")
esmart.no.industry.healthcare <- subset(esmart.no, Industry == "Healthcare")

#Dump
#esmart.yes.industry.financial <- esmart.yes.industry.financial[sample(nrow(esmart.yes.industry.financial), size = 1128, replace = F), ]
#esmart.yes.industry.manufacturing <- esmart.yes.industry.manufacturing[sample(nrow(esmart.yes.industry.manufacturing), size = 1088, replace = F), ]
#esmart.yes.industry.diversified <- esmart.yes.industry.diversified[sample(nrow(esmart.yes.industry.diversified), size = 1928, replace = F), ]
#esmart.yes.industry.public <- esmart.yes.industry.public[sample(nrow(esmart.yes.industry.public), size = 176, replace = F), ]
#esmart.yes.industry.healthcare <- esmart.yes.industry.healthcare[sample(nrow(esmart.yes.industry.healthcare), size = 56, replace = F), ]

#Dump
#esmart.industry <- rbind(esmart.no, 
#                         esmart.yes.industry.financial,
 #                        esmart.yes.industry.manufacturing,
  #                       esmart.yes.industry.diversified,
   #                      esmart.yes.industry.public,
    #                     esmart.yes.industry.healthcare)

esmart.yes.portfolio.platform <- subset(esmart.yes, Portfolio == "Platform")
esmart.yes.portfolio.applcations <- subset(esmart.yes, Portfolio == "Applications")
esmart.yes.portfolio.datacenter <- subset(esmart.yes, Portfolio == "Data Center")
esmart.yes.portfolio.workplace <- subset(esmart.yes, Portfolio == "Workplace")
esmart.yes.portfolio.servicemanagement <- subset(esmart.yes, Portfolio == "Service Management")
esmart.yes.portfolio.account <- subset(esmart.yes, Portfolio == "Account")
esmart.yes.portfolio.projmanagement <- subset(esmart.yes, Portfolio == "Program & Project Mngt")
esmart.yes.portfolio.cyber <- subset(esmart.yes, Portfolio == "Cyber")
esmart.yes.portfolio.bps <- subset(esmart.yes, Portfolio == "Business Process Services")
esmart.yes.portfolio.consulting <- subset(esmart.yes, Portfolio == "Consulting")
esmart.yes.portfolio.cloud <- subset(esmart.yes, Portfolio == "Cloud")

esmart.no.portfolio.platform <- subset(esmart.no, Portfolio == "Platform")
esmart.no.portfolio.applcations <- subset(esmart.no, Portfolio == "Applications")
esmart.no.portfolio.datacenter <- subset(esmart.no, Portfolio == "Data Center")
esmart.no.portfolio.workplace <- subset(esmart.no, Portfolio == "Workplace")
esmart.no.portfolio.servicemanagement <- subset(esmart.no, Portfolio == "Service Management")
esmart.no.portfolio.account <- subset(esmart.no, Portfolio == "Account")
esmart.no.portfolio.projmanagement <- subset(esmart.no, Portfolio == "Program & Project Mngt")
esmart.no.portfolio.cyber <- subset(esmart.no, Portfolio == "Cyber") # Exclude
esmart.no.portfolio.bps <- subset(esmart.no, Portfolio == "Business Process Services")
esmart.no.portfolio.consulting <- subset(esmart.no, Portfolio == "Consulting")
esmart.no.portfolio.cloud <- subset(esmart.no, Portfolio == "Cloud") # Exdclude

# Dump
#esmart.yes.portfolio.platform <- esmart.yes.portfolio.platform[sample(nrow(esmart.yes.portfolio.platform), size = 604, replace = F), ]
#esmart.yes.portfolio.applcations <- esmart.yes.portfolio.applcations[sample(nrow(esmart.yes.portfolio.applcations), size = 440, replace = F), ]
#esmart.yes.portfolio.datacenter <- esmart.yes.portfolio.datacenter[sample(nrow(esmart.yes.portfolio.datacenter), size = 260, replace = F), ]
#esmart.yes.portfolio.workplace <- esmart.yes.portfolio.workplace[sample(nrow(esmart.yes.portfolio.workplace), size = 1400, replace = F), ]
#esmart.yes.portfolio.servicemanagement <- esmart.yes.portfolio.servicemanagement[sample(nrow(esmart.yes.portfolio.servicemanagement), size = 1340, replace = F), ]
#esmart.yes.portfolio.account <- esmart.yes.portfolio.account[sample(nrow(esmart.yes.portfolio.account), size = 200, replace = F), ]
#esmart.yes.portfolio.projmanagement <- esmart.yes.portfolio.projmanagement[sample(nrow(esmart.yes.portfolio.projmanagement), size = 92, replace = F), ]
#esmart.yes.portfolio.bps <- esmart.yes.portfolio.bps[sample(nrow(esmart.yes.portfolio.bps), size = 20, replace = F), ]
#esmart.yes.portfolio.consulting <- esmart.yes.portfolio.consulting[sample(nrow(esmart.yes.portfolio.consulting), size = 20, replace = F), ]

esmart.new <- rbind(esmart.no.portfolio.platform,
                    esmart.no.portfolio.applcations,
                    esmart.no.portfolio.datacenter,
                    esmart.no.portfolio.workplace,
                    esmart.no.portfolio.servicemanagement,
                    esmart.no.portfolio.account,
                    esmart.no.portfolio.projmanagement,
                    esmart.no.portfolio.bps,
                    esmart.no.portfolio.consulting,
                    esmart.yes.portfolio.platform,
                    esmart.yes.portfolio.applcations,
                    esmart.yes.portfolio.datacenter,
                    esmart.yes.portfolio.workplace,
                    esmart.yes.portfolio.servicemanagement,
                    esmart.yes.portfolio.account,
                    esmart.yes.portfolio.projmanagement,
                    esmart.yes.portfolio.bps,
                    esmart.yes.portfolio.consulting)

esmartmodel.industry <- glm(Achieved2 ~ Industry-1, data=esmart.industry, family="binomial")
summary(esmartmodel.industry)
round(exp(cbind(OddsRatio=coef(esmartmodel.industry), confint.default(esmartmodel.industry))),2)
plot(allEffects(mod=esmartmodel.industry))

esmartmodel.portfolio <- glm(Achieved2 ~ Portfolio-1, data=esmart.portfolio, family="binomial")
summary(esmartmodel.portfolio)
round(exp(cbind(OddsRatio=coef(esmartmodel.portfolio), confint.default(esmartmodel.portfolio))),2)
plot(allEffects(mod=esmartmodel.portfolio))

esmartnew.model1 <- glm(Achieved2 ~ Industry-1 + Portfolio-1, data=esmart.new, family="binomial")
summary(esmartnew.model1)
round(exp(cbind(OddsRatio=coef(esmartnew.model1), confint.default(esmartnew.model1))),2)
plot(allEffects(mod=esmartnew.model1))
plot(Effect(focal.predictors = "Industry", mod=esmartnew.model1))
plot(Effect(focal.predictors = "Portfolio", mod=esmartnew.model1))

esmartnew.model2 <- lrm(Achieved2 ~ Industry-1 + Portfolio-1, data=esmart.new, x=T, y=T)
validate(esmartnew.model2, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

table(esmart.new$Industry, esmart.new$Achieved2) # Industry
table(esmart.new$Portfolio, esmart.new$Achieved2) # Portfolio

CrossTable(esmart.new$Industry, esmart.new$Achieved2, expected = T, prop.r = F, prop.c = F, prop.t = F)

# No Healthcare
esmart.new.nohealthcare <- subset(esmart.new, Industry != "Healthcare")

esmartnew.model3 <- glm(Achieved2 ~ Industry-1 + Portfolio-1, data=esmart.new.nohealthcare, family="binomial")
summary(esmartnew.model3)
round(exp(cbind(OddsRatio=coef(esmartnew.model3), confint.default(esmartnew.model3))),2)
plot(allEffects(mod=esmartnew.model3))
plot(Effect(focal.predictors = "Industry", mod=esmartnew.model3))
plot(Effect(focal.predictors = "Portfolio", mod=esmartnew.model3))

esmartnew.model4 <- lrm(Achieved2 ~ Industry-1 + Portfolio-1, data=esmart.new.nohealthcare, x=T, y=T)
validate(esmartnew.model4, method="boot", rule="p", B=100, sls=0.05, type="individual", bw=T)

table(esmart.new.nohealthcare$Industry, esmart.new.nohealthcare$Achieved2) # Industry
table(esmart.new.nohealthcare$Portfolio, esmart.new.nohealthcare$Achieved2) # Portfolio

CrossTable(esmart.new.nohealthcare$Industry, esmart.new.nohealthcare$Achieved2, expected = T, prop.r = F, prop.c = F, prop.t = F)
CrossTable(esmart.new.nohealthcare$Portfolio, esmart.new.nohealthcare$Achieved2, expected = T, prop.r = F, prop.c = F, prop.t = F)
