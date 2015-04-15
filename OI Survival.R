# Top 50 Accounts
options(rf.cores = -1)
require(randomForestSRC)
require(ggRandomForests)
require(survival)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
strCol <- brewer.pal(3, "Set1")[c(2,1,3)]

#### Data Prep ####
oi.dataset <- read.csv('OI Survival.csv', head=T, sep=",")
rownames(oi.dataset) <- oi.dataset$Client.Period # Row labels (Client, Period and GDN)
oi.dataset$Status <- ifelse(oi.dataset$OPERATING.INCOME < 0, yes=T, no=F) # Class of Negative OI
oi.dataset <- oi.dataset[, !(colnames(oi.dataset) %in% c("Client.Period",
                                                         "OPERATING.INCOME",
                                                         "TOTAL.OVERHEADS.AND.ALLOCATIONS"))]
oi.dataset[is.na(oi.dataset)] <- 0 # Replace all NA with 0


#### Non-parametric ####
# Kaplan-Meier non-parametrics survival estimates per Industry
oi.survival.industry <- gg_survival(interval="TimePeriod.Actuals",
                                    censor="Status", by="Industry",
                                    data=oi.dataset)
oi.survival.industry$groups <- factor(oi.survival.industry$groups, 
                                      levels=c("MANUFACTURING",
                                               "DIVERSIFIED",
                                               "INSURANCE",
                                               "PUBLIC SECTOR",
                                               "BANKING & CAPITAL MA"))
oi.survival.industry.plot <- plot(oi.survival.industry) + 
    labs(y = "Survival Probability", x = "Period", color = "Industry", fill = "Industry") + 
    facet_wrap(~groups, nrow = 1) + 
    theme(legend.position="none") + 
    coord_cartesian(y = c(0.1,1.01)) + 
    ggtitle("Per Industry") + 
    coord_cartesian(y = c(0,1.01))

# Kaplan-Meier non-parametrics survival estimates - Roll up
oi.survival <- gg_survival(interval="TimePeriod.Actuals",
                           censor="Status",
                           data=oi.dataset)
oi.survival.rollup.plot <- plot(oi.survival) + 
    labs(y = "Survival Probability", x = "Period") + 
    theme(legend.position="none") + 
    coord_cartesian(y = c(.3,1.01)) + 
    ggtitle("Roll-Up") + 
    coord_cartesian(y = c(0,1.01))

grid.arrange(oi.survival.rollup.plot, oi.survival.industry.plot, nrow=1, 
             widths=1:2, main = "Kaplan-Meier non-parametrics survival estimates")

#### Random Forest ####
survivalmod <- rfsrc(Surv(TimePeriod.Actuals, Status) ~ ., data=oi.dataset, 
                     bootstrap="by.root", ntree=300, 
                     seed=100, statistics=T, membership=T)
plot(survivalmod); survivalmod

# Prediction Estimates
oi.rfsrc <- gg_rfsrc(survivalmod)
oi.rfsrc.plot <- plot(oi.rfsrc, alpha = .2) +
    theme(legend.position = "none") +
    scale_color_manual(values = strCol) + 
    labs(y = "Survival Probability", x = "Period") +
    facet_wrap(~cens) + 
    ggtitle("Roll-up") + 
    coord_cartesian(y = c(0,1.01))

oi.rfsrc.industry <- gg_rfsrc(survivalmod, by="Industry")
oi.rfsrc.industry$group <- factor(oi.rfsrc.industry$group, 
                                  levels=c("MANUFACTURING",
                                           "DIVERSIFIED",
                                           "INSURANCE",
                                           "PUBLIC SECTOR",
                                           "BANKING & CAPITAL table(MA"))
oi.rfsrc.industry.plot <- plot(oi.rfsrc.industry, by="Industry") +
    theme(legend.position = "none") +
    facet_wrap(~group, nrow=1) + 
    labs(y = "Survival Probability", x = "Period") + 
    ggtitle("Per Industry") + 
    coord_cartesian(y = c(0,1.01))

grid.arrange(oi.rfsrc.plot, oi.rfsrc.industry.plot, 
             nrow=1, widths=1:4, 
             main="Random Forest Survival Prediction Estimates",
             sub="TRUE = Negative OI")








# Variable Importance
oi.vimp <- gg_vimp(survivalmod)
plot(oi.vimp) + theme(legend.position = c(.8,.2))+ labs(fill = "VIMP > 0")

oi.error <- gg_error(survivalmod)
plot(oi.error)

oi.mindepth <- gg_minimal_depth(survivalmod)
plot(oi.mindepth)

oi.interaction <- gg_interaction(survivalmod)
plot(oi.interaction, panel=T)

oi.variable <- gg_variable(survivalmod) 
plot(oi.variable)

oi.partial <- partial.rfsrc(survivalmod, partial=T)
oi.partial <- gg_partial(oi.partial)
plot(oi.partial)