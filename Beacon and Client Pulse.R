library(rattle)
rattle()

cp <- read.csv("Beacon and Client Pulse.csv", header=T, sep=",")
cp$Q1 <- factor(cp$Q1, levels=c("Promoter", "Neutral/Detractor"))

require(devtools)
#import 'gar.fun' from Github
source_gist('62067Q
            37')
gar.fun(crs$target, crs$nnet)
gar.fun()
plot.nnet(crs$nnet)

cp.forest <- read.csv("Client Pulse and Beacon - Forest.csv", header=T, sep=",")
