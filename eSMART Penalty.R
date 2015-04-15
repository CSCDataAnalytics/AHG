require(ggplot2)

#### Month ####
dec.esmart.kpisla <- read.csv('C:\\Users\\acuyugan\\Desktop\\actualsextract.csv', head=T, sep=",")
dec.esmart <- subset(dec.esmart.kpisla, SLA.or.KPI == "SLA")

ggplot(dec.esmart, aes(x=Month, y=log(Penalty))) + geom_boxplot() + ggtitle("Distribution of Penalties per Month\nin Natural Logarithm")
pairwise.t.test(dec.esmart$Penalty, g=dec.esmart$Month, p.adjust.method='none')

#### Industry ####
diversified <- subset(dec.esmart, Industry == 'Diversified')
financial <- subset(dec.esmart, Industry == 'Financial Services')
manufacturing <- subset(dec.esmart, Industry == 'Manufacturing')

ggplot(dec.esmart, aes(x=Month, y=log(Penalty))) + geom_boxplot() + 
    ggtitle("Distribution of Penalties per Industry\nin Natural Logarithm") + 
    facet_wrap(~Industry)
pairwise.t.test(diversified$Penalty, g=diversified$Month, p.adjust.method='none')
pairwise.t.test(financial$Penalty, g=financial$Month, p.adjust.method='none')
pairwise.t.test(manufacturing$Penalty, g=manufacturing$Month, p.adjust.method='none')


#### GDN ####
account <- subset(dec.esmart, GDN == 'Account')
datacenter <- subset(dec.esmart, GDN == 'Data Center')
platform <- subset(dec.esmart, GDN == 'Platform')
programproj <- subset(dec.esmart, GDN == 'Program & Project Mngt')
servicemgmt <- subset(dec.esmart, GDN == 'Service Management')
workplace <- subset(dec.esmart, GDN == 'Workplace')

ggplot(dec.esmart, aes(x=Month, y=log(Penalty))) + geom_boxplot() + 
    ggtitle("Distribution of Penalties per GDN\nin Natural Logarithm") + 
    facet_wrap(~GDN)
pairwise.t.test(account$Penalty, g=account$Month, p.adjust.method='none')
pairwise.t.test(datacenter$Penalty, g=datacenter$Month, p.adjust.method='none')
pairwise.t.test(platform$Penalty, g=platform$Month, p.adjust.method='none')
pairwise.t.test(programproj$Penalty, g=programproj$Month, p.adjust.method='none')
pairwise.t.test(servicemgmt$Penalty, g=servicemgmt$Month, p.adjust.method='none')
pairwise.t.test(workplace$Penalty, g=workplace$Month, p.adjust.method='none')


#### Account ####
abbott <- subset(dec.esmart, Client == 'Abbott Laboratories')
pairwise.t.test(abbott$Penalty, g=abbott$Month, p.adjust.method='none')

acco <- subset(dec.esmart, Client == 'ACCO')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

airbus <- subset(dec.esmart, Client == 'Airbus N.A. Engineering')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

aon <- subset(dec.esmart, Client == 'Aon')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

avaya <- subset(dec.esmart, Client == 'Avaya Inc.')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

baker <- subset(dec.esmart, Client == 'Baker & Taylor')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

beckman <- subset(dec.esmart, Client == 'Beckman Coulter - BCI')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

bhp <- subset(dec.esmart, Client == 'BHP Billiton Petroleum')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

boeing <- subset(dec.esmart, Client == 'Boeing')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

bombardier <- subset(dec.esmart, Client == 'Bombardier Transportation')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

bt <- subset(dec.esmart, Client == 'BT Concert')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

carefusion <- subset(dec.esmart, Client == 'Carefusion Corporation')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

cargill <- subset(dec.esmart, Client == 'Cargill')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

chrysler <- subset(dec.esmart, Client == 'Chrysler')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

cscinternal <- subset(dec.esmart, Client == 'CSC services - internal OLAs')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

csci <- subset(dec.esmart, Client == 'CSCi')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

dominion <- subset(dec.esmart, Client == 'Dominion Diamond Corporation')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

dupont <- subset(dec.esmart, Client == 'DuPont')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

eaton <- subset(dec.esmart, Client == 'Eaton')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

ebay <- subset(dec.esmart, Client == 'eBay')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

ets <- subset(dec.esmart, Client == 'Educational Testing Service')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

elc <- subset(dec.esmart, Client == 'Estee Lauder')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

gd <- subset(dec.esmart, Client == 'GD')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

gwp <- subset(dec.esmart, Client == 'General Growth Properties')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

ingalls <- subset(dec.esmart, Client == 'Ingalls Shipbuilding')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

ir <- subset(dec.esmart, Client == 'Ingersoll Rand')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

kemper <- subset(dec.esmart, Client == 'Kemper Corporation')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

minera <- subset(dec.esmart, Client == 'Minera Yanacocha S.R.L.')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

motorolamobility <- subset(dec.esmart, Client == 'Motorola Mobility')
pairwise.t.test(acco$Penalty, g=acco$Month, p.adjust.method='none')

motorolasol <- subset(dec.esmart, Client == 'Motorola Solutions')
pairwise.t.test(motorolasol$Penalty, g=motorolasol$Month, p.adjust.method='none')

nationalgrid <- subset(dec.esmart, Client == 'National Grid')
pairwise.t.test(nationalgrid$Penalty, g=nationalgrid$Month, p.adjust.method='none')
ggplot(nationalgrid, aes(x=Month, y=log(Penalty))) + geom_boxplot() + 
    ggtitle("National Grid Penalties\nDistribution in Natural Logarithm")

nfip <- subset(dec.esmart, Client == 'NFIP')
pairwise.t.test(nfip$Penalty, g=nfip$Month, p.adjust.method='none')

nokia <- subset(dec.esmart, Client == 'Nokia')
pairwise.t.test(nokia$Penalty, g=nokia$Month, p.adjust.method='none')

nyse <- subset(dec.esmart, Client == 'NYSE Euronext')
pairwise.t.test(nyse$Penalty, g=nyse$Month, p.adjust.method='none')

pfizer <- subset(dec.esmart, Client == 'Pfizer')
pairwise.t.test(pfizer$Penalty, g=pfizer$Month, p.adjust.method='none')

pinto <- subset(dec.esmart, Client == 'Pinto Valley Mining Corp')
pairwise.t.test(pinto$Penalty, g=pinto$Month, p.adjust.method='none')

raytheon <- subset(dec.esmart, Client == 'Raytheon')
pairwise.t.test(raytheon$Penalty, g=raytheon$Month, p.adjust.method='none')
ggplot(raytheon, aes(x=Month, y=log(Penalty))) + geom_boxplot() + 
    ggtitle("Raytheon Penalties\nDistribution in Natural Logarithm")

skagit <- subset(dec.esmart, Client == 'Skagit Valley Hospital')
pairwise.t.test(skagit$Penalty, g=skagit$Month, p.adjust.method='none')

standard <- subset(dec.esmart, Client == 'Standard Register')
pairwise.t.test(standard$Penalty, g=standard$Month, p.adjust.method='none')

textron <- subset(dec.esmart, Client == 'Textron')
pairwise.t.test(textron$Penalty, g=textron$Month, p.adjust.method='none')
ggplot(textron, aes(x=Month, y=log(Penalty))) + geom_boxplot() + 
    ggtitle("Textron Penalties\nDistribution in Natural Logarithm")

thyssen <- subset(dec.esmart, Client == 'Thyssen Krupp Elevator')
pairwise.t.test(thyssen$Penalty, g=thyssen$Month, p.adjust.method='none')

toyota <- subset(dec.esmart, Client == 'Toyota Motor Sales')
pairwise.t.test(toyota$Penalty, g=toyota$Month, p.adjust.method='none')

ubs <- subset(dec.esmart, Client == 'UBS')
pairwise.t.test(ubs$Penalty, g=ubs$Month, p.adjust.method='none')

uphs <- subset(dec.esmart, Client == 'University of Pennsylvania Health System (UPHS)')
pairwise.t.test(uphs$Penalty, g=uphs$Month, p.adjust.method='none')

utc <- subset(dec.esmart, Client == 'UTC')
pairwise.t.test(utc$Penalty, g=utc$Month, p.adjust.method='none')

verizon <- subset(dec.esmart, Client == 'Verizon Business')
pairwise.t.test(verizon$Penalty, g=verizon$Month, p.adjust.method='none')

vf <- subset(dec.esmart, Client == 'VF Corporation')
pairwise.t.test(vf$Penalty, g=vf$Month, p.adjust.method='none')

visa <- subset(dec.esmart, Client == 'Visa')
pairwise.t.test(visa$Penalty, g=visa$Month, p.adjust.method='none')

vwr <- subset(dec.esmart, Client == 'VWR (Americas)')
pairwise.t.test(vwr$Penalty, g=vwr$Month, p.adjust.method='none')

westinghouse <- subset(dec.esmart, Client == 'Westinghouse Electric Company')
pairwise.t.test(westinghouse$Penalty, g=westinghouse$Month, p.adjust.method='none')

xerox <- subset(dec.esmart, Client == 'Xerox')
pairwise.t.test(xerox$Penalty, g=xerox$Month, p.adjust.method='none')

zurichasp <- subset(dec.esmart, Client == 'Zurich ASP III')
pairwise.t.test(zurichasp$Penalty, g=zurichasp$Month, p.adjust.method='none')

zurichewp <- subset(dec.esmart, Client == 'Zurich eWP')
pairwise.t.test(zurichewp$Penalty, g=zurichewp$Month, p.adjust.method='none')

zurichisp <- subset(dec.esmart, Client == 'Zurich ISP')
pairwise.t.test(zurichisp$Penalty, g=zurichisp$Month, p.adjust.method='none')


#### Correl ####
esmart.sla <- read.table('clipboard', head=T, sep="\t")
summary(lm(esmart.sla$Average.of.Penalty ~ esmart.sla$Count.of.SLA.ID))
ggplot(esmart.sla, aes(x=Count.of.SLA.ID, y=Average.of.Penalty)) + geom_point(aes(col=GDN)) + stat_smooth(method="lm")




reg <- read.table('clipboard', head=T, sep="\t")
summary(lm(reg$Account ~ reg$Period))