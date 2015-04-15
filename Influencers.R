repeating <- read.csv("Remediation Record - Repeating.csv", sep=",", head=T)
remediation.started <- read.csv('Remediation Record - Started Records and No AHG Names.csv', 
                                sep="\t", head=T)

############## Keyword Extract ###############

# Code block to install packages
is.installed <- function(x) {
    is.element(x, installed.packages()[,1])
} 

ifelse(!is.installed('tm'), install.packages('tm'), require(tm))
ifelse(!is.installed('RWeka'), install.packages('RWeka'), require(RWeka))
ifelse(!is.installed('arules'), install.packages('arules'), require(arules))
ifelse(!is.installed('kernlab'), install.packages('kernlab'), require(kernlab))
ifelse(!is.installed('slam'), install.packages('slam'), require(slam))
ifelse(!is.installed('reshape2'), install.packages('reshape2'), require(reshape2))
ifelse(!is.installed('arulesViz'), install.packages('arulesViz'), require(arulesViz))
ifelse(!is.installed('xlsx'), install.packages('xlsx'), require(xlsx))
ifelse(!is.installed('ggplot2'), install.packages('ggplot2'), require(ggplot2))
ifelse(!is.installed('wordcloud'), install.packages('wordcloud'), require(wordcloud))
ifelse(!is.installed('RColorBrewer'), install.packages('RColorBrewer'), require(RColorBrewer))
ifelse(!is.installed('extrafont'), install.packages('extrafont'), require(extrafont))
ifelse(!is.installed('Rgraphviz'), install.packages('Rgraphviz'), require(Rgraphviz))

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

twophrase.tokenizer <- function(x) { 
    NGramTokenizer(x, Weka_control(min = 2, max = 2)) 
}

keyword <- as.data.frame(remediation.started$Status.Summary)
keyword <- Corpus(DataframeSource(keyword))
keyword <- tm_map(keyword, stripWhitespace) # Whitespace
keyword <- tm_map(keyword, content_transformer(tolower))
keyword <- tm_map(keyword, removePunctuation) # Remove punctuation
keyword <- tm_map(keyword, removeNumbers) # Remove numbers
keyword <- tm_map(keyword, removeWords, stopwords(kind='en')) # Remove stop words
keyword <- tm_map(keyword, removeWords, stopwords(kind='SMART')) # Remove stop words
keyword <- tm_map(keyword, stemDocument) # Stemming

# Build Term Document Matrix - using 2 phrase tokenizer
#tdm <- TermDocumentMatrix(keyword, control = list(tokenize = twophrase.tokenizer))
#dtm <- DocumentTermMatrix(keyword, control = list(tokenize = twophrase.tokenizer))
#frequency <- sort(rowSums(as.matrix(tdm)), decreasing = T)
#frequency.df <- data.frame(keyword = names(frequency), freq = frequency) 
#head(frequency.df, n=100)

# Build a simple TDM or DTM
tdm <- TermDocumentMatrix(keyword)
dtm <- DocumentTermMatrix(keyword)

frequency <- sort(rowSums(as.matrix(tdm)), decreasing = T)
frequency.df <- data.frame(keyword = names(frequency), freq = frequency) 
head(frequency.df, n=100)

# Convert the matrices into matrix
tdm.df <- as.matrix(tdm)
dtm.df <- as.matrix(dtm)

# Append the Document Term Matrix to the original Dataframe
remediation.started <- cbind.data.frame(remediation.started, dtm.df)
write.csv(remediation.started, "Remediation Started.csv")

# Remove sparse terms based on frequency weight
tdm.common <- removeSparseTerms(tdm, sparse=0.90) # Top 90%
dtm.common <- removeSparseTerms(dtm, sparse=0.90) # Top 90%
inspect(tdm.common[1:10, 1:10])

tdm.common.matrix <- as.matrix(tdm.common)
dtm.common.matrix <- as.matrix(dtm.common)
dtm.common.df <- as.data.frame(dtm.common.matrix)
tdm.common.matrix <- melt(tdm.common.matrix, value.name="count")

# Visualizes the Top 90% terms across all records using a tile
ggplot(tdm.common.matrix, aes(x=Docs, y=Terms, fill=count)) +
    geom_tile() + 
    ggtitle("Count of Terms across all Remediation Records \nThat Already Started") + 
    scale_fill_gradient(high="deeppink2" , low="#FFFFFF")+
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

######### Association Rules ##########
library(arules)

dtm.common.matrix <- as.matrix(dtm.common)
dtm.common.transactions <- as(dtm.common.matrix, "transactions")
summary.rules <- apriori(dtm.common.matrix, parameter=list(support=0.05, confidence=0.5))
summary.rules <- sort(summary.rules, decreasing=T, by="support")

rules.dataframe = data.frame(
    lhs = labels(lhs(summary.rules))$elements,
    rhs = labels(rhs(summary.rules))$elements, 
    summary.rules@quality)

head(rules.dataframe, 30)
write.csv(rules.dataframe, "Repeating Apriori.csv")

plot(summary.rules, "grouped")

####### Amar and Uma's Scrubbing #######
scrub.matrix <- as.matrix(read.table("clipboard", head=T, sep="\t"))
scrub.matrix[is.na(scrub.matrix)] <- 0
scrub.transactions <- as(scrub.matrix, Class="transactions")

scrub.apriori <- apriori(scrub.matrix, parameter=list(support=0.01, conf=0.05))
scrub.apriori <- sort(scrub.apriori, decreasing=T, by="support")
inspect(scrub.apriori)

scrub.apriori.dataframe = data.frame(
    lhs = labels(lhs(scrub.apriori))$elements,
    rhs = labels(rhs(scrub.apriori))$elements, 
    scrub.apriori@quality)

itemFrequencyPlot(scrub.transactions, type = "relative")
itemFrequencyPlot(scrub.transactions, support = 0.01, horiz=T, sort=T)


########### Test ###########
table1 <- as.matrix(read.table('clipboard', sep="\t", head=T))
table1[is.na(table1)] <- 0
table1.apriori <- apriori(table1, parameter=list(supp=0.5, conf=0.5))
summary(table1.apriori)
inspect(table1.apriori)

table2 <- as.matrix(read.table('apriori quic.csv', sep=",", head=T))
table2[is.na(table2)] <- 0
table2.apriori <- apriori(table2, parameter=list(supp=0.5, conf=0.5))
summary(table2.apriori)
inspect(table2.apriori)

scrub.apriori.dataframe = data.frame(
    lhs = labels(lhs(table2.apriori))$elements,
    rhs = labels(rhs(table2.apriori))$elements, 
    table2.apriori@quality)

########### Action Plans ###########
actionplans <- read.csv("Remediation Action Plans.csv", header=T, sep=",")
actionplans.relationship <- subset(actionplans, Remediation.Category=="Relationship")
actionplans.finance <- subset(actionplans, Remediation.Category=="Finance")
actionplans.service <- subset(actionplans, Remediation.Category=="Service")

########### TM for Export ###########
keyword <- as.data.frame(actionplans.service$Concatenate)
keyword <- Corpus(DataframeSource(keyword))
keyword <- tm_map(keyword, stripWhitespace) # Whitespace
keyword <- tm_map(keyword, content_transformer(tolower)) # To lowercase
keyword <- tm_map(keyword, removeWords, stopwords(kind='en')) # Remove stop words
keyword <- tm_map(keyword, removeWords, stopwords(kind='SMART')) # Remove stop words
keyword <- tm_map(keyword, removeWords, stopwords(kind='german')) # Remove stop words
keyword <- tm_map(keyword, removeWords, stopwords(kind='catalan')) # Remove stop words
keyword <- tm_map(keyword, removeWords, c('papaikonomou', 'george', 'barlow', 'michael',
                                          'january', 'sethi', 'csc', 'bhavna', 'chang',
                                          'february', 'feb', 'march', 'april', 'may',
                                          'june', 'july', 'august', 'sept', 'september',
                                          'october', 'november', 'december', 'dave', 
                                          'reid', 'mike', 'tombs', 'remediation', 'jame',
                                          'jim', 'bob', 'cscs', 'stephen', 'steve', 'jan',
                                          'mark', 'cameron', 'lorenzo', 'sclark', 'frank', 
                                          'ferguson', 'ian', 'gpeel', 'mamzi', 'joergen',
                                          'hodgson', 'chris', 'beheshti', 'abil', 'vijay',
                                          'sergio', 'øyvind', 'martinsson', 'kath',
                                          'cannington', 'bordoni', 'arjunan', 'suzanne',
                                          'suzann', 'ramsay', 'svend', 'jpedgley', 
                                          'hamilton', 'sarah', 'scott', 'sofia', 'narayan',
                                          'smith', 'malcolm', 'phillip', 'action', 'account',
                                          'rebecca', 'beltway', 'foxcroft', 'amber', 'gad',
                                          'rai', 'rais', 'origin', 'close', 'issue', 'issu',
                                          'raise', 'clos', 'item', 'complete', 'complet',
                                          'soebi', 'hening', 'soeby', 'confirm', 'update', 
                                          'confirmed', 'updated'))
keyword <- tm_map(keyword, removePunctuation) # Remove punctuation
keyword <- tm_map(keyword, removeNumbers) # Remove numbers
keyword <- tm_map(keyword, stemDocument) # Stemming

# Build a simple TDM or DTM
tdm <- TermDocumentMatrix(keyword)
dtm <- DocumentTermMatrix(keyword)

frequency <- sort(rowSums(as.matrix(tdm)), decreasing = T)
frequency.df <- data.frame(keyword = names(frequency), freq = frequency) 
head(frequency.df, n=300)

# Remove Sparse Terms
tdm.common <- removeSparseTerms(tdm, sparse=0.98) # Top 90%
dtm.common <- removeSparseTerms(dtm, sparse=0.98) # Top 90%

frequency <- sort(rowSums(as.matrix(tdm.common)), decreasing = T)
frequency.df <- data.frame(keyword = names(frequency), freq = frequency) 
head(frequency.df, n=300)

# Convert the matrices into matrix
tdm.df <- as.matrix(tdm.common)
dtm.df <- as.matrix(dtm.common)

# Append the Document Term Matrix to the original Dataframe
forexport <- cbind.data.frame(actionplans.service$Concatenate, dtm.df)
write.csv(forexport, "Corpus.csv")

write.csv(dtm.df, "Service Concatenate.csv")

# Heatmap
tdm.common.meltmatrix <- melt(tdm.df, value.name="count")
ggplot(tdm.common.meltmatrix, aes(x=Docs, y=Terms, fill=count)) +
    geom_tile() + 
    ggtitle("Count of Terms across all Remediation Records \nThat Already Started") + 
    scale_fill_gradient(high="deeppink2" , low="#FFFFFF")+
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

itemFrequencyPlot(dtm.common, type = "relative")

# Heatmap
tdm.common.meltmatrix <- melt(relationship.tdm.df, value.name="count")
ggplot(tdm.common.meltmatrix, aes(x=Docs, y=Terms, fill=count)) +
    geom_tile() + 
    ggtitle("Count of Terms across all Remediation Records \nThat Already Started") + 
    scale_fill_gradient(high="deeppink2" , low="#FFFFFF")+
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Fixed variables, no need to replace except if changing terms

# Relationsip minsup = 0.02 | minconf = 0.5
#relationship.tdm <- tdm.common
#relationship.dtm <- dtm.common
#relationship.count <- tdm
relationship.tdm.df <- as.matrix(relationship.tdm)
relationship.dtm.df <- as.matrix(relationship.dtm)
relationship.count <- sort(rowSums(as.matrix(relationship.count)), decreasing=T)
relationship.count <- data.frame(keyword=names(relationship.count), freq=relationship.count)

#Wordcloud
relationship.color <- brewer.pal(n=4, name="Paired")
wordcloud(words=relationship.count$keyword, freq=relationship.count$freq, scale=c(8, .3),
          min.freq=5, max.words=Inf,random.order=F, rot.per=.15, colors=relationship.color)

relationship.transactions <- as(relationship.dtm.df, Class="transactions")
relationship.rule <- apriori(relationship.transactions, parameter=list(supp=0.01, conf=.5))
relationship.rule <- sort(relationship.rule, decreasing=T, by="lift")

#Prune
relationship.subset <- is.subset(relationship.rule, relationship.rule)
relationship.subset[lower.tri(relationship.subset, diag=T)] <- NA
relationship.redundant <- colSums(relationship.subset, na.rm=T) >= 1
relationship.pruned <- relationship.rule[!relationship.redundant]
inspect(relationship.pruned)

#Rules in table
relationship.rule.df = data.frame(
    lhs = labels(lhs(relationship.pruned))$elements,
    rhs = labels(rhs(relationship.pruned))$elements, 
    relationship.pruned@quality)
write.csv(relationship.rule.df, "Relationship Rule.csv")

#Plot graph
tiff("Relationship.tiff", width=700, height=600, bg="transparent")
par(mfrow=c(1,2))
plot(relationship.pruned, method="graph",
     control=list(type="items", precision=2, cex=.9, main="Relationship Items"))
plot(relationship.pruned, method="graph", 
     control=list(type="itemsets", precision=2, cex=.9, main="Relationship Itemsets"))
dev.off()
saveAsGraph(head(sort(relationship.rule, by="lift"),1000), file="relationship.graphml")

# Finance minsup = 0.09 | minconf = 0.9
#finance.tdm <- tdm.common
#finance.dtm <- dtm.common
#finance.count <- tdm
finance.tdm.df <- as.matrix(finance.tdm)
finance.dtm.df <- as.matrix(finance.dtm)
finance.count <- sort(rowSums(as.matrix(finance.count)), decreasing=T)
finance.count <- data.frame(keyword=names(finance.count), freq=finance.count)

#Wordcloud
finance.color <- brewer.pal(n=4, name="Paired")
wordcloud(words=finance.count$keyword, freq=finance.count$freq, scale=c(8, .3),
          min.freq=5, max.words=Inf,random.order=F, rot.per=.15, colors=finance.color)

finance.transactions <- as(finance.dtm.df, Class="transactions")
finance.rule <- apriori(finance.transactions, parameter=list(supp=0.1, conf=0.7))
finance.rule <- sort(finance.rule, decreasing=T, by="lift")

#Prune
finance.subset <- is.subset(finance.rule, finance.rule)
finance.subset[lower.tri(finance.subset, diag=T)] <- NA
finance.redundant <- colSums(finance.subset, na.rm=T) >= 1
finance.pruned <- finance.rule[!finance.redundant]
inspect(finance.pruned)

#Rules in table
finance.rule.df = data.frame(
    lhs = labels(lhs(finance.pruned))$elements,
    rhs = labels(rhs(finance.pruned))$elements, 
    finance.pruned@quality)
write.csv(finance.rule.df, "Finance Rule.csv")

#Plot graph
tiff("Finance.tiff", width=700, height=600, bg="transparent")
par(mfrow=c(1,2))
plot(finance.pruned, method="graph",
     control=list(type="items", precision=2, cex=.8, main="Finance Items"))
plot(finance.pruned, method="graph", 
     control=list(type="itemsets", precision=2, cex=.8, main="Finance Itemsets"))
dev.off()
saveAsGraph(finance.rule, file="finance.graphml", type="items")

# Service minsup = 0.05 | minconf = 0.4
#service.tdm <- tdm.common
#service.dtm <- dtm.common
#service.count <- tdm
service.tdm.df <- as.matrix(service.tdm)
service.dtm.df <- as.matrix(service.dtm)
service.count <- sort(rowSums(as.matrix(service.count)), decreasing=T)
service.count <- data.frame(keyword=names(service.count), freq=service.count)

#Wordcloud
service.color <- brewer.pal(n=4, name="Paired")
wordcloud(words=service.count$keyword, freq=service.count$freq, scale=c(8, .3),
          min.freq=5, max.words=Inf,random.order=F, rot.per=.15, colors=service.color)

service.transactions <- as(service.dtm.df, Class="transactions")
service.rule <- apriori(service.transactions, parameter=list(supp=0.04, conf=0.4))
service.rule <- sort(service.rule, decreasing=T, by="lift")

#Prune
service.subset <- is.subset(service.rule, service.rule)
service.subset[lower.tri(service.subset, diag=T)] <- NA
service.redundant <- colSums(service.subset, na.rm=T) >= 1
service.pruned <- service.rule[!service.redundant]
inspect(service.pruned)

#Rules in table
service.rule.df = data.frame(
    lhs = labels(lhs(service.pruned))$elements,
    rhs = labels(rhs(service.pruned))$elements, 
    service.pruned@quality)
write.csv(service.rule.df, "Service Rule.csv")

#Plot graph
tiff("Service.tiff", width=700, height=600, bg="transparent")
par(mfrow=c(1,2))
plot(service.pruned, method="graph",
     control=list(type="items", precision=2, cex=.9, main="Service Items"))
plot(service.pruned, method="graph", 
     control=list(type="itemsets", precision=2, cex=.9, main="Service Itemsets"))
dev.off()
saveAsGraph(service.rule, file="service.graphml", type="items")