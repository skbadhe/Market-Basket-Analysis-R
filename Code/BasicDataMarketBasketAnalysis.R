# Load the libraries
library(arules)
library(arulesViz)
library(datasets)

#default groceries dataset
data(Groceries)

#plot data frequencyplot top 25
itemFrequencyPlot(Groceries,topN=25,type="absolute")

#What are customers likely to buy before buying whole milk
rules <- apriori(Groceries, parameter = list(supp= 0.001, conf = 0.8),
                 appearance = list(default="lhs",rhs='whole milk'),
                 control = list(verbose = F))
rules <- sort(rules, by='confidence', decreasing = TRUE)
inspect(rules[1:5])

#What are customers likely to buy if they purchase whole milk?
rules<- apriori(Groceries, parameter = list(supp=0.001, conf=0.15, minlen=2),
                appearance = list(lhs='whole milk', default='rhs'),
                control = list(verbose = F))
rules <- sort(rules,by='confidence', decreasing = TRUE)
inspect(rules[1:5])

#visualize rules
#rules with support 0.001 and confidence 0.8
rules <- apriori(Groceries,parameter = list(supp = 0.001, conf = 0.8))
#reducing the max length of the rules to 3
#rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
#show 3 digits of numbers
options(digits = 3)
#show top 6 rules
inspect(rules[1:6])

#summary info
summary(rules)

#sort rules with confidence high to low
rules <- sort(rules, by='confidence', decreasing = TRUE)

inspect(rules[1:6])

#removing redundancy
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#visualize data
plot(rules,method="graph",interactive=TRUE)
