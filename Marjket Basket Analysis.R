install.packages("arules")
install.packages("shiny")
library("arules")
install.packages("arulesViz")
library("arulesViz")
library("RColorBrewer")
library("ggplot2")
library("shiny")

df <- read.transactions(file ="ElectronidexTransactions2017.csv", format="basket",
 sep = ",",rm.duplicates = T,header=F)

 
inspect (df[1]) # You can view the transactions. Is there a way to see a certain # of transactions?
length (df) # Number of transactions.
size (df) # Number of items per transaction
LIST(df) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(df)# To see the item labels
summary(df)

#Visualizations
itemFrequencyPlot(df,topN=10,col="blue",main='Relative Item Frequency Plot',type="absolut"
                 ,horiz=T)

par(mar=c(2,10,2,2),mfrow=c(1,1))
barplot(sort(table(unlist(LIST(df))))[1:10],horiz=T,las=1,col="orange")   

image(sample(df,1200))

crossTable(df,measure="lift",sort=T)[1:4,1:4]

#FREQUENT ITEMSETS AND RULES
it_2=apriori(df,parameter = list(supp=0.1,minlen=2,target="frequent"))
inspect(head(sort(it_2,by="support",decreasing=T)))

rules=apriori(df,parameter=list(supp=0.005,conf=0.1,minlen=2,target="rules"))
inspect(sort(rules,by="count",decreasing=T))

#Setting parameters
clevels=seq(from=0.95, to =0.05, by=-0.05)
rul_sup04=NULL
rul_sup03=NULL

for(i in 1:length(clevels)){ 
  rul_sup04[i]= length(apriori(df,parameter=list(supp=0.04,conf=clevels[i],target="rules")))
}
for(i in 1:length(clevels)){ 
  rul_sup03[i]= length(apriori(df,parameter=list(supp=0.03,conf=clevels[i],target="rules")))
}

# Create data frame with all metrics to be plotted
nb_rules = data.frame(rul_sup04, rul_sup03,
                      clevels)

# Number of rules found with a support level of 40% and 30%
ggplot(data=nb_rules, aes(x=clevels)) +
  # Lines and points for rules_sup04
  geom_line(aes(y=rul_sup04, colour="Support level of 4%")) + 
  geom_point(aes(y=rul_sup04,colour="Support level of 4%")) +
  # Lines and points for rules_sup03
  geom_line(aes(y=rul_sup03, colour="Support level of 3%")) +
  geom_point(aes(y=rul_sup03,colour="Support level of 3%")) + 
  # Polishing the graph
  theme_bw() + ylab("") +
  ggtitle("Number of extracted rules with apriori")


# Extract rules with the apriori
rules_items = apriori(df,
                       parameter = list(supp = 0.03,
                                        conf = 0.25,
                                        minlen = 2, 
                                        target = "rules"))

# Summary of extracted rules
summary(rules_items)
inspect((sort(rules_items,by="lift",decreasing=T)))

# Create redudant rules and filter from extracted rules
rules_red = is.redundant(rules_items)
rules.pruned = rules_items[!rules_red]
# Inspect the non-redundant rules with highest confidence
inspect((sort(rules.pruned, by="confidence")))

#VISUALIATIONS OF THE RULES
plot(rules_items,
     measure = c("confidence", "lift"),
     shading = "support",
     jitter = 1,
     engine = "html")

plot(rules_items, method = "matrix",
     shading ="confidence",
     engine = "html"
)


plot(rules_items, 
     method = "grouped",
     measure = "lift",
     shading = "confidence")

plot(rules_items, 
     method = "paracoord", 
     shading = "confidence")

plot(rules_items,
     method = "graph",
     engine = "htmlwidget")


top10_rules_items = head(sort(rules_items,
                              by = "confidence"), 10)

plot(top10_rules_items,
     method = "graph",engine = "htmlwidget")

inspectDT(rules_items)


