# Data Mining
# In-class exercise: Association Analysis
# Cameron Ervin

# 1. Install and load packages
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

# 2. Load the dataset Groceries, and save the dataset as myData
data("Groceries")
myData <- Groceries

# 3. Print first 5 transactions using the function inspect()
inspect(myData)

# 4. Use summary() to answer the following questions
summary(myData)
# total transactions = 9835
# number items sold = 43367
# item sold the most = whole milk (2513)
# number transactions with one item = 2159
# average ot items sold in transactions = 4.409

# 5. Print names of all items
colnames(myData)

# 6. Plot a bar chart to show top ten most frequent items
itemFrequencyPlot(myData, topN = 10)

# 7. Generate association rules using S=0.01, C=0.3, max length=3
rules <- apriori(myData, parameter = list(support = 0.01, confidence = 0.3, maxlen = 3))
summary(rules)
# there are 125 rules generated
sorted_rules <- sort(rules, by = "support")
inspect(head(sorted_rules,10))

# 8. Repeat with different threshold
rules2 <- apriori(myData, parameter = list(support = 0.01, confidence = 0.1), appearance = list(default = "lhs", rhs = "rolls/buns"))
summary(rules2)
# there are 36 rules generated
sorted_rules2 <- sort(rules, by = "confidence")
inspect(head(sorted_rules,10))
# the rule with the highest confidence is [1]  {other vegetables} => {whole milk}       0.07483477 0.3867578 
# lift of this rule is 1.513634

# 9. Plot summary of generated rules from previous problem with plot()
plot(rules2)

# 10. Visualize the rules
plot(rules2, method = "grouped")

# 11. Convert generated rules into an interactive chart
inspectDT(sorted_rules2)
