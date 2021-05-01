library(readxl)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(data.table)
library(ROCR)
library(psych)
library(ineq)
library(StatMeasures)
library(htmlwidgets)
library(DataExplorer)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(ellipse)
library(RColorBrewer)
library(partykit)
library(CHAID)
library(dplyr)
library(purrr)
library(randomForest)
library(InformationValue)
library(data.table)

setwd("C:/Users/Samrat/Documents/R/Directories/")
getwd()
data = read_excel("Thera Bank_Personal_Loan_Modelling-dataset-1.xlsx")
head(data)

names(data)
data = data[,c(-1,-5)]
summary(data)
str(data)
attach(data)

table(data$Income.in.K.month)

colnames(data) = make.names(colnames(data))

data[is.na(data)] = 0 
sum(is.na(data))

data$Experience.in.years = ifelse(data$Experience.in.years < 0, 0, data$Experience.in.years)


plot(data$Mortgage,data$Age.in.years)

mortgage_data = subset(data, Mortgage > 0)
hist(mortgage_data$Mortgage, density = 20, col = "black")
boxplot(mortgage_data$Mortgage, col = "red")


plot(data$Education)
plot(data$Education~data$Age.in.years)

plot(data$Experience.in.years~data$Age.in.years)

plot(data$CCAvg~data$Income.in.Kmonth)


boxplot(data[,c(-1,-2,-3,-7)], col = "green")
boxplot(data[,c(1,2,3)], col = "blue")

sum(is.na(data))

corr.matrix = round(cor(data),3)
plot_correlation(data)
ggcorrplot(corr.matrix, type = "lower", ggtheme = ggplot2::theme_gray,
           show.legend = TRUE, show.diag = TRUE, colors = c("cyan","white","sky blue"),
           lab = TRUE)

my_colors = brewer.pal(7, "Blues")
my_colors = colorRampPalette(my_colors)(100)
plotcorr(corr.matrix , col=my_colors[corr.matrix*50+50] , mar=c(1,1,1,1), )



dist.matrix = dist(data[,c(1,2,3,4,5,7)], method = "euclidean", upper = FALSE)
print(dist.matrix)

dist.matrix2 = dist(data[,c(6,8,9,10,11,12)], method = "minkowski", p = 2)
print(dist.matrix2)

apply(data,2,mean)
apply(data,2,sd)

data.scaled  = scale(data)
print(data.scaled)

apply(data.scaled,2,mean)
apply(data.scaled,2,sd)

dist.matrix.scaled = dist(data.scaled, method = "euclidean")
print(dist.matrix.scaled)


cluster = hclust(dist.matrix.scaled, method = "average")
plot(cluster)

rect.hclust(cluster, k = 4, border = "blue")

data$Cluster = cutree(cluster, k = 4)
print(data)

Profile = aggregate(data[,-13], list(data$Cluster), FUN = "mean")
print(Profile)




#CART
data = data[,-13]

sum(data$Personal.Loan == 1)/nrow(data)

set.seed(100)

split = sample.split(data$Personal.Loan, SplitRatio = 0.7)
#we are splitting the data such that we have 70% of the data is Train Data and 30% of the data is my Test Data

train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

plot(train$Mortgage,train$Income.in.K.month, xlab = "Mortagage" , ylab = "Income")
points(train$Income.in.K.month[data$Personal.Loan == 1], train$Mortgage[data$Personal.Loan == 1], col = "blue", pch = 19)
points(train$Income.in.K.month[data$Personal.Loan == 0], train$Mortgage[data$Personal.Loan == 0], col = "red", pch = 19)


tree = rpart(formula = Personal.Loan ~ ., data = train, method = "class", minbucket = 5, cp = 0)
tree


rpart.plot(tree)
printcp(tree)
plotcp(tree)


ptree = prune(tree, cp = 0.025, "CP")

printcp(ptree)
rpart.plot(ptree)
ptree


train$Prediction = predict(ptree, data = train, type = "class")
train$Score = predict(ptree, data = train, type = "prob")

test$Prediction = predict(ptree, data = test, type = "class")
test$Score = predict(ptree, data = test, type = "prob")

train$Education = as.factor(train$Education)
train$Personal.Loan = as.factor(train$Personal.Loan)
train$Securities.Account = as.factor(train$Securities.Account)
train$CD.Account = as.factor(train$CD.Account)
train$Online = as.factor(train$Online)
train$Credit.Card = as.factor(train$Credit.Card)

summary(train)

seed = 1000
set.seed(seed)

r.Forest = randomForest(Personal.Loan ~ .,data = train[,c(1,2,3,4,5,6,7,9,10,11,12)], ntree = 501, mtry = 5, nodesize = 10, importance = TRUE)
print(r.Forest)

importance(r.Forest)

plot(r.Forest)

set.seed(seed)


t.r.Forest = tuneRF(x=train[,c(1,2,3,4,5,6,7,9,10,11,12)], y = train$Personal.Loan, mtryStart = 3, stepFactor = 1.5, ntreeTry = 51, improve = 0.0001, 
                    trace = TRUE, plot = TRUE, doBest = TRUE, importance = TRUE)

train$Predict.Class = predict(t.r.Forest, train, type = "class")
train$Prob1 = predict(t.r.Forest, train, type = "prob")[,"1"]
head(train)


tabular = table(train$Personal.Loan,train$Predict.Class)
print(tabular)
print((tabular[1,2]+tabular[2,1])/3500)

qs = quantile(train$Prob1, probs = seq(0,1,length = 11))
print(qs)


threshold = qs[10]
mean(train$Personal.Loan[train$Prob1 > threshold] == "1")

test$Predict.Class = predict(t.r.Forest, test, type = "class")
test$Prob1 = predict(t.r.Forest, test, type = "prob")[,"1"]
head(test)

tabular1 = table(test$Personal.Loan,test$Predict.Class)
print(tabular1)
print((tabular1[1,2]+tabular1[2,1])/1500)
mean(test$Personal.Loan[test$Prob1 > threshold] == "1")


train$deciles = cut(train$Prob1, unique(qs), include.lowest = TRUE)
trainDT = data.table(train)
rank.Table = trainDT[, list(cnt = length(Personal.Loan),
                            cnt_trgt1 = sum(Personal.Loan == 1),
                            cnt_trgt0 = sum(Personal.Loan == 0)
                            ), by = deciles][order(-deciles)]


rank.Table$rrate = round(rank.Table$cnt_trgt1/rank.Table$cnt,4)*100
rank.Table$c_resp_rate = cumsum(rank.Table$cnt_trgt1)
rank.Table$c_nonresp_rate = cumsum(rank.Table$cnt_trgt0)


rank.Table$c_rel_resp = round(rank.Table$c_resp_rate/sum(rank.Table$cnt_trgt1),4)*100
rank.Table$c_rel_nonresp = round(rank.Table$c_nonresp_rate/sum(rank.Table$cnt_trgt0),4)*100

rank.Table$ks = abs(rank.Table$c_rel_resp - rank.Table$c_rel_nonresp)

print(rank.Table)



pred_obj = prediction(train$Prob1, train$Personal.Loan)

perf = performance(pred_obj, "tpr","fpr")
plot(perf)


auc = performance(pred_obj, "auc")
auc = as.numeric(auc@y.values)
print(auc)


gini = ineq(train$Prob1, "gini")
print(gini)


Concordance(actuals = train$Personal.Loan, predictedScores = train$Prob1)




pred_obj1 = prediction(test$Prob1, test$Personal.Loan)

perf1 = performance(pred_obj1, "tpr","fpr")
plot(perf1)


auc1 = performance(pred_obj1, "auc")
auc1 = as.numeric(auc1@y.values)
print(auc1)


gini1 = ineq(test$Prob1, "gini")
print(gini1)


Concordance(actuals = test$Personal.Loan, predictedScores = test$Prob1)