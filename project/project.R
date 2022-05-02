library(dplyr)

# data
#beans = read.csv("/Users/drew/Desktop/OneDrive - University of Missouri/sp 22/STAT 4510/project/beans.csv")
beans = read.csv("D:/Docs/Homework/STAT4510/project/beans.csv")
beans$Class = as.factor(beans$Class)

# train/testing split
set.seed(1)
train = sample(1:nrow(beans), 0.7*nrow(beans))
beans.train = beans[train,]
beans.test = beans[-train,]
testPlot = beans[1:10000,1:2]
plot(testPlot)
# tree's
library(tree)
tree.fit = tree(Class ~ ., beans, subset=train)
plot(cv.tree(tree.fit), type="b")

plot(tree.fit)
text(tree.fit, pretty=0)

pred = predict(tree.fit, newdata = beans[-train,], type="class")
table = table(beans.test$Class, pred)
table

# boosting
library(gbm)

set.seed(1)

# 3 boosted models with differing number of trees
#number of leaves = interaction depth + 1
gbm.beans.1 = gbm(Class ~ ., data=beans[train,], interaction.depth=3,n.trees=100)
gbm.beans.2 = gbm(Class ~ ., data=beans[train,], interaction.depth=3,n.trees=1000)
gbm.beans.3 = gbm(Class ~ ., data=beans[train,], interaction.depth=16,n.trees=500)

#pretty print trees to see nodes
pretty.gbm.tree(gbm.beans.1)
pretty.gbm.tree(gbm.beans.2)
pretty.gbm.tree(gbm.beans.3)

#pred and make confusion matrix for each tree
pred.gbm.1 = predict.gbm(gbm.beans.1, newdata=beans.test, type="response")
pred.gbm.2 = predict.gbm(gbm.beans.2, newdata=beans.test, type="response")
pred.gbm.3 = predict.gbm(gbm.beans.3, newdata=beans.test, type="response")

pred.gbm.1 = colnames(pred.gbm.1)[apply(pred.gbm.1, 1, which.max)]
pred.gbm.2 = colnames(pred.gbm.2)[apply(pred.gbm.2, 1, which.max)]
pred.gbm.3 = colnames(pred.gbm.3)[apply(pred.gbm.3, 1, which.max)]

table.1 = table(beans.test$Class, pred.gbm.1)
table.2 = table(beans.test$Class, pred.gbm.2)
table.3 = table(beans.test$Class, pred.gbm.3)

table.1
table.2
table.3

# random forest


# one-v-one svm
library(e1071)
?svm
svmBeans1 = svm(Class~., data=beans[train,], kernel="linear", cost=.1, type = "C")
svmBeans2 = svm(Class~., data=beans[train,], kernel="polynomial", cost=.1, type = "C")
svmBeans3 = svm(Class~., data=beans[train,], kernel="radial", cost=.1, type = "C")
svmBeans4 = svm(Class~., data=beans[train,], kernel="sigmoid", cost=10, type = "C")


predSVMBeans1 = predict(svmBeans1, newdata=beans.test)
predSVMBeans2 = predict(svmBeans2, newdata=beans.test)
predSVMBeans3 = predict(svmBeans3, newdata=beans.test)
predSVMBeans4 = predict(svmBeans4, newdata=beans.test)

tableSVM1 = table(beans.test$Class, predSVMBeans1)
tableSVM2 = table(beans.test$Class, predSVMBeans2)
tableSVM3 = table(beans.test$Class, predSVMBeans3)
tableSVM4 = table(beans.test$Class, predSVMBeans4)

tableSVM1
tableSVM2
tableSVM3
tableSVM4


