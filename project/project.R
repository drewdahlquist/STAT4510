library(dplyr)

# data
beans = read.csv("/Users/drew/Desktop/OneDrive - University of Missouri/sp 22/STAT 4510/project/beans.csv")
beans$Class = as.factor(beans$Class)

# train/testing split
set.seed(1)
train = sample(1:nrow(beans), 0.7*nrow(beans))
beans.train = beans[train,]
beans.test = beans[-train,]

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
gbm.beans.1 = gbm(Class ~ ., data=beans[train,], interaction.depth=3,n.trees=100)
gbm.beans.2 = gbm(Class ~ ., data=beans[train,], interaction.depth=3,n.trees=1000)

pred.gbm.1 = predict.gbm(gbm.beans.1, newdata=beans.test, type="response")
pred.gbm.2 = predict.gbm(gbm.beans.2, newdata=beans.test, type="response")

pred.gbm.1 = colnames(pred.gbm.1)[apply(pred.gbm.1, 1, which.max)]
pred.gbm.2 = colnames(pred.gbm.2)[apply(pred.gbm.2, 1, which.max)]

table.1 = table(beans.test$Class, pred.gbm.1)
table.2 = table(beans.test$Class, pred.gbm.2)

table.1
table.2

# random forest


# one-v-one svm
