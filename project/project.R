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


# random forest


# one-v-one svm
