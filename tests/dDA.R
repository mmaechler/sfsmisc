library(sfsmisc)

set.seed(234)

## The same example as  knn() & knn1() from "class" :
data(iris3)
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- rep(1:3, each = 25)

diagDA(train, cl, test)
table(diagDA(train, cl, test), cl)## 0 + 1 + 2 misclassified
## knn (    k=1) has 0 + 1 + 3
## knn ( *, k=3) has 0 + 2 + 3

## ==> diagDA() is best!
