library(sfsmisc)

###--------------- "Iris Example for ever" ----------------------------
data(iris)
cl.true <- as.integer(iris[,"Species"])
n <- length(cl.true)
stopifnot(cl.true == rep(1:3, each = 50))
m.iris <- data.matrix(iris[, 1:4])

.proctime00 <- proc.time()

## Self Prediction:  Not too good (2+4 and 3+3 misclass.)
table(diagDA(m.iris, cl.true, m.iris),             cl.true)
table(diagDA(m.iris, cl.true, m.iris, pool=FALSE), cl.true)

## Crossvalidation:  The same example as  knn() & knn1() from "class" :
data(iris3)
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- rep(1:3, each = 25)

pcl <- diagDA(train, cl, test)
table(pcl, cl)## 0 + 1 + 2 misclassified
## knn (    k=1) has 0 + 1 + 3
## knn ( *, k=3) has 0 + 2 + 3   ==> ``diagDA() is best ..''

stopifnot(pcl == diagDA(train,cl, test, pool = FALSE))
                                        # i.e. quadratic identical here

## Now do some (randomized) CV :
## for each observation, count how often it's misclassified
M <- 200
set.seed(234)
missCl <- integer(n)
for(m in 1:M) {
    itr <- sample(n, 0.9 * n)
    lrn <- m.iris[ itr,]
    tst <- m.iris[-itr,]
    pcl <- diagDA(lrn, cl.true[itr],  tst)
    stopifnot(pcl == predict(dDA(lrn, cl.true[itr]),  tst))
    missCl <- missCl + as.integer(pcl != cl.true[ - itr])
}
missCl ; mean(missCl) / M

missCl <- integer(n)
for(m in 1:M) {
    itr <- sample(n, 0.9 * n)
    lrn <- m.iris[ itr,]
    tst <- m.iris[-itr,]
    pcl <- diagDA(lrn, cl.true[itr],  tst, pool=FALSE)
    stopifnot(pcl == predict(dDA(lrn, cl.true[itr], pool=FALSE),  tst))
    missCl <- missCl + as.integer(pcl != cl.true[ - itr])
}
missCl ; mean(missCl) / M ## here somewhat worse than linear

cat('Time elapsed: ', proc.time() - .proctime00,'\n')

