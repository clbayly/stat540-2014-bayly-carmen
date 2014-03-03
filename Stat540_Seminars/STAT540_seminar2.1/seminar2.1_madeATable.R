#matrix construction
B <- 1000
#'''#with rnorm
name  <- rnorm
n <- 10
x10 <- matrix(rnorm(B * n), nrow = B, ncol = n)
n <- 100
x100 <- matrix(rnorm(B * n), nrow = B, ncol = n)
n <- 1000
x1000 <- matrix(rnorm(B * n), nrow = B, ncol = n)
n <- 10000
x10000 <- matrix(rnorm(B * n), nrow = B, ncol = n)
#'''
'''
#with dbeta
name  <- dbeta
n <- 10
x10 <- matrix(dbeta(B * n), nrow = B, ncol = n)
n <- 100
x100 <- matrix(dbeta(B * n), nrow = B, ncol = n)
n <- 1000
x1000 <- matrix(dbeta(B * n), nrow = B, ncol = n)
n <- 10000
x10000 <- matrix(dbeta(B * n), nrow = B, ncol = n)
'''
'''
#with dhyper
name  <- dhyper
n <- 10
x10 <- matrix(dhyper(B * n), nrow = B, ncol = n)
n <- 100
x100 <- matrix(dhyper(B * n), nrow = B, ncol = n)
n <- 1000
x1000 <- matrix(dhyper(B * n), nrow = B, ncol = n)
n <- 10000
x10000 <- matrix(dhyper(B * n), nrow = B, ncol = n)
'''
#sample size
n10 <- ncol(x10)
n100 <- ncol(x100)
n1000 <- ncol(x1000)
n10000 <- ncol(x10000)
#Sample means
xbar10 <- rowMeans(x10)
xbar100 <- rowMeans(x100)
xbar1000 <- rowMeans(x1000)
xbar10000 <- rowMeans(x10000)
#trueSEM
tSEM10 <- sd(x10/sqrt(nrow(x10)))
tSEM100 <- sd(x100/sqrt(nrow(x100)))
tSEM1000 <- sd(x1000/sqrt(nrow(x1000)))
tSEM10000 <- sd(x10000/sqrt(nrow(x10000)))
#obsSEM
oSEM10 <- sd(rowMeans(x10))
oSEM100 <- sd(rowMeans(x100))
oSEM1000 <- sd(rowMeans(x1000))
oSEM10000 <- sd(rowMeans(x10000))
#sampleMeanIQR
IQR10 <- IQR(rowMeans(x10))
IQR100 <- IQR(rowMeans(x100))
IQR1000 <- IQR(rowMeans(x1000))
IQR10000 <- IQR(rowMeans(x10000))
#sampleMeanMad
mad10 <- mad(rowMeans(x10))
mad100 <- mad(rowMeans(x100))
mad1000 <- mad(rowMeans(x1000))
mad10000 <- mad(rowMeans(x10000))
#make table
rownames  <-rbind(n10, n100, n1000, n10000)
trueSEM <- rbind(tSEM10, tSEM100, tSEM1000, tSEM10000)
obsSEM <- rbind(oSEM10, oSEM100, oSEM1000, oSEM10000)
IQR  <- rbind(IQR10, IQR100, IQR1000, IQR10000)
mad <- rbind(mad10, mad100, mad1000, mad10000)
bigtable <- cbind(rownames, trueSEM, obsSEM, IQR, mad)
xname <- c("sampleSize", "trueSEM", "obsSEM", "sampleMeanIQR", "sampleMeanMad")
colnames(bigtable)  <- xname
name
bigtable
