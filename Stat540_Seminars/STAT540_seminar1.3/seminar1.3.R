x <- 3 * 4
x
is.vector(x)
length(x)
x[2] <- 100
x
x[5] <- 3
x
x[11]
x[0]
x <- 1:4
(y <- x^2)
z <- vector(mode = mode(x), length = length(x))
for (i in seq_along(x)) {
  z[i] <- x[i]^2
}
identical(y, z)
set.seed(1999)
rnorm(5, mean = 10^(1:5))
round(rnorm(5, sd = 10^(0:4)), 2)
(y <- 1:3)
(z <- 3:7)
y + z
(y <- 1:10)
(z <- 3:7)
y + z
str(c("hello", "world"))
str(c(1:3, 100, 150))
(x <- c("cabbage", pi, TRUE, 4.3))
str(x)
length(x)
mode(x)
class(x)
n <- 8
set.seed(1)
(w <- round(rnorm(n), 2))  # numeric floating point
(x <- 1:n)  # numeric integer
(y <- LETTERS[1:n])  # character
(z <- runif(n) > 0.3)  # logical
str(w)
length(x)
is.logical(y)
as.numeric(z)
w
names(w) <- letters[seq_along(w)]
w
w < 0
which(w < 0)
w[w < 0]
seq(from = 1, to = length(w), by = 2)
w[seq(from = 1, to = length(w), by = 2)]
w[-c(2, 5)]
w[c("c", "a", "f")]
a.blarg <- c('cabbage', pi, TRUE, 4.3)
a.blarg
(a <- list("cabbage", pi, TRUE, 4.3))
str(a.blarg)
str(a)
length(a.blarg)
length(a)
mode(a.blarg)
mode(a)
class(a.blarg)
class(a)
names(a)
names(a) <- c("veg", "dessert", "myAim", "number")
a
a <- list(veg = "cabbage", dessert = pi, myAim = TRUE, number = 4.3)
names(a)
(a <- list(veg = c("cabbage", "eggplant"), tNum = c(pi, exp(1), sqrt(2)), myAim = TRUE, 
           joeNum = 2:6))
str(a)
length(a)
class(a)
mode(a)
a[[2]]  # index with a positive integer
a$myAim  # use dollar sign and element name
str(a$myAim)  # we get myAim itself, a length 1 logical vector
a[["tNum"]]  # index with length 1 character vector 
str(a[["tNum"]])  # we get tNum itself, a length 3 numeric vector
iWantThis <- "joeNum"  # indexing with length 1 character object
a[[iWantThis]]  # we get joeNum itself, a length 5 integer vector
a[[c("joeNum", "veg")]]  # does not work! can't get > 1 elements! see below
a[[2]]
mode(a[[2]])
mode(a[["tNum"]])
class(a[["tNum"]])
names(a)
a[c("tNum", "veg")]  # indexing by length 2 character vector
str(a[c("tNum", "veg")])  # returns list of length 2
a["veg"]  # indexing by length 1 character vector
str(a["veg"])  # returns list of length 1
length(a["veg"])  # really, it does!
length(a["veg"][[1]])  # contrast with length of the veg vector itself
#Creating a data.frame explicitly
n <- 8
n <- 8
set.seed(1)
(jDat <- data.frame(w = round(rnorm(n), 2),
                    x = 1:n,
                    y = I(LETTERS[1:n]),
                    z = runif(n) > 0.3,
                    v = rep(LETTERS[9:12], each = 2)))
str(jDat)
mode(jDat)
class(jDat)
is.list(jDat) # data.frames are lists
jDat[[5]] # this works but I prefer ...
jDat$v # using dollar sign and name, when possible
jDat[c("x", "z")] # get multiple variables
str(jDat[c("x", "z")]) # returns a data.frame
identical(subset(jDat, select = c(x, z)), jDat[c("x", "z")])
(qDat <- list(w = round(rnorm(n), 2),
              x = 1:(n-1), ## <-- LOOK HERE! I MADE THIS VECTOR SHORTER!
              y = I(LETTERS[1:n])))
as.data.frame(qDat) ## does not work! elements don't have same length!
qDat$x <- 1:n ## fix the short variable x
(qDat <- as.data.frame(qDat)) ## we're back in business
#Indexing Arrays, e.g. matrices
jMat <- outer(as.character(1:4), as.character(1:4), function(x, y) {
  paste0("x", x, y)
})
jMat
str(jMat)
class(jMat)
mode(jMat)
dim(jMat)
nrow(jMat)
ncol(jMat)
rownames(jMat)
rownames(jMat) <- paste0("row", seq_len(nrow(jMat)))
colnames(jMat) <- paste0("col", seq_len(ncol(jMat)))
dimnames(jMat)  # also useful for assignment
jMat
jMat[2, 3]
jMat[2, ]  # getting row 2
is.vector(jMat[2, ])  # we get row 2 as an atomic vector
jMat[, 3, drop = FALSE]  # getting column 3
dim(jMat[, 3, drop = FALSE])  # we get column 3 as a 4 x 1 matrix
jMat[c("row1", "row4"), c("col2", "col3")]
jMat[-c(2, 3), c(TRUE, TRUE, FALSE, FALSE)]  # wacky but possible
jMat[7]
jMat
jMat[1, grepl("[24]", colnames(jMat))]
jMat["row1", 2:3] <- c("HEY!", "THIS IS NUTS!")
jMat
#Creating arrays, eg matrices
##fill a matrix with a vector
matrix(1:15, nrow = 5)
matrix("yo!", nrow = 3, ncol = 6)
matrix(c("yo!", "foo?"), nrow = 3, ncol = 6)
matrix(1:15, nrow = 5, byrow = TRUE)
matrix(1:15, nrow = 5, dimnames = list(paste0("row", 1:5), paste0("col", 1:3)))
## Glueing vectors together
vec1 <- 5:1
vec2 <- 2^(1:5)
cbind(vec1, vec2)
rbind(vec1, vec2)
## creating from a data.frame
vecDat <- data.frame(vec1 = 5:1,
                     vec2 = 2^(1:5))
str(vecDat)
vecMat <- as.matrix(vecDat)
str(vecMat)
#this was an ex of silently convert everything to a character fail.
multiDat <- data.frame(vec1 = 5:1,
                       vec2 = paste0("hi", 1:5))
str(multiDat)
(multiMat <- as.matrix(multiDat))
str(multiMat)
#Implications for data.frames
jDat
jDat$z
iWantThis <- "z"
jDat[[iWantThis]]
str(jDat[[iWantThis]]) 
jDat["y"]
str(jDat["y"])  # we get a data.frame with one variable, y
iWantThis <- c("w", "v")
jDat[iWantThis]  # index with a vector of variable
str(jDat[c("w", "v")])
str(subset(jDat, select = c(w, v)))  # using subset() function
#matrix-style indexing demo
jDat[ , "v"]
str(jDat[ , "v"])
jDat[ , "v", drop = FALSE]
str(jDat[ , "v", drop = FALSE])
jDat[c(2, 4, 7), c(1, 4)] # awful and arbitrary but syntax works
jDat[jDat$z, ]
subset(jDat, subset = z)
