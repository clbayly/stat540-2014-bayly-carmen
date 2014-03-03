##LOAD PHOOREC DATA AND LATTICE PACKAGE##
setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)
##WRITE A FUNCTION TO PREPARE A MINISET...##

(luckyGenes <- c("1419655_at","1438815_at"))
##GET A DATA FRAME FROM THIS.
str(luckyGenes)
(luckyGene <- "1419655_at")
(unlist(prDat[luckyGene, ]))
##Ah OK. must get it to pick our data from from this.
pDat <- data.frame(prDes, gExp = unlist(prDat[luckyGene, ]))
str(pDat)
##Okeydoke. found it in:
#http://www.ugrad.stat.ubc.ca/~stat540/seminars/seminar04_compileNotebook-dataAggregation-twoGroupComparison.html

luckygenes <- c("1419655_at","1438815_at")
cDat <- subset(prDat, rownames(prDat) %in% luckygenes)
str(cDat) #good but I have to get the design matrix involved. 
#cDat <- suppressWarnings(data.frame(prDes, cDat)) #nope.
cDat <- data.frame(gExp = as.vector(t(as.matrix(miniDat))),
                      gene = factor(rep(rownames(miniDat), each = ncol(miniDat)),
                                    levels = keepGenes))

#Okay, wait.

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)
luckygenes <- c("1419655_at","1438815_at")

cDat <- subset(prDat, rownames(prDat) %in% luckygenes)
cDat <- data.frame(gExp = as.vector(t(as.matrix(cDat))),
                      gene = factor(rep(rownames(cDat), each = ncol(cDat)),
                                    levels = luckygenes))
cDat <- suppressWarnings(data.frame(prDes, cDat))
cDat <- data.frame(prDes, cDat)
str(cDat)

#good! now funciton-ify it.

bakeMyFrame <- function(x) {
  x <- data.frame(gExp = as.vector(t(as.matrix(x))),
                     gene = factor(rep(rownames(x), each = ncol(x)),
                                   levels = luckygenes))
  cDat <- suppressWarnings(data.frame(prDes, x))  
}

cDat <- bakeMyFrame(cDat)
str(cDat) #hmmm... wierd. try again.

##################

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
#str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
#str(prDes)
luckygenes <- c("1419655_at","1438815_at")

bakeMyFrame <- function(x) {
  x <- data.frame(gExp = as.vector(t(as.matrix(x))),
                  gene = factor(rep(rownames(x), each = ncol(x)),
                                levels = luckygenes))
  cDat <- suppressWarnings(data.frame(prDes, x))  
}

cDat <- bakeMyFrame(cDat)
str(cDat) #nope. there's something very wrong with gene expression.
But I think we did this earlier, I just have to find it...

#########

prDat <- read.table("GSE4051_data.tsv")
#str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
#str(prDes)
luckygenes <- c("1419655_at","1438815_at")
cDat <- data.frame(gExp = as.vector(t(as.matrix(cDat))))
str(cDat) #still have the 'factor w/160 levels' problem.

cDat <- data.frame(prDes, gExp = unlist(prDat[luckyGene, ]))
str(pDat)

cDat1 <- data.frame(prDes, gExp = unlist(prDat["1419655_at", ]))
str(pDat)

cDat2  <- data.frame(prDes, gExp = unlist(prDat["1438815_at", ]))
str(pDat)

identical(cDat, cDat1)

##############

prDat <- read.table("GSE4051_data.tsv")
#str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
#str(prDes)
luckygenes <- c("1419655_at","1438815_at")

bakeMyFrame <- function(x) {
  x <- data.frame(gExp = unlist(x),
                  gene = factor(rep(rownames(x), each = ncol(x)),
                                levels = luckygenes))
  x <- suppressWarnings(data.frame(prDes, x))  
}

cDat <- bakeMyFrame(cDat)

str(cDat)
################
#OK. in light of everything else not working, I will first make 
#a data frame like the mini, and then reshape.
#stupidcoding. 

prDat <- read.table("GSE4051_data.tsv")
#str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
#str(prDes)
luckygenes <- c("1419655_at","1438815_at")
#jDat <- prDat[getMe, ]
#str(jDat)
cDat <- prDat[luckygenes, ]
str(cDat)
#jDat <- data.frame(t(jDat))
#names(jDat) <- c("crabHammer", "eggBomb", "poisonFang")
#str(jDat)
cDat <- data.frame(t(cDat))
names(cDat) <- c("1419655_at","1438815_at")
str(cDat)
#identical(prDes$sidChar, rownames(jDat))
#identical(prDes$sidChar, rownames(cDat))
cmDat <- data.frame(prDes, cDat)
str(cmDat) #gives me odd gene titles - can't have a number as a 1st character. 
#nDat <-
#  with(kDat,
#       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
#                  probeset = factor(rep(c("eggBomb", "poisonFang"), each = nrow(kDat))),
#                  geneExp = c(eggBomb, poisonFang)))
#str(nDat)
csDat <-
  with(cmDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  probeset = factor(rep(c("X1419655_at", "X1438815_at"), each = nrow(cmDat))),
                  geneExp = c(X1419655_at, X1438815_at)))
str(csDat)
#well, that worked. 
#so, this shall become a function.

################

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)

(myGenes <- c("1419655_at","1438815_at"))

bakeMyFrame <- function(myGenes) {
  cDat <- prDat[myGenes, ]
  cDat <- data.frame(t(cDat))
  cmDat <- data.frame(prDes, cDat)
  identical(prDes$sidChar, rownames(cDat))
  csDat <-
    with(cmDat,
         data.frame(sidChar, sidNum, devStage, gType,
                    probeset = factor(rep(c(myGenes), each = nrow(cmDat))),
                    geneExp = myGenes))
}

bakeMyFrame(myGenes)
str(csDat)

####get variables in right order

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")
str(prDat)
str(prDes)
(myGenes <- c("1419655_at","1438815_at"))

bakeMyFrame <- function(myGenes) {
  cDat <- prDat[myGenes, ]
  cDat <- data.frame(t(cDat))
  cmDat <- data.frame(prDes, cDat)
  identical(prDes$sidChar, rownames(cDat))
  csDat <-
    with(cmDat,
         data.frame(sidChar, sidNum, devStage, gType,
                    geneExp = myGenes,
                    gene = factor(rep(c(myGenes), each = nrow(cmDat)))))
  return(csDat)
}

csDat <- bakeMyFrame(myGenes)
str(csDat)

################
setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)

(myGenes <- c("1419655_at","1438815_at"))

bakeMyFrame <- function(myGenes) {
  cDat <- prDat[myGenes, ]
  cDat <- data.frame(t(cDat))
  cmDat <- data.frame(prDes, cDat)
  identical(prDes$sidChar, rownames(cDat))
  csDat <-
    with(cmDat,
         data.frame(sidChar, sidNum, devStage, gType,
                    probeset = factor(rep(c(myGenes), each = nrow(cmDat))),
                    geneExp = myGenes))
}

bakeMyFrame(myGenes)
str(csDat)

#########
setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)
(myGenes <- c("1419655_at","1438815_at"))

cDat <- prDat[myGenes, ]
cDat <- data.frame(t(cDat))
identical(prDes$sidChar, rownames(cDat))
cmDat <- data.frame(prDes, cDat)
cbDat <-
  with(cmDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  gExp = c(X1419655_at, X1438815_at),
                  gene = factor(rep(c("X1419655_at", "X1438815_at"), each = nrow(cmDat)))))
str(cbDat) #thisworks

########### making it shorter for being funciton-amenable

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
#str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
#str(prDes)
myGenes <- c("1419655_at","1438815_at")

cmDat <- data.frame(prDes, data.frame(t(prDat[myGenes, ])))
cbDat <-
  with(cmDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  
                  gExp = c(X1419655_at, X1438815_at),
                  gene = factor(rep(c("X1419655_at", "X1438815_at"), each = nrow(cmDat)))))
str(cbDat) #thisworks

##########

myGenes
as.vector(myGenes)
list(as.vector(myGenes))
unlist(as.vector(myGenes))
unlist(myGenes)

#the question is, how to get this operation into a function:
setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")
myGenes <- c("1419655_at","1438815_at")


cmDat <- data.frame(prDes, data.frame(t(prDat[myGenes, ])))
cbDat <-
  with(cmDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  gExp = c(X1419655_at, X1438815_at),
                  gene = factor(rep(c("X1419655_at", "X1438815_at"), each = nrow(cmDat)))))
levels(cbDat$gene) <- myGenes #YAAAAYYYYEEEEEE!
str(cbDat)


names(cbDat$gene) <- c("1419655_at","1438815_at")
str(cbDat)
head(cbDat)
cbDat$gene
levels(cbDat$gene)
levels(cbDat$gene) <- myGenes #YAAAAYYYYEEEEEE!

OK, I get this. this is because rownames cannot start with a number
but Im guessing factor names can. 

So Imma have to try to make these factors before they ever become rows.
and recall naming command,
names(jDat) <- c("crabHammer", "eggBomb", "poisonFang")


##########

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)

bakeMyFrame <- function(mygenes, prDat, prDes) {
  cDat <- prDat[myGenes, ]
  cDat <- data.frame(t(prDat[myGenes]))
}
cDat <- prDat[myGenes, ]
cDat <- data.frame(t(cDat))
identical(prDes$sidChar, rownames(cDat))
cmDat <- data.frame(prDes, cDat)
cbDat <-
  with(cmDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  gExp = c(X1419655_at, X1438815_at),
                  gene = factor(rep(c("X1419655_at", "X1438815_at"), each = nrow(cmDat)))))
str(cbDat) #thisworks


####################OK. now we've fixed the factor naming issue.
setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")
myGenelist <- c("1419655_at","1438815_at")

#bakeMyFrame <- function(myGenelist)
cmDat <- data.frame(prDes, data.frame(t(prDat[myGenelist, ])))
str(cmDat)

cm2Dat  <- data.frame(gExp = c(t(prDat[myGenelist, ])),
                      gene = factor(rep(c(myGenelist))))
str(cm2Dat) #Good! now the issue is having alternating levels instead of all one than the other.

##########

cm2Dat  <- data.frame(gExp = c(t(prDat[myGenelist, ])),
                      gene = factor(unlist(myGenelist)))
str(cm2Dat)

##########
(unlist(myGenelist))


cbDat <-
  with(cmDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  gExp = c(X1419655_at, X1438815_at),
                  gene = factor(rep(c("X1419655_at", "X1438815_at"), each = nrow(cmDat)))))
levels(cbDat$gene) <- myGenelist #YAAAAYYYYEEEEEE!
str(cbDat)

#################

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")

myGenie  <- c("1419655_at","1438815_at")
#rightversion
nDat <-
  with(prDes,
       data.frame(sidChar, sidNum, devStage, gType,
                  probeset = factor(rep(myGenie, each = nrow(kDat))),
                  geneExp = c(t(prDat[myGenie, ]))))
str(nDat) #Yes! that one worked.

####################

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
str(prDat)
tail(prDat)
prDes <- readRDS("GSE4051_design.rds")

myGeneList  <- c("1419655_at","1438815_at")
#rightversion

nDat <- with(prDes,
             data.frame(sidChar, sidNum, devStage, gType,
                        gExp = c(t(prDat[myGeneList, ])),
                        gene = factor(rep(myGeneList, each = ncol(prDat)))))
str(nDat)

#################

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")
myGeneList  <- c("1419655_at","1438815_at")

bakeMyFrame  <- function(prDes, prDat, myGeneList){
  with(prDes,
       data.frame(sidChar, sidNum, devStage, gType,
                  gExp = c(t(prDat[myGeneList, ])),
                  gene = factor(rep(myGeneList, each = ncol(prDat)))))
}

cDat <- bakeMyFrame(prDes, prDat, myGeneList)
str(cDat)

#YAYYYYYYY
##WRITE A FUNCTION TO PREPARE A MINISET...## dealing with factoring sidchar problem

bakeMyFrame  <- function(prDes, prDat, myGeneList){
  with(prDes,
       data.frame(sidChar, sidNum, devStage, gType,
                  gExp = c(t(prDat[myGeneList, ])),
                  gene = factor(rep(myGeneList, each = ncol(prDat)))))
}

(myGeneList <- c("1419655_at","1438815_at"))
cDat <- bakeMyFrame(prDes, prDat, myGeneList)
str(cDat)
head(cDat)
tail(cDat)

###############Fixing the sidChar factor problem.
setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")

bakeMyFrame  <- function(prDes, prDat, myGeneList){
       data.frame(prDes,
                  gExp = c(t(prDat[myGeneList, ])),
                  gene = factor(rep(myGeneList, each = ncol(prDat))))
}

(myGeneList <- c("1419655_at","1438815_at"))
cDat <- bakeMyFrame(prDes, prDat, myGeneList)
str(cDat)
head(cDat)
tail(cDat)

#######NEW MISSION##########
##LOAD PHOOREC DATA AND LATTICE PACKAGE##

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)

##WRITE A FUNCTION TO PREPARE A MINISET...##STRIPPLOTING funct

bakeMyFrame  <- function(prDes, prDat, myGeneList){
  data.frame(prDes,
             gExp = c(t(prDat[myGeneList, ])),
             gene = factor(rep(myGeneList, each = ncol(prDat))))
}

##preparing my miniset

(myGeneList <- c("1419655_at","1438815_at"))
cDat <- bakeMyFrame(prDes, prDat, myGeneList)
str(cDat)
head(cDat)
tail(cDat)

##stripplot

stripplot(gExp ~ devStage | gene, cDat,
          group = gType, jitter.data = TRUE,
          auto.key = TRUE, type = c('p', 'a'), grid = TRUE) #not quite the same results, but similar. 

#making a stripploting function
#ingredients: 
setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")

myGeneList <- c("1419655_at","1438815_at")

bakeMyFrame  <- function(prDes, prDat, myGeneList){
  data.frame(prDes,
             gExp = c(t(prDat[myGeneList, ])),
             gene = factor(rep(myGeneList, each = ncol(prDat))))
}

cDat <- bakeMyFrame(prDes, prDat, myGeneList)

stripplot(gExp ~ devStage | gene, cDat,
          group = gType, jitter.data = TRUE,
          auto.key = TRUE, type = c('p', 'a'), grid = TRUE)

bakeData <- function(prDes, prDat, myGeneList)






