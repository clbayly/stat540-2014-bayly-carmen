
setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")
myGenelist <- c("1419655_at","1438815_at")



kDat <- readRDS("GSE4051_MINI.rds")
str(kDat)

myGenie  <- c("eggBomb", "poisonFang")
#rightversion
nDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(rep(c("eggBomb", "poisonFang"), each = nrow(kDat))),
                  geneExp = c(eggBomb, poisonFang)))
str(nDat)

#this one gives the annoying alternatings
nDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(rep(c("eggBomb", "poisonFang"))),
                  geneExp = c(eggBomb, poisonFang)))
str(nDat)

#this one is trying substitution

(myGenie <- c("eggBomb", "poisonFang"))
(myGenie2 <- (rep(c("eggBomb", "poisonFang"))))


nDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(rep(myGenie, each = nrow(prDes))),
                  geneExp = myGenie))
str(nDat)# AHHHHHHH

#############

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")
myGenelist <- c("1419655_at","1438815_at")

kDat <- readRDS("GSE4051_MINI.rds")
str(kDat)

myGenie  <- c("eggBomb", "poisonFang")
#rightversion
nDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(rep(myGenie, each = nrow(kDat))),
                  geneExp = c(eggBomb, poisonFang)))
str(nDat) #Yes! that one worked.

#################

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
prDes <- readRDS("GSE4051_design.rds")
myGenelist <- c("1419655_at","1438815_at")

kDat <- readRDS("GSE4051_MINI.rds")
str(kDat)

myGenie  <- c("eggBomb", "poisonFang")
#rightversion
nDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(rep(myGenie, each = nrow(kDat))),
                  geneExp = c(t(prDat[myGenie, ]))))
str(nDat) #Yes! that one worked.

#######






