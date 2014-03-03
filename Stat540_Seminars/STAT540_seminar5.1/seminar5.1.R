##LOAD PHOOREC DATA AND LATTICE PACKAGE##

setwd('/Users/carmenbayly/Stat540_Seminars/STAT540_seminar5.1/')
library(lattice) # if you don't already have this loaded ...
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)

##WRITE A FUNCTION TO PREPARE A MINISET...##

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
  #*skip for now # OK fine

bakeStripplot <- function(X){
  stripplot(gExp ~devStage | gene, x,
            group = gType, jitter.data = TRUE,
            auto.key = TRUE, type = c('p', 'a'), grid = TRUE)
}
bakeStripplot(cDat)

#checking out the t test question

(myGeneList <- c("1456341_a_at")
cDat <- bakeMyFrame(prDes, prDat, myGeneList)
bakeStripplot(cDat)

#from lect06 slide 16: t.test(gexp ~ gType, TheDat)
t.test(gExp ~ devStage | E16, cDat)
str(cDat)
?t.test()

##### need example solutions
####from https://github.com/abrarwafa/stat540-2014-wafa-abrar/blob/master/Seminar05/Seminar05.Rmd
(someDat <- subset(pDat, devStage == "P2"| devStage == "4_weeks" )) #notyourcode
t.test(gExp ~ devStage, someDat ) #notyourcode

#####from https://github.com/omaromeir/stat540-2014-alomeir-omar/blob/master/Seminar05/seminar05.R
t.test(gExp ~ devStage, subset(tDat, devStage == "P2" | devStage == "4_weeks" ))

##OK, I think this is pretty general stuff. Now for me:
t.test(gExp ~ devStage, subset(cDat, devStage == "P2" | devStage == "4_weeks" ))

#######next - 
lm(formula = gExp ~ devStage, data = cDat, subset = gType == "wt")
##     "wt")