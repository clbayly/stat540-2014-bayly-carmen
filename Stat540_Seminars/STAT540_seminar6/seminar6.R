Seminar6

setwd("/Users/carmenbayly/Stat540/Stat540_Seminars/STAT540_seminar5.1")
#setwd("/Users/carmenbayly/Stat540/Stat540_git/practicing/Assignment1")
#source("http://www.bioconductor.org/biocLite.R")
#biocLite("limma")
#biocLite("statmod")
library(limma)
library(lattice)
library(ggplot2)

prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)

#difficulty est genewise war
m <- 1000
n <- 3
x <- matrix(rnorm(m * n), nrow = m)

obsVars <- apply(x, 1, var)
summary(obsVars)
mean(obsVars < 1/3)
densityplot(~obsVars, n = 200)

#Fit a linear model: explain gene expression in the wild type mice as a function of developmental stage (one-way ANOVA)

wtDes <- subset(prDes, gType == "wt")
str(wtDes)

wtDat <- subset(prDat, select = prDes$gType == "wt")
str(wtDat, max.level = 0)

wtDesMat <- model.matrix(~devStage, wtDes)
str(wtDesMat)

wtFit <- lmFit(wtDat, wtDesMat)
wtEbFit <- eBayes(wtFit)

str(wtDat)
str(wtFit)
str(wtDesMat)

topTable(wtEbFit)
topTable(wtEbFit, coef = 2:5)  # cryptic! error-prone!

colnames(coef(wtEbFit))  # remind yourself of the coef names
(dsHits <- topTable(wtEbFit, coef = grep("devStage", colnames(coef(wtEbFit)))))

#the rest is graphing the results...
#***** own code here for returning rows with F below a certain numeber**
#*****own code: pick out a hit by rank (ie 63rd)
#****own: creating scatter (she used kernsmooth) for the t-stat
#******and density plot of the  B-H p-values (1 column of TopTable)

#PERMORM INFERENCE ON SOME CONTRASTS
colnames(wtDesMat)
(cont.matrix <- makeContrasts(P10VsP6 = devStageP10 - devStageP6, fourweeksVsP10 = devStage4_weeks - 
                                devStageP10, levels = wtDesMat))
wtFitCont <- contrasts.fit(wtFit, cont.matrix)
wtEbFitCont <- eBayes(wtFitCont)
topTable(wtEbFitCont)

#plotdata of top 4 hits
cutoff <- 1e-04
wtResCont <- decideTests(wtEbFitCont, p.value = cutoff, method = "global")
summary(wtResCont)

(hits1 <- rownames(prDat)[which(wtResCont[, "P10VsP6"] < 0)])
#stripplotIt(prepareData(hits1)) ****Need your own!
(hits2 <- rownames(prDat)[which(wtResCont[, "fourweeksVsP10"] < 0)])
#4 of 36 that increase p10 to 4 weeks
(hits3 <- rownames(prDat)[which(wtResCont[, "fourweeksVsP10"] > 0)])

intersect(hits1, hits3)
#nohits dissappointing try again

cutoff <- 0.01
nHits <- 8
wtResCont <- decideTests(wtEbFitCont, p.value = cutoff, method = "global")
summary(wtResCont)
hits1 <- rownames(prDat)[which(wtResCont[, "P10VsP6"] < 0)]
#stripplotIt(prepareData(hits1[1:nHits]))
hits2 <- rownames(prDat)[which(wtResCont[, "fourweeksVsP10"] < 0)]
#stripplotIt(prepareData(hits2[1:nHits]))
hits3 <- rownames(prDat)[which(wtResCont[, "P10VsP6"] > 0)]
#stripplotIt(prepareData(hits3[1:nHits]))
hits4 <- rownames(prDat)[which(wtResCont[, "fourweeksVsP10"] > 0)]
#stripplotIt(prepareData(hits4[1:nHits]))

vennDiagram(wtResCont)

#----and that's all to seminar 6, apparently the last seminar you need.

#you need to make a clone of this for your own data. 
#make your cumbersome functions into a prepareData function, 
#but then use all this...





