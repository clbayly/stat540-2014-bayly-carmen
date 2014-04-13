#Loading packages and data-----
library(lattice) # if you don't already have this loaded ...
library(ggplot2) # we'll make figures with both
library(reshape2) # for the function melt
library(data.table) #for renaming my function

prDat <- read.table("/Users/carmenbayly/Stat540/Stat540_Seminars/Seminars_at_once/GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("/Users/carmenbayly/Stat540/Stat540_Seminars/Seminars_at_once/GSE4051_design.rds")
str(prDes)

#for notes, from dataAggregation-----
#notmine-notmine-notmine-notmine-notmine-notmine-notmine-notmine-
'''
jFun <- function(x) {
  estCoefs <- coef(lm(lifeExp ~ I(year - yearMin), x))
  names(estCoefs) <- c("intercept", "slope")
  return(estCoefs)
}
jFun(jDat)  

jFun(subset(gDat, country == "Canada"))
jFun(subset(gDat, country == "Uruguay"))
'''
#notmine-notmine-notmine-notmine-notmine-notmine-notmine-notmine-

#Write prepareData functions--------
#Standard data.frame: example: MyFrame <- prepareSData(c("1419655_at","1438815_at"))
prepareSData <- function(x) {
  gDat <- prDat[x, ]
  identical(prDes$sidChar, names(gDat))
  sDat <- data.frame(prDes,t(gDat))
}

#TallNonMelt data.frame: MyFrame <- prepareTData(c("1419655_at","1438815_at"))
prepareTData <- function(x) {
  gDat <- prDat[x, ]
  stDat <- with(prDes,
                data.frame(sidChar, sidNum, devStage, gType,
                           gExp = unlist(gDat),
                           gene = rownames(gDat)))
  out <- stDat[c("sidChar","sidNum","devStage", "gType", "gExp","gene")]
}

#Tall Melt Function: MyFrame <- prepareMData(c("1419655_at","1438815_at"))
library(data.table)
str(prDes)
prepareMData <- function(x) {
  gDat <- prDat[x, ]
  sDat <- cbind(prDes,t(gDat))
  out <- melt(sDat, id.vars=colnames(prDes))[c(1:4,6,5)]
  setnames(out, old = c('variable','value'), new = c('gene','gExp'))
}

#Write makeStripplot functions--------

#Make Lattice stripplots
makeLstripplot <- function(x){
  stripplot(gExp ~ devStage | gene, jDat,
            group = gType, jitter.data = TRUE,
            auto.key = TRUE, type = c('p', 'a'), grid = TRUE)
}

#Make GGplot stripplots
makeGGstripplot <- function(x) {
  (p <- ggplot(x, aes(x = devStage, y = gExp, color = gType, group = gType)) + 
     geom_point(position = position_jitter(width = 0.01)) + 
     facet_wrap(~ gene) +
     geom_line(stat = "summary", fun.y = mean)) 
}

# Continuing Seminar 5-------------
(luckyGenes <- c("1419655_at","1438815_at"))
jDat <- prepareTData(luckyGenes)
str(jDat)

makeGGstripplot(jDat)
makeGGstripplot(newDat <- prepareTData("1456341_a_at"))

t.test(gExp ~ gType, newDat,
       subset = gType %in% c("wt", "NrlKO"),
       var.equal = TRUE) #**this will take learning**

#ex2t.test
t.test(gExp ~ devStage, newDat,
       subset = devStage %in% c("P2", "4_weeks"),
       var.equal = TRUE) #**this will take learning**

#linear modeling
makeGGstripplot(mDat <- prepareTData("1438786_a_at"))
mFit <- lm(gExp ~ devStage, mDat, subset = gType == "wt")
summary(mFit)
coef(mFit)

#exploring...
sample(rownames(prDat),1)
makeGGstripplot(mDat <- prepareTData(sample(rownames(prDat),1)))
mFit <- lm(gExp ~ devStage, mDat, subset = gType == "NrlKO") 
#note: estimate is the estimate of the mean of the distribution yielding the given datapoints.
#st. error is basically a measure of variance that involved diagonals of covariant matricies.
#t=(est./st.error), and p = the prb of that being 0.

#Perform Inference for a contrast------
coef(mFit) #recall: coef = estimated mean for each group. #QQQ ever different from "simple mean"
(contMat <- matrix(c(0, 1, 0, -1, 0), nrow = 1)) #This basically subtracts one mean from another
(obsDiff <- contMat %*% coef(mFit)) #and this outputs the difference between 2 means. 

(sampMeans <- aggregate(gExp ~ devStage, mDat, FUN = mean,
                        subset = gType == "wt"))
with(sampMeans, gExp[devStage == "P2"] - gExp[devStage == "P10"])

vcov(mFit) #takes standard errors in mFit, sets on both axis, computes products. 
#Shouldn't they be div'd by each's own variance, though? otherwise, tiny variance*large variance 
#could still be big.
summary(mFit)$coefficients[ , "Std. Error"]
sqrt(diag(vcov(mFit)))

#Found useful tapply ex in lacture 8 slide 13
with(jDat, round(tapply(gExp, list(gType, devStage), mean),2))

#Now to get the est. std error for our contrast-------
#vcov() and is equal to (XTX)−1σ^2.
vcov(mFit)
summary(mFit)$coefficients[ , "Std. Error"]
sqrt(diag(vcov(mFit)))
summary(mFit)$coefficients

#the variance-covariance matrix of a contrast obtained as: Cα^ is C(xTx)−1CTσ^2.
# C means.... the contrast matrix? is contrast different from contrast matrix?

(estSe <- contMat %*% vcov(mFit) %*% t(contMat)) #estimated standard error
            #I see C(xTx). what about other -1*CTσ^2. QQQ???

(testStat <- obsDiff/estSe)
2 * pt(abs(testStat), df = df.residual(mFit), lower.tail = FALSE) #pt gives distribution function.
# QQQ why called a dist function when it's really like a t-table?

#so close... if I want to table plain and simple all devStage values 
#at once for diff gType factor levels, though? QQQ how would I do this?

