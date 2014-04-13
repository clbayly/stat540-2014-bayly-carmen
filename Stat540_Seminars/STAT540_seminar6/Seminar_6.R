#Load data---------------
library(limma)
library(lattice)
library(ggplot2)

prDat <- read.table("/Users/carmenbayly/Stat540/Stat540_Seminars/Seminars_at_once/GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("/Users/carmenbayly/Stat540/Stat540_Seminars/Seminars_at_once/GSE4051_design.rds")
str(prDes)

#Write MOD prepareData functions--------
#Standard data.frame: example: MyFrame <- prepareSData(c("1419655_at","1438815_at"))
prepareSData <- function(x) {
  gDat <- prDat[x, ]
  identical(prDes$sidChar, names(gDat))
  sDat <- data.frame(prDes,t(gDat))
}
'''
#MODTallNonMelt data.frame: MyFrame <- prepareTData(c("1419655_at","1438815_at"))
REPAIRS REQUIRED
prepareTData <- function(x) {
  gDat <- prDat[x, ]
  stDat <- with(prDes,
                data.frame(sidChar, sidNum, devStage, gType,
                           gExp = unlist(as.data.frame(t(gDat)), use.names = FALSE),
                           gene = rep(c(rownames(gDat)))
  #out <- stDat[c("sidChar","sidNum","devStage", "gType", "gExp","gene")]
  #return(out)
}

TheProblems: 
#Unlist seems to unravel  matrix in the wrong order, 
#other levels in the df are not doing nrow adjacent repeats before the next level comes
#it seems there are rules govorning whether my function will relase me my matrix!
'''
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
     facet_wrap(~ gene, nrow = 3) +
     geom_line(stat = "summary", fun.y = mean)) 
}

#Practice with small sample variance-------------
m <- 1000
n <- 3
x <- matrix(rnorm(m * n), nrow = m)

obsVars <- apply(x, 1, var)
summary(obsVars)
mean(obsVars < 1/3)
densityplot(~obsVars, n = 200)

#Fit a linear model (setup) -----------
wtDes <- subset(prDes, gType == "wt") #QQQ my results sytematically undershoot those on the page.
str(wtDes)
wtDat <- subset(prDat, select = prDes$gType == "wt")
str(wtDat, max.level = 0)
head(wtDat)
wtDesMat <- model.matrix(~devStage, wtDes)
str(wtDesMat)

#Fit The Model (one-way ANOVA)------------------
#echo=FALSE
wtFit <- lmFit(wtDat, wtDesMat)
wtEbFit <- eBayes(wtFit)
topTable(wtEbFit)
colnames(coef(wtEbFit))  # remind yourself of the coef names
(dsHits <- topTable(wtEbFit, coef = grep("devStage", colnames(coef(wtEbFit)))))

#echo=FALSE 1-liner
topHits <- c("1451617_at", "1425222_x_at", "1422929_s_at")
hitDat <- prepareMData(topHits)
makeGGstripplot(hitDat <- prepareMData(topHits)) 

#Be the boss of Toptable()----------------------
#echo=FALSE
cutoff <- 1e-05
dsHits <- topTable(wtEbFit,
                   coef = grep("devStage", colnames(coef(wtEbFit))),
                   p.value = cutoff, n = Inf)
cat((numbHits <- nrow(dsHits)), "probes")
dsHits[63, c("F", "adj.P.Val", "devStageP6")]

#Scatterplot P2 vs p10 test stat-------------------
#echo=FALSE
wtDes <- subset(prDes, gType == "wt")                 #did before but for clarity
wtDat <- subset(prDat, select = prDes$gType == "wt")  #did before but for clarity
wtDesMat <- model.matrix(~devStage, wtDes)            #did before but for clarity

wtFit <- lmFit(wtDat, wtDesMat)                       #did before but for clarity
wtEbFit <- eBayes(wtFit)                              #did before but for clarity
P2Hits <- topTable(wtEbFit, coef = "devStageP2", n = Inf, sort = "none") #echo = FALSE
P10Hits <- topTable(wtEbFit, coef = "devStageP10", n = Inf, sort = "none") #echo = FALSE

#Lattice
xyplot(P10Hits$t ~ P2Hits$t, aspect = 1,
       xlab = "t-statistic for P2 effect",
       ylab = "t-statistic for P10 effect",
       xlim = c(-20, 16), ylim = c(-20, 16),
       panel = function(x, y, ...) {
         panel.smoothScatter(x, y, nbin = 100, ...)
         panel.abline(a = 0, b = 1, col = "orange")
       })                                             #echo = FALSE

#ggplot
p2vp10t <- data.frame(p2 = P2Hits$t, p10 = P10Hits$t) #makes a DF with 2 columns p2 and p10 t values
ggplot(p2vp10t, aes(p2, p10)) +
  geom_point(alpha=0.1) +   #QQQ means what?
  stat_density2d(aes(fill = ..level..), geom="polygon") +
  geom_abline(intercept=0, slope=1) +
  scale_x_continuous(limits=c(-21,20)) +
  scale_y_continuous(limits=c(-21,20)) +
  coord_fixed() +           #QQQ means what?
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black')) +
  xlab("t-statistic for P2 effect") +
  ylab("t-statistic for P10 effect")+
  theme(legend.position = "none")                     #echo = FALSE

#P-value plot: stage most distinguished from (E16)--------
#Lattice version
densityplot(~ P10Hits$adj.P.Val + P2Hits$adj.P.Val, auto.key = TRUE,
            plot.points = FALSE, n = 300)             #echo = FALSE

#ggplot version
p2vp10p <- data.frame(p2 = P2Hits$adj.P.Val, p10 = P10Hits$adj.P.Val)
ggplot(p2vp10p) +
  geom_density(aes(x = p2, colour = "red")) +
  geom_density(aes(x = p10, colour = "blue")) +
  scale_colour_manual(name = "",
                      values = c('purple', 'pink'),
                      labels = c('P10 adj p-val', 'P2 adj p-val')) +
  xlab("p2/p10 adj p-val") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))    #echo = FALSE

#Hits,plots for BH adjusted p-value < 1e-03----------
#echo = FALSE
cutoff <- 1e-03
foo <- data.frame(P2 = P2Hits$adj.P.Val < cutoff,
                  P10 = P10Hits$adj.P.Val < cutoff)
addmargins(with(foo, table(P2, P10)))

#create scatterplots with various adjusted P values
#echo = FALSE
P10pVals <- data.frame(raw = P10Hits$P.Value,
                       BH = P10Hits$adj.P.Val,
                       BY = p.adjust(P10Hits$P.Value, method = "BY"))
splom(P10pVals,
      panel = function(x, y, ... ) {
        panel.xyplot(x, y, pch = ".", ...)
        panel.abline(a = 0, b = 1, col = "orange")
      })

#Perfrom inference for some contrasts------------
    #MYNOTE: when you hear this, you need to read: "use bits of this info/fitted df to make 
    #        comparisons between subsets (and combinations of subsets)!"

#making contrast matrix
colnames(wtDesMat)
(cont.matrix <- makeContrasts(
  P10VsP6 = devStageP10 - devStageP6,
  fourweeksVsP10 = devStage4_weeks - devStageP10,
  levels = wtDesMat))

#?fitting? contrast. How does this differ from lm(fit)?
wtFitCont <- contrasts.fit(wtFit, cont.matrix) #What does contrasts.fit do? What of warning?
wtEbFitCont <- eBayes(wtFitCont)
topTable(wtEbFitCont)

#plotting topHits from lect
#echo = FALSE
foo <- topTable(wtEbFitCont)
makeGGstripplot(prepareMData(rownames(foo)[1:4])) #mystripplot FUN, her FOO.

#Adjust contrasts' p-values globally (= all together)--------------
#(what? does it change p-vals or just combine and threshold?)
cutoff <- 1e-04
wtResCont <- decideTests(wtEbFitCont, p.value = cutoff, method = "global")
summary(wtResCont)

#Plot subsets w/ diff trends, assess overlap---------
#plotting the 4 that decline from P6 to P10.
(hits1 <- rownames(prDat)[which(wtResCont[, "P10VsP6"] < 0)])
makeGGstripplot(prepareMData(hits1))

#Here are 4 of the 8 that decline from P10 to 4_weeks.
(hits2 <- rownames(prDat)[which(wtResCont[, "fourweeksVsP10"] < 0)])
makeGGstripplot(prepareMData(hits2[1:4]))

#Overlap between those declining in 1st group and 2nd group?
intersect(hits1, hits2)

#Here are 4 of the 46 that increase from P10 to 4_weeks.
(hits3 <- rownames(prDat)[which(wtResCont[, "fourweeksVsP10"] > 0)])
makeGGstripplot(prepareMData(hits3[1:4]))

#overlap between these probes and the previous "down" hits?
intersect(hits1, hits3)
intersect(hits2, hits3)

#Make the p-value cutoff less stringent and try again-------------
cutoff <- 0.01
nHits <- 8
wtResCont <- decideTests(wtEbFitCont, p.value = cutoff, method = "global")
summary(wtResCont)

#Again plot subsets w/ diff trends, assess overlap---------
hits1 <- rownames(prDat)[which(wtResCont[, "P10VsP6"] < 0)]
makeGGstripplot(prepareMData(hits1[1:nHits]))

hits2 <- rownames(prDat)[which(wtResCont[, "fourweeksVsP10"] < 0)]
makeGGstripplot(prepareMData(hits2[1:nHits]))

hits3 <- rownames(prDat)[which(wtResCont[, "P10VsP6"] > 0)]
makeGGstripplot(prepareMData(hits3[1:nHits]))

hits4 <- rownames(prDat)[which(wtResCont[, "fourweeksVsP10"] > 0)]
makeGGstripplot(prepareMData(hits4[1:nHits]))

vennDiagram(wtResCont)

#Finding gene with increasing, decreasing profile----------
hits5 <- rownames(prDat)[which(wtResCont[, "P10VsP6"] != 0 & wtResCont[, "fourweeksVsP10"] != 0)]
# QQQ isn't that pretty nonspecfic? how many would actually have a slope of 0? what rounding is goinhg on here?
makeGGstripplot(prepareMData(hits5))

hits6 <- rownames(prDat)[which(wtResCont[, "P10VsP6"] > 0 & wtResCont[, "fourweeksVsP10"] < 0)]
makeGGstripplot(prepareMData(hits6))
