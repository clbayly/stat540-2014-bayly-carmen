#lattice graphics
setwd("~/Stat540_Seminars/STAT540_seminar3.1")
library(lattice)
kDat <- readRDS("GSE4051_MINI.rds")
str(kDat)
table(kDat$devStage)
table(kDat$gType)
with(kDat, table(devStage,gType))
xyplot(eggBomb ~ crabHammer, kDat)
xyplot(poisonFang ~ crabHammer, kDat)
xyplot(eggBomb + poisonFang ~crabHammer, kDat, auto.key = TRUE)
xyplot(eggBomb + poisonFang ~crabHammer, kDat, outer = TRUE, grid = TRUE)
xyplot(eggBomb + poisonFang ~crabHammer, kDat, 
       outer = TRUE, grid = TRUE,
       groups = gType, auto.key = TRUE)
nDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(
                    rep(c("eggBomb", "poisonFang"), each = nrow(kDat))),
                  geneExp = c(eggBomb, poisonFang)))
str(nDat)
str(kDat)
xyplot(geneExp ~ crabHammer | probeset, nDat,
       grid = TRUE,
       groups = gType, auto.key = TRUE)
#You try: Remake this plot but instead of conveying genotype via color, show developmental stage
nDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(rep(c("eggBomb", "poisonFang"), each = nrow(kDat))),
                  geneExp = c(eggBomb, poisonFang)))
str(nDat)
#and then you can do...
xyplot(geneExp ~ crabHammer | probeset, nDat,
       grid = TRUE,
       groups = devStage, auto.key = TRUE)

#stripplot

oDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  probeset = factor(rep(c("crabHammer", "eggBomb",
                                          "poisonFang"), each = nrow(kDat))),
                  geneExp = c(crabHammer, eggBomb, poisonFang)))
str(oDat)
stripplot(~ geneExp, oDat)
stripplot(probeset ~ geneExp, oDat)
stripplot(probeset ~ geneExp, oDat, jitter.data = TRUE)
stripplot(~ geneExp | probeset, oDat, layout = c(nlevels(oDat$probeset), 1))
stripplot(~ geneExp| probeset, oDat, 
          layout = c(nlevels(oDat$probeset), 1),
          groups = gType, auto.key = TRUE)
stripplot(geneExp ~ devStage, oDat)
stripplot(geneExp ~ devStage | probeset, oDat, 
          layout = c(nlevels(oDat$probeset), 1))
stripplot(geneExp ~ devStage | probeset, oDat, 
          layout = c(nlevels(oDat$probeset), 1),
          groups = gType, auto.key = TRUE)
stripplot(geneExp ~ devStage | probeset, oDat, 
          layout = c(nlevels(oDat$probeset), 1),
          groups = gType, auto.key = TRUE, grid = TRUE,
          type = c('p', 'a'))
densityplot(~ geneExp, oDat)
densityplot(~ geneExp | gType, oDat,
            grid = TRUE)
densityplot(~ geneExp, oDat,
            groups = gType, auto.key = TRUE)
jBw <- 0.2
jn <- 400
densityplot(~ geneExp, oDat,
            groups = gType, auto.key = TRUE,
            bw = jBw, n = jn,
            main = paste("bw =", jBw, ", n =", jn))
#You try: use densityplot() to explore the gene 
  #expression distribution by gene and/or developmental stage.
  #ahh maybe later

bwplot(geneExp ~ devStage, oDat)
bwplot(geneExp ~ devStage | gType, oDat)
bwplot(geneExp ~ devStage, oDat,
       panel = panel.violin)

#heatmaps
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)

prDes <- readRDS("GSE4051_design.rds")
str(prDes)
set.seed(1)
(yo <- sample(1:nrow(prDat), size = 50))
hDat <- prDat[yo, ]
str(hDat)
hDat <- as.matrix(t(hDat))
rownames(hDat) <- with(prDes,
                       paste(devStage, gType, sidChar, sep="_"))
str(hDat)
heatmap(hDat, Rowv = NA, Colv = NA, scale="none", margins = c(5, 8))
heatmap(hDat, Rowv = NA, Colv = NA, col = cm.colors(256),
        scale="none", margins = c(5, 8))
library(RColorBrewer)
display.brewer.all()
jGraysFun <- colorRampPalette(brewer.pal(n = 9, "Greys"))
jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
heatmap(hDat, Rowv = NA, Colv = NA, scale="none", margins = c(5, 8),
        col = jGraysFun(256))
heatmap(hDat, Rowv = NA, Colv = NA, scale="none", margins = c(5, 8),
       col = jBuPuFun(256))
heatmap(hDat, margins = c(5, 8), col = jBuPuFun(256))
heatmap(hDat, col = jBuPuFun(256), margins = c(5, 8), scale=c("column"))
library(gplots)
heatmap.2(hDat, col = jGraysFun, trace = "none")
heatmap.2(hDat, col = jBuPuFun, trace = "none")
set.seed(924)
(yo <- sample(1:ncol(prDat), size = 2))
y <- prDat[[yo[1]]]
z <- prDat[[yo[2]]]
str(y)
str(z)
xyplot(y ~ z, asp = 1)
smoothScatter(y ~ z, asp = 1)
xyplot(y ~ z, asp = 1, panel = panel.smoothScatter, nbin = 150)
library(hexbin)
hexbinplot(y ~ z)
#plot matrix
set.seed(3)
(yo <- sample(1:ncol(prDat), size = 4))
pairDat <- subset(prDat, select = yo)
str(pairDat)
pairs(pairDat)
pairs(pairDat,
      panel = function(...) smoothScatter(..., add=TRUE))
splom(pairDat)
splom(pairDat, panel = panel.smoothScatter, raster = TRUE)
hexplom(pairDat)
#there is a take-home problem I shall not attempt right now.