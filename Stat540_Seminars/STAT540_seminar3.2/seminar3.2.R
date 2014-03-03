#ggplot2 graphics
setwd("~/Stat540_Seminars/STAT540_seminar3.2")
library(ggplot2)
apropos("^geom_")
apropos("^stat_")
apropos("^scale_")
#recreating lattice plots
kDat <- readRDS("GSE4051_MINI.rds")
str(kDat)
table(kDat$devStage)
table(kDat$gType)
with(kDat, table(devStage,gType))
qplot(crabHammer, eggBomb, data = kDat)
#Scatterplots
p <- ggplot(kDat, aes(x = crabHammer, y = eggBomb))
str(p)
print(p)
(q <- p + geom_point())
(r <- q + stat_smooth())
(s <- r + theme_bw() + 
   xlab("Expression of crabHammer") + 
   ylab("Expression of eggBomb") + 
   ggtitle("Scatterplot for expression levels"))
nDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType, crabHammer,
                  probeset = factor(rep(c("eggBomb", "poisonFang"),
                                        each = nrow(kDat))),
                  geneExp = c(eggBomb, poisonFang)))
str(nDat)
(p <- ggplot(nDat, aes(crabHammer, geneExp)) + 
   geom_point(color = probeset)) #error! setting a graphic elem applies to all data. 
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) + 
   geom_point()) #instead, to change appnce. of SOME data, have to MAP graphical element.
#add a smoothing line
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) + 
   geom_point() + 
   stat_smooth(se = F))
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) + 
   geom_point() + 
   stat_smooth(se = F, aes(group = 1)))
#make facets
(p <- ggplot(nDat, aes(crabHammer, geneExp)) + 
   geom_point() + 
   facet_wrap(~ probeset))
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = gType)) + 
   geom_point() + 
   facet_wrap(~ probeset))
#STRIP PLOTS
#first reshape data...
oDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  probeset = factor(rep(c("crabHammer", "eggBomb",
                                          "poisonFang"), each = nrow(kDat))),
                  geneExp = c(crabHammer, eggBomb, poisonFang)))
str(oDat)
#thenWePlot
(p <- ggplot(oDat, aes(geneExp, probeset)) + 
   geom_point())
#adding jitter
(p <- ggplot(oDat, aes(geneExp, probeset)) + 
   geom_point(position = position_jitter(height = 0.1)))
#and more ex...
(p <- ggplot(oDat, aes(devStage, geneExp)) + 
   geom_point())
#...and in many panels!
(p <- p + facet_wrap(~ probeset))
#..with genotype info!
(p <- p + aes(color = gType))
#adding averages
(p <- p + stat_summary(fun.y = mean, geom = "point", shape = 4, size = 4))

#DENSITY PLOTS
(p <- ggplot(oDat, aes(geneExp)) + 
   geom_density())
(p <- ggplot(oDat, aes(geneExp)) + 
   stat_density(geom = "line", position = "identity"))
#adding data points to the bottom
(p <- ggplot(oDat, aes(geneExp)) + 
   stat_density(geom = "line", position = "identity") + 
   geom_point(aes(y = 0.05), position = position_jitter(height = 0.005)))
#changing bandwidth
(p <- ggplot(oDat, aes(geneExp)) + 
   stat_density(geom = "line", position = "identity", adjust = 0.5) + 
   geom_point(aes(y = 0.05), position = position_jitter(height = 0.005)))
#facets!
(p <- p + facet_wrap(~ gType))
#color/genotype
(p <- ggplot(oDat, aes(geneExp, color = gType)) + 
   stat_density(geom = "line", position = "identity") + 
   geom_point(aes(y = 0.05), position = position_jitter(height = 0.005)))

#BOXPLOT
(p <- ggplot(oDat, aes(devStage, geneExp)) + 
   geom_boxplot())
#separate genotypes
(p <- p + facet_wrap(~ gType))
#violin
(p <- ggplot(oDat, aes(devStage, geneExp)) + 
   geom_violin())

#OVERPLOTTING AND PLOT MATRIX
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)
prDes <- readRDS("GSE4051_design.rds")
str(prDes)
set.seed(2)
(yo <- sample(1:ncol(prDat), size = 2))
bDat <- data.frame(y = prDat[[yo[1]]], z = prDat[[yo[2]]])
str(bDat)
(p <- ggplot(bDat, aes(z, y)) + 
   geom_point())
#"reducing" transparacy of plot
(p <- ggplot(bDat, aes(z, y)) + 
   geom_point(alpha = 0.1))
#density2D function
(p <- ggplot(bDat, aes(z, y)) + 
   stat_density2d())
#density2d with color
(p <- ggplot(bDat, aes(z, y)) + 
   stat_density2d(geom = "tile", contour = F, aes(fill = ..density..)) + 
   scale_fill_gradient(low = "white", high = "blue"))
#stat binhex
(p <- ggplot(bDat, aes(z, y)) + 
   stat_binhex())
#using plotmatrix
set.seed(3)
(yo <- sample(1:ncol(prDat), size = 4))
pairDat <- subset(prDat, select = yo)
str(pairDat)
#with stat_binhex
(p <- plotmatrix(pairDat) + 
   stat_binhex())

#Me: getting many graphs on one sheet
install.packages("gridExtra")
library(gridExtra)
p1 = ggplot(oDat, aes(devStage, geneExp)) + 
  geom_violin()
p2 = ggplot(oDat, aes(geneExp)) + 
  stat_density(geom = "line", position = "identity", adjust = 0.5) + 
  geom_point(aes(y = 0.05), position = position_jitter(height = 0.005))
p3 = ggplot(oDat, aes(geneExp, probeset)) + 
  geom_point(position = position_jitter(height = 0.1))
grid.arrange(p1, p2, p3, ncol=3)
