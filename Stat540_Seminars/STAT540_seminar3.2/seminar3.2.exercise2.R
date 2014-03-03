#new angle for exercise 2
#OK. How do I get my own probeset?

setwd("~/Stat540_Seminars/STAT540_seminar3.2")
prDat <- read.table("GSE4051_data.tsv")
str(prDat)

prDes <- readRDS("GSE4051_design.rds")
str(prDes)

#OK. so she said in preprocessing, she picked 3 probesets (ROWS)
#transposed the data (reshaping them into 3 columns)
#glue these with the genotype info
#replace affymetrix probe names with arbitrary pokemon attacks. 

#Quest1: find the transpose command!
# according to http://www.statmethods.net/management/reshape.html,
#it's t()

#how do I randomly select rows?
#set.seed(1)
#(yo <- sample(1:nrow(prDat), size = 50))

set.seed(1)
(abilityScore <- sample(1:nrow(prDat), size = 6))
#I'm gonna assume the output means 'rows'. next:
jDat <- prDat[abilityScore, ]
str(jDat)

#now going from Jenny's 03_createMiniDataset.r code:
tjDat  <-  data.frame(t(jDat))
names(tjDat) <- c("Strength", "Dexterity", "Constitution", "Intelligence", "Wisdom", "Charisma")
str(tjDat)

#puts two columns from 2 different data frames beside each other
cbind(prDes$sidChar, rownames(tjDat))
identical(prDes$sidChar, rownames(tjDat))

#merge the 2 data frames
kDat  <-  data.frame(prDes, tjDat)
str(kDat) #YAYYYYYYYY!

#OK. Let's check out these genes. 
#gene key - using DND ability score categories (previous were razorLeaf, suckerPunch, drillPeck, mistBall, tickle, and zenHeadButt)
          #Strength = 7952 
          #Dexterity = 11145  
          #Constitution = 17156 
          #Intelligence = 27198
          #Wisdom = 6040
          #Charisma = 26902
table(kDat$devStage)
table(kDat$gType)
with(kDat, table(devStage, gType))
#ookay, all good til here. Now, I could, using less code, 
#maybe make a marix of plots. But that would be annoying and I want results fast.
#So: gridExtra it is!
library(ggplot2)
library(gridExtra)
qp1 <- qplot(Strength, data = kDat) +  xlab("Exp of STR") + ggtitle("Strength")
qp2 <- qplot(Dexterity, data = kDat) +  xlab("Exp of DEX") + ggtitle("Dexterity")
qp3 <- qplot(Constitution, data = kDat) +  xlab("Exp of CON") + ggtitle("Constitution")
qp4 <- qplot(Intelligence, data = kDat) +  xlab("Exp of INT") + ggtitle("Intelligence")
qp5 <- qplot(Wisdom, data = kDat) +  xlab("Exp of WIS") + ggtitle("Wisdom")
qp6 <- qplot(Charisma, data = kDat) +  xlab("Exp of CHA") + ggtitle("Charisma")
grid.arrange(qp1, qp2, qp3, qp4, qp5, qp6, ncol=2, nrow=3)
#And now as Density plots!

dp1 <- ggplot(kDat, aes(Strength)) + 
   stat_density(geom = "line", position = "identity") + 
   geom_point(aes(y = 0.05), position = position_jitter(height = 0.02))
dp2 <- ggplot(kDat, aes(Dexterity)) + 
   stat_density(geom = "line", position = "identity") + 
   geom_point(aes(y = 0.05), position = position_jitter(height = 0.02))
dp3 <- ggplot(kDat, aes(Constitution)) + 
  stat_density(geom = "line", position = "identity") + 
  geom_point(aes(y = 0.05), position = position_jitter(height = 0.02))
dp4 <- ggplot(kDat, aes(Intelligence)) + 
  stat_density(geom = "line", position = "identity") + 
  geom_point(aes(y = 0.05), position = position_jitter(height = 0.02))
dp5 <- ggplot(kDat, aes(Wisdom)) + 
  stat_density(geom = "line", position = "identity") + 
  geom_point(aes(y = 0.05), position = position_jitter(height = 0.02))
dp6 <- ggplot(kDat, aes(Charisma)) + 
  stat_density(geom = "line", position = "identity") + 
  geom_point(aes(y = 0.05), position = position_jitter(height = 0.02))
grid.arrange(dp1, dp2, dp3, dp4, dp5, dp6, ncol=2, nrow=3)
#Result of above code: Y scales all different. Would have to define X axis for each?
#could I use  FOR loop to condense this code?
#Oh, I get what I want to see. a line graph.
#underlying question: What kind of ranges do these genes have?
#but this requires restructuring of kDat!
oDat <-
  with(kDat,
       data.frame(sidChar, sidNum, devStage, gType,
                  probeset = factor(rep(c("Strength", "Dexterity",
                                          "Constitution", "Intelligence",
                                          "Wisdom", "Charisma"),
                                           each = nrow(kDat))),
                  geneExp = c(Strength, Dexterity, Constitution, 
                              Intelligence, Wisdom, Charisma)))
str(oDat)
(p <- ggplot(oDat, aes(geneExp, probeset)) + 
   geom_point(position = position_jitter(height = 0.1)))
#great - someone who is strong and persuasive, but stupid (or at least unwise)
#let's find out these stretch over development
(p <- ggplot(oDat, aes(devStage, geneExp, color = gType, group = gType)) + 
   geom_point() + 
   facet_wrap(~ probeset, nrow = 3, ncol = 2) +
   aes(group = gType) +
   stat_smooth(group = 1) + 
   stat_summary(fun.y = mean, geom = "point", shape = 4, size = 4))
#Really, I think 'Charisma' is the only interesting geneset here...
#well, maybe strenth too...
qplot(Charisma, Strength, data = kDat, color = gType)
p <- ggplot(kDat, aes(x = Charisma, y = Strength))
str(p)
(p  <- p + geom_point() +
  stat_smooth() + theme_bw() +
  xlab("Expression of Charisma") + 
  ylab("Expression of Strength") + 
  ggtitle("Scatterplot for expression levels"))
  
Next: plot Charisma and strentgh togehter against other geners 

dp1 <- ggplot(kDat, aes(Strength)) + 
  stat_density(geom = "line", position = "identity") + 
  geom_point(aes(y = 0.05), position = position_jitter(height = 0.02))
#next: 
#nice to set age as a continuous variable here. (that's in one of the lectures, no?)
#will plot, for Charisma and Strength, KO vs wt.()
#will transform previous histograms into density plots
#will turn those into boxplots and violin plots, wt and KO
  #(will use facet AND grid.arrange)
#but what to do with overplotting and matrix?

