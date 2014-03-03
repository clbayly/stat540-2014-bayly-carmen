setwd("/Users/carmenbayly/Stat540_Seminars/STAT540_seminar4.1/")

library(lattice)
prDat <- read.table("GSE4051_data.tsv")
str(prDat, max.level = 0)

prDes <- readRDS("GSE4051_design.rds")
str(prDes)

set.seed(987)
(theGene <- sample(1:nrow(prDat), 1))

pDat <- data.frame(prDes, gExp = unlist(prDat[theGene, ]))
str(pDat)

set.seed(987)
(theGene <- sample(1:nrow(prDat), 1))

pDat <- data.frame(prDes, gExp = unlist(prDat[theGene, ]))
str(pDat)

aggregate(gExp ~ gType, pDat, FUN = mean)

stripplot(gType ~ gExp, pDat)

t.test(gExp ~ gType, pDat)

ttRes <- t.test(gExp ~ gType, pDat)
str(ttRes)

ttRes$statistic

ttRes$p.value

install.packages("plyr", dependencies = TRUE)
library(plyr)

gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderDataFiveYear.txt"
gDat <- read.delim(file = gdURL)
(maxLeByCont <- ddply(gDat, ~ continent, summarize, maxLifeExp = max(lifeExp)))
str(maxLeByCont)
levels(maxLeByCont$continent)

(minGDPpercap<- ddply(gDat, ~ continent, summarize, minGDP = min(gdpPercap)))
ddply(gDat, ~continent, summarize, nUniqCountries = length(unique(country)))
ddply(gDat, ~ continent,
      function(x) return(c(nUniqCountries = length(unique(x$country)))))

ddply(gDat, ~ continent, summarize,
      minLifeExp = min(lifeExp), maxLifeExp = max(lifeExp),
      medGdpPercap = median(gdpPercap))

jCountry <- jCountry <- "France"  # pick, but do not hard wire, an example
(jDat <- subset(gDat, country == jCountry)) 

xyplot(lifeExp ~ year, jDat, type = c("p", "r"))
jFit <- lm(lifeExp ~ year, jDat)
summary(jFit)

(yearMin <- min(gDat$year))

jFit <- lm(lifeExp ~ I(year - 1955), jDat)
summary(jFit)

class(jFit)
mode(jFit)
names(jFit)

coef(jFit)
jFun <- function(x) coef(lm(lifeExp ~ I(year - yearMin), x)) # YOU SEEE? she just wrote a function!!
jFun(jDat)

jFun <- function(x) {
  estCoefs <- coef(lm(lifeExp ~ I(year - yearMin), x))
  names(estCoefs) <- c("intercept", "Hello")
  return(estCoefs)
}
jFun(jDat)

jFun(subset(gDat, country == "Canada"))
jCoefs <- ddply(gDat, ~country, jFun)
str(jCoefs)

tail(jCoefs)
gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderDataFiveYear.txt"
gDat <- read.delim(file = gdURL)

yearMin <- min(gDat$year)
jFun <- function(x) {
  estCoefs <- coef(lm(lifeExp ~ I(year - yearMin), x))
  names(estCoefs) <- c("intercept", "slope")
  return(estCoefs)
}
jCoefs <- ddply(gDat, ~country, jFun)
str(jCoefs)

#install.packages("xtable", dependencies = TRUE)
library(xtable)
set.seed(916)
foo <- jCoefs[sample(nrow(jCoefs), size = 15), ]
foo <- xtable(foo)
print(foo, type = "html", include.rownames = FALSE)

jCoefs <- ddply(gDat, ~country, jFun)
jCoefs <- ddply(gDat, ~country + continent, jFun)
str(jCoefs)

--------------- end bit on data aggregation

kDat <- readRDS("GSE4051_MINI.rds")
kMat <- as.matrix(kDat[c('crabHammer', 'eggBomb', 'poisonFang')])
str(kMat)
median(kMat[ , 1])  
median(kMat[ , 'eggBomb'])
median(kMat[ , 'crabHammer'])
apply(kMat, 2, median)
apply(kMat, 2, quantile, probs = c(0.25, 0.75))
colnames(kMat)[apply(kMat, 1, which.max)]
rowSums(kMat)
all.equal(rowSums(kMat), apply(kMat, 1, sum))
aggregate(eggBomb ~ devStage, kDat, FUN = mean)
aggregate(eggBomb ~ gType * devStage, kDat, FUN = mean)
aggregate(eggBomb * crabHammer ~ gType * devStage, kDat, FUN = mean) #wierd result.

#2-sample tests

keepGenes <- c("1431708_a_at", "1424336_at", "1454696_at",
               "1416119_at", "1432141_x_at", "1429226_at" )
miniDat <- subset(prDat, rownames(prDat) %in% keepGenes)
head(miniDat)
str(miniDat)
(as.matrix(miniDat))
(t(as.matrix(miniDat)))
(gExp = as.vector(t(as.matrix(miniDat))))                              
miniDat <- data.frame(gExp = as.vector(t(as.matrix(miniDat))),
                      gene = factor(rep(rownames(miniDat), each = ncol(miniDat)),
                                    levels = keepGenes))
str(miniDat)
miniDat <- suppressWarnings(data.frame(prDes, miniDat)) #not sure this is actu. working...
str(miniDat)

stripplot(gType ~ gExp | gene, miniDat,
          scales = list(x = list(relation = "free")),
          group = gType, auto.key = TRUE)

someDat <- droplevels(subset(miniDat, gene == keepGenes[1]))
str(someDat)
head(someDat)
str(miniDat)
t.test(gExp ~ gType, someDat)

library(plyr)
d_ply(miniDat, ~ gene, function(x) t.test(gExp ~ gType, x), .print = TRUE)
ttRes <- dlply(miniDat, ~ gene, function(x) t.test(gExp ~ gType, x))
names(ttRes)
ttRes[["1454696_at"]]

ttRes <- ddply(miniDat, ~ gene, function(z) {
  zz <- t.test(gExp ~ gType, z)
  round(c(tStat = zz$statistic, pVal = zz$p.value), 4)
})
ttRes
