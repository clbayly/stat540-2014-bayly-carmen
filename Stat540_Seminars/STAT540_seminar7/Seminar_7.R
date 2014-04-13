#Preliminaries---------
library(edgeR)
library(limma)
library(lattice)
library(ggplot2)
library(data.table) #for renaming my function
library(DESeq)

dat <- read.table("/Users/carmenbayly/Stat540/Stat540_Seminars/Seminars_at_once/bottomly_count_table.tsv",
                    header = TRUE, row.names = 1)
des <- read.table("/Users/carmenbayly/Stat540/Stat540_Seminars/Seminars_at_once/bottomly_phenodata.tsv",
                           header = TRUE, row.names = 1)
#Main Work----------------
#for_multiple (but in glm section)------

with(des, table(strain)) #checking to see if the rownames and colnames in des and dat line up.
group <- factor(c(rep("1", 10), rep("2", 11))) #making the factor
design <- model.matrix(~group) 
#SO. when given a factor with n levels, model.matrix makes a ? contrast matrix?
#that, when multipled with a matrix of counts, gives .. intercept+interaction effects???

tail(design)
head(design)
head(des)
head(dat)

#GLMedgeR----------
dge.glm <- DGEList(counts = prDes, group = group)
str(dge.glm)

names(dge.glm)
all(rownames(des) == colnames(dat)) #Are they identical?
with(des, table(strain)) #how many samples per strain? so can make group
group <- factor(c(rep("1", 10), rep("2", 11))) #making a factor
dge.glm <- DGEList(counts = dat, group = group) #DGElists contain 2 inf. types...
dge.glm[["samples"]]


design <- model.matrix(~group)
dge.glm.com.disp <- estimateGLMCommonDisp(dge.glm, design, verbose = TRUE)
#estimates general linear model common dispersion. QQQ what is this?

dge.glm.trend.disp <- estimateGLMTrendedDisp(dge.glm.com.disp, design)
#estimates general linear model trended dispesion. QQQ what is this?

dge.glm.tag.disp <- estimateGLMTagwiseDisp(dge.glm.trend.disp, design)
#etsimates General linear model tagwise dispersion

plotBCV(dge.glm.tag.disp) #QQQ what's BVC? biological variation coefficient?


fit <- glmFit(dge.glm.tag.disp, design)
colnames(coef(fit))
lrt <- glmLRT(fit, coef = 2) #QQQ what's LRT???
topTags(lrt)                #QQQ what does topTags do?
tt.glm <- topTags(lrt, n = Inf)
class(tt.glm)

nrow(tt.glm$table[tt.glm$table$FDR < 0.01, ]) #I don't know what you're doing here.
interestingSamples <- rownames(tt.glm$table[tt.glm$table$FDR < 1e-50, ]) #getting rowname of samples with an FDR <1e-50
cpm(dge.glm.tag.disp)[interestingSamples, ] getting the counts per million of those.

summary(de.glm <- decideTestsDGE(lrt, p = 0.05, adjust = "BH")) #QQQ what does 'decideTestsDGE()' do? 
#451 genes are under-expressed in groups 2,35660 show no difference,
#425

#plotting the tagwise log fold change against log-cpm
tags.glm <- rownames(dge.glm.tag.disp)[as.logical(de.glm)] #QQQ not sure what all these parts do
plotSmear(lrt, de.tags = tags.glm)
abline(h = c(-2, 2), col = "blue")

#DESeq-----------------

deSeqDat <- newCountDataSet(dat, group)
head(counts(deSeqDat))

#estimate the size factors to account for differences in library coverage and estimate the variance
deSeqDat <- estimateSizeFactors(deSeqDat)
sizeFactors(deSeqDat)
deSeqDat <- estimateDispersions(deSeqDat)
plotDispEsts(deSeqDat)
results <- nbinomTest(deSeqDat, levels(group)[1], levels(group)[2])
str(results) #why are the padj values not there?
plotMA(results)

#Voom and Limma------------------
norm.factor <- calcNormFactors(dat)
dat.voomed <- voom(dat, design, plot = TRUE, lib.size = colSums(dat) * norm.factor)

fit <- lmFit(dat.voomed, design)
fit <- eBayes(fit)
topTable(fit)
