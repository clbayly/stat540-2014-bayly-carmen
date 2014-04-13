
#Load data and packages----------
library(RColorBrewer)
library(cluster)
library(pvclust)
library(xtable)
library(limma)
library(plyr)

prDat <- read.table("GSE4051_data.tsv",
                    header = TRUE, row.names = 1) # the whole enchilada
str(prDat, max.level = 0)

prDes <- readRDS("GSE4051_design.rds")
str(prDes)

#Rescaling the rows, since we're not interested in absolute differences----
sprDat <- t(scale(t(prDat)))
str(sprDat, max.level = 0, give.attr = FALSE)

round(data.frame(avgBefore = rowMeans(head(prDat)),
                 avgAfter = rowMeans(head(sprDat)),
                 varBefore = apply(head(prDat), 1, var),
                 varAfter = apply(head(sprDat), 1, var)), 2)

#Sample Clustering----------
#Hierarchical clustering for photoRec data--------

# compute pairwise distances
pr.dis <- dist(t(sprDat), method = 'euclidean')

# create a new factor representing the interaction of gType and devStage
prDes$grp <- with(prDes, interaction(gType, devStage))
summary(prDes$grp)


# compute hierarchical clustering using different linkage types
pr.hc.s <- hclust(pr.dis, method = 'single')
pr.hc.c <- hclust(pr.dis, method = 'complete')
pr.hc.a <- hclust(pr.dis, method = 'average')
pr.hc.w <- hclust(pr.dis, method = 'ward')

# plot them
op <- par(mar = c(0,4,4,2), mfrow = c(2,2))

plot(pr.hc.s, labels = FALSE, main = "Single", xlab = "")
plot(pr.hc.c, labels = FALSE, main = "Complete", xlab = "")
plot(pr.hc.a, labels = FALSE, main = "Average", xlab = "")
plot(pr.hc.w, labels = FALSE, main = "Ward", xlab = "")

par(op)

# identify 10 clusters
op <- par(mar = c(1,4,4,1))
plot(pr.hc.w, labels = prDes$grp, cex = 0.6, 
     main = "Ward showing 10 clusters")
rect.hclust(pr.hc.w, k = 10)

#Play with the options of the heatmap function and compare the different heatmaps. ------
jGraysFun <- colorRampPalette(brewer.pal(n = 9, "Greys"))
gTypeCols <- brewer.pal(11, "RdGy")[c(4,7)]
heatmap(as.matrix(sprDat), Rowv = NA, col = jGraysFun(256),
        hclustfun = function(x) hclust(x, method = 'ward'),
        scale = "none", labCol = prDes$grp, labRow = NA, margins = c(8,1),
        ColSideColor = gTypeCols[unclass(prDes$gType)])
legend("topright", legend = levels(prDes$gType),
       col = gTypeCols, lty = 1, lwd = 5, cex = 0.5)

#Play with the options of the heatmap function and compare the different heatmaps. ------
#sidenote: heatmap construction takes FOREVER.
jGraysFun <- colorRampPalette(brewer.pal(n = 9, "Greys"))
gTypeCols <- brewer.pal(11, "RdGy")[c(4,7)]
heatmap(as.matrix(sprDat), Rowv = NA, col = jGraysFun(256),
        hclustfun = function(x) hclust(x, method = 'ward'),
        scale = "none", labCol = prDes$grp, labRow = NA, margins = c(8,1),
        ColSideColor = gTypeCols[unclass(prDes$gType)])
legend("topright", legend = levels(prDes$gType),
       col = gTypeCols, lty = 1, lwd = 5, cex = 0.5)

heatmap(as.matrix(sprDat), Rowv = NA, col = jGraysFun(256),
        hclustfun = function(x) hclust(x, method = 'average'),
        scale = "none", labCol = prDes$grp, labRow = NA, margins = c(8,1),
        ColSideColor = gTypeCols[unclass(prDes$gType)])

heatmap(as.matrix(sprDat), Rowv = NA, col = jGraysFun(256),
        hclustfun = function(x) hclust(x, method = 'complete'),
        scale = "none", labCol = prDes$grp, labRow = NA, margins = c(8,1),
        ColSideColor = gTypeCols[unclass(prDes$gType)])

heatmap(as.matrix(sprDat), Rowv = NA, col = jGraysFun(256),
        hclustfun = function(x) hclust(x, method = 'single'),
        scale = "none", labCol = prDes$grp, labRow = NA, margins = c(8,1),
        ColSideColor = gTypeCols[unclass(prDes$gType)])

#Partitioning methods for photoRec data----------
#K-means clustering--------
#Objects in columns

set.seed(31)
k <- 5
pr.km <- kmeans(t(sprDat), centers = k, nstart =  50)

#We can look at the within sum of squares of each cluster
pr.km$withinss


#We can look at the composition of each cluster
pr.kmTable <- data.frame(devStage = prDes$devStage, cluster = pr.km$cluster)
prTable  <-  xtable(with(pr.kmTable, table(devStage,cluster)),
                    caption='Number of samples from each develomental stage within each k-means cluster')

align(prTable) <- "lccccc"
print(prTable, type = 'html', caption.placement = 'top')

#PAM algorithm---------
pr.pam <- pam(pr.dis, k = k)
pr.pamTable <- data.frame(devStage = prDes$devStage,
                          cluster = pr.pam$clustering)
pamTable  <-  xtable(with(pr.pamTable, table(devStage, cluster)),
                     caption='Number of samples from each develomental stage within each PAM cluster')

align(pamTable) <- "lccccc"
print(pamTable, type = 'html', caption.placement = 'top')

#Silhuoette plot------------
op <- par(mar = c(5,1,4,4))
plot(pr.pam, main = "Silhouette Plot for 5 clusters")

par(op)

# Exercise: draw a plot with number of clusters in the x-axis 
# and the average silhouette widths in the y-axis. 
# Use the information obtained to determine if 5 was the best choice for the number of clusters.
# (to revisit)

# Exercise: For a common choice of $k$, compare the clustering across different methods, 
# e.g. hierarchical (pruned to specific $k$, obviously), k-means, PAM. 
# You will re-discover the "label switching problem" for yourself. How does that manifest itself? 
# How concordant are the clusterings for different methods?
# (to revisit)

# Making topDat, for gene clusering)-----------
cutoff <- 1e-05
DesMat <- model.matrix(~ devStage, prDes)
dsFit <- lmFit(prDat, DesMat)
dsEbFit <- eBayes(dsFit)
dsHits <- topTable(dsEbFit,
                   coef = grep("devStage", colnames(coef(dsEbFit))),
                   p.value = cutoff, n = Inf)
numBHhits <- nrow(dsHits)
topGenes <- rownames(dsHits)

#Scaled data of topGenes
topDat <- sprDat[topGenes, ]

#Gene clustering--------
geneC.dis <- dist(topDat, method = 'euclidean')
geneC.hc.a <- hclust(geneC.dis, method = 'average')
plot(geneC.hc.a, labels = FALSE,
     main = "Hierarchical with Average Linkage", xlab = "")

#Partitioning----------
set.seed(1234)
k <- 5
kmeans.genes <- kmeans(topDat, centers = k)

# choose which cluster we want
clusterNum <- 1 

# Set up the axes without plotting; ylim set based on trial run.
plot(kmeans.genes$centers[clusterNum, ], ylim = c(-4, 4), type = 'n',
     xlab = "Samples", ylab = "Relative expression" ) 

# Plot the expression of all the genes in the selected cluster in grey. 
matlines(y = t(topDat[kmeans.genes$cluster == clusterNum, ]), col = 'grey') 

# Add the cluster center. This is last so it isn't underneath the members
points(kmeans.genes$centers[clusterNum, ], type = 'l') 

# Optional: colored points to show which development stage the samples are from.
points(kmeans.genes$centers[clusterNum, ],  col = prDes$devStage, pch = 20) 

#Dendrograms on Heatmaps-----------
devStageCols <- brewer.pal(11, "RdGy")[c(2,4,7,9,11)]
heatmap(as.matrix(topDat), col = jGraysFun(256),
        hclustfun = function(x) hclust(x, method = 'average'),
        labCol = prDes$grp, labRow = NA, margin = c(8,1), scale = "none",
        ColSideColor = devStageCols[unclass(prDes$devStage)])
legend("topleft", levels(prDes$devStage), col = devStageCols,
       lty = 1, lwd = 5, cex = 0.5)

#Redefining the attributes------
annoTopDat <- stack(as.data.frame(topDat)) # stack probe data tall and skinny
annoTopDat$probeset <- rownames(topDat) # add probeset ID as variable
## get info on gType and devStage, then average over reps within devStage
annoTopDat <- merge(annoTopDat, prDes, by.x = "ind", by.y = "sidChar")
devStageAvg <- ddply(annoTopDat, ~ probeset, function(x) {
  avgByDevStage <- aggregate(values ~ devStage, x, mean)$values
  names(avgByDevStage) <- levels(x$devStage)
  avgByDevStage
})
## put probset info back into rownames and heatmap
rownames(devStageAvg) <- devStageAvg$probeset
devStageAvg$probeset <- NULL
str(devStageAvg)

heatmap(as.matrix(devStageAvg), Colv = NA, col = jGraysFun(256),
        hclustfun = function(x) hclust(x,method = 'average'),
        labCol = colnames(devStageAvg), labRow = NA, margin = c(8,1))

#average expression of genes within a cluster for each developmental stage.-------
k <- 4
geneDS.km <- kmeans(devStageAvg, centers = k, nstart = 50)
clust.centers <- geneDS.km$centers

#Look at all clusters
op <- par(mfrow = c(2, 2))
for(clusterNum in 1:4) {
  # Set up the axes without plotting; ylim set based on trial run.
  plot(clust.centers[clusterNum,], ylim = c(-4,4), type='n',
       xlab = "Develomental Stage", ylab = "Relative expression",
       axes = F, main = paste("Cluster", clusterNum, sep = " ")) 
  axis(2)
  axis(1, 1:5, c(colnames(clust.centers)[1:4],"4W"), cex.axis = 0.9)
  
  # Plot the expression of all the genes in the selected cluster in grey.
  matlines(y = t(devStageAvg[geneDS.km$cluster == clusterNum, ]),
           col = 'grey') 
  
  # Add the cluster center. This is last so it isn't underneath the members
  points(clust.centers[clusterNum, ] , type = 'l') 
  
  # Optional: points to show development stages.
  points(clust.centers[clusterNum, ],  pch = 20)
} 

par(op)

#compare all clusters' centers.
plot(clust.centers[clusterNum, ], ylim = c(-4, 4), type = 'n',
     xlab = "Develomental Stage", ylab = "Average expression",
     axes = FALSE, main = "Clusters centers") 
axis(2)
axis(1, 1:5, c(colnames(clust.centers)[1:4],"4W"), cex.axis = 0.9)

for(clusterNum in 1:4) {
  points(clust.centers[clusterNum,], type = 'l', col = clusterNum, lwd=2) 
  points(clust.centers[clusterNum,] , col = clusterNum, pch = 20)
}

#3-D illustration of clusters determined by kmeans.
cloud(devStageAvg[ ,"E16"] ~ devStageAvg[ ,"P6"] *
        devStageAvg[ ,"4_weeks"], col = geneDS.km$clust,
      xlab = "E16", ylab = "P6", zlab = "4_weeks")

##Statistical measures to evaluate clusters---------
pvc <- pvclust(topDat, nboot = 100)
plot(pvc, labels = prDes$grp, cex = 0.6)
pvrect(pvc, alpha = 0.95) 

#PCA (principal components analysis)------------
pcs <- prcomp(sprDat, center = F, scale = F) 

# scree plot
plot(pcs) 

# append the rotations for the first 10 PCs to the phenodata
prinComp <- cbind(prDes, pcs$rotation[prDes$sidNum, 1:10]) 

# scatter plot showing us how the first few PCs relate to covariates
plot(prinComp[ ,c("sidNum", "devStage", "gType", "PC1", "PC2", "PC3")],
     pch = 19, cex = 0.8) 

# plot data on first two PCs, colored by development stage
plot(prinComp[ ,c("PC1","PC2")], bg = prDes$devStage, pch = 21, cex = 1.5)
legend(list(x = 0.2, y = 0.3), as.character(levels(prDes$devStage)),
       pch = 21, pt.bg = c(1,2,3,4,5))

#Most of the plots in this Seminar were done with basic R graphics. 
#As an exercise, you can try to create new plots using lattice and/or ggplot2!