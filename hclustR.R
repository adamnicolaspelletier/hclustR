setwd("/Volumes/pelletie/scripts")
setwd("/Users/adam-nicolaspelletier/Desktop/CindyHeatmap")

#1. Install Bionconductor
# source("https://bioconductor.org/biocLite.R")
# biocLite()

# biocLite("made4")



library("heatmap3")
library("gplots")
library("RColorBrewer")
library("made4")
library("stats")

read.csv("pDC_47.csv", sep=";", row.names=1)->pDC47
d<- as.matrix(sapply(pDC47, as.numeric))


# ## Massaging the data
# d <- t(scale(t(pDC47matrix)))         ## "dual scaling"
# d <- pmin(pmax(pDC47matrix, -3), 3)      ## Compressing data to max/min +/- 3

## Setup the dendrograms
cols <- rev(colorRampPalette(brewer.pal(10, "RdBu"))(256))
colv <- as.dendrogram(hclust(as.dist(1 - cor(d)), method="ave"))
rowv <- as.dendrogram(hclust(as.dist(1 - cor(t(d))), method="ave"))



# heatmap.2(d, trace="none", Rowv=rowv, Colv=colv, scale="row", col=cols, density.info="none")
# heatplot(d)
# heatmap3(d)
# 
# 
# svg(file="pdc47heatmap3.svg")
# 
# plot(heatmap3(pDC47matrix))
# dev.off()
# 







# cluster it
hr <- hclust(as.dist(1-cor(t(d), method="pearson")), method="complete")
hr.col <- hclust(as.dist(1-cor(d, method="pearson")), method="complete")

# define some clusters
mycl <- cutree(hr, h=max(hr$height/1.1))
# get a color palette equal to the number of clusters
clusterCols <- rainbow(length(unique(mycl)))

# create vector of colors for side bar
myClusterSideBar <- clusterCols[mycl]

heatmap.2(d, main="Hierarchical Cluster", Rowv=as.dendrogram(hr), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)

unique(mtycl)
# cutree returns a vector of cluster membership
# in the order of the original data rows
# examine it


# examine the cluster membership by it's order
# in the heatmap
mycl[hr$order]
unique(mycl)



#Export whole plot
pdf(file= "whole_plot_heatmap_pDC47.pdf" )
heatmap.2(d, Rowv=as.dendrogram(hr), Colv=as.dendrogram(hr.col), scale="row", col=myheatcol, density.info="none", trace="none", cexRow=1,cexCol=1, srtCol=45, margins= c(12,12))

dev.off()

# or simply add the cluster ID to your data
foo <- as.data.frame(cbind(d, clusterID=mycl, genename=row.names(pDC47)))

foo <-foo[hr$order,]

cluster1 <- foo[foo$clusterID == 12,]
cluster2 <- foo[foo$clusterID %in% c(10,11),]
cluster3 <- foo[foo$clusterID == 9,]



#Cluster 1
genenames1 <- lapply(cluster1$genename, as.character)
cluster1$clusterID <- NULL
cluster1$genename <- NULL

cluster1<- as.data.frame(lapply(cluster1, function(x) as.numeric(as.character(x))))
c1mat <- data.matrix(cluster1)
rownames(c1mat) <- genenames1
hr.cl1 <- hclust(as.dist(1-cor(t(c1mat), method="pearson")), method="complete")
# hr.cl1.col <- hclust(as.dist(1-cor(c1mat, method="pearson")), method="complete")

svg(filename= "pDC47_cluster1.svg", width = 12, height =12, pointsize=6 )
heatmap.2(c1mat, Rowv=as.dendrogram(hr.cl1), Colv=as.dendrogram(hr.col), scale="row", col=myheatcol, density.info="none", trace="none", cexRow=1,cexCol=1, srtCol=45, margins= c(12,12))
dev.off()


#Cluster 2
genenames2 <- lapply(cluster2$genename, as.character)
cluster2$clusterID <- NULL
cluster2$genename <- NULL
cluster2<- as.data.frame(lapply(cluster2, function(x) as.numeric(as.character(x))))
c2mat <- data.matrix(cluster2)
rownames(c2mat) <- genenames2
hr.cl2 <- hclust(as.dist(1-cor(t(c2mat), method="pearson")), method="complete")

svg(filename= "pDC47_cluster2.svg", width = 12, height =12, pointsize=6 )
heatmap.2(c2mat, Rowv=as.dendrogram(hr.cl2), Colv=as.dendrogram(hr.col), scale="row", col=myheatcol, density.info="none", trace="none", cexRow=1,cexCol=1, srtCol=45, margins= c(12,12))
dev.off()

#Cluster 3
genenames3 <- lapply(cluster3$genename, as.character)
cluster3$clusterID <- NULL
cluster3$genename <- NULL
c3mat<- as.matrix(sapply(cluster3, as.numeric))








# 
# pDC47filtered<- foo[foo$clusterID %in% l1,]
# genenames <- lapply(pDC47filtered$genename, as.character)
# pDC47filtered$clusterID <- NULL
# pDC47filtered$genename <- NULL
# d2<- as.matrix(sapply(pDC47filtered, as.numeric))
# 
# 
# 
# 
# ##start over with the reduced dataset!!
# 
# hr2 <- hclust(as.dist(1-cor(t(d2), method="pearson")), method="complete")
# 
# 
# mycl2 <- cutree(hr2, h=max(hr2$height/1.1))
# # get a color palette equal to the number of clusters
# clusterCols <- rainbow(length(unique(mycl2)))
# # create vector of colors for side bar
# myClusterSideBar <- clusterCols[mycl2]
# # choose a color palette for the heat map
# myheatcol <- rev(redgreen(75))
# # draw the heat map. Chaque couleur à gauche est un cluster défini selon le cutree paramaeter qu'on a mit plus haut. 
# heatmap.2(d2, main="Hierarchical Cluster", Rowv=as.dendrogram(hr2), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)
# 
# #pour voir la liste des clusters (pratique pour voir le nombre de clusters entres autres)
# mycl2[hr2$order]
# unique(mycl2)
# foo2 <- as.data.frame(cbind(d2, clusterID=mycl2, genename=genenames))
# l2 <- c(4,5,6,7,8,9)
# pDC47filtered2<- foo2[foo2$clusterID %in% l2,]
# genenames2 <- lapply(pDC47filtered2$genename, as.character)
# pDC47filtered2$clusterID <- NULL
# pDC47filtered2$genename <- NULL
# d3<- as.matrix(sapply(pDC47filtered2, as.numeric))
# 
# 
# 
# #####Start over to fine tune the clusters now that we eliminated a lot of dirt!
# hr3 <- hclust(as.dist(1-cor(t(d3), method="pearson")), method="complete")
# 
# 
# mycl3 <- cutree(hr3, h=max(hr3$height/1.15))
# # get a color palette equal to the number of clusters
# clusterCols <- rainbow(length(unique(mycl3)))
# # create vector of colors for side bar
# myClusterSideBar <- clusterCols[mycl3]
# # choose a color palette for the heat map
# myheatcol <- rev(redgreen(75))
# # draw the heat map. Chaque couleur à gauche est un cluster défini selon le cutree paramaeter qu'on a mit plus haut. 
# heatmap.2(d3, main="Hierarchical Cluster", Rowv=as.dendrogram(hr3), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)
# 
# #pour voir la liste des clusters (pratique pour voir le nombre de clusters entres autres)
# mycl3[hr3$order]
# unique(mycl3)
# 
# foo3 <- as.data.frame(cbind(d3, clusterID=mycl3, genename=genenames2))
# cluster1 <- foo3[foo3$clusterID %in% c(8,9),]
# pDC47filtered3<- foo3[foo3$clusterID==c(8,9,10,11,12),]
# genenames3 <- lapply(pDC47filtered2$genename, as.character)
# pDC47filtered3$clusterID <- NULL
# pDC47filtered3$genename <- NULL
# d4<- as.matrix(sapply(pDC47filtered3, as.numeric))
# 
# 
# 



# examine the data with cluster ids attached, and ordered like the heat map
foo[hr$order,]