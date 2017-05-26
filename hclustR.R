##Load required packages

library("gplots")
library("made4")



#1. Import desired dataset as a dataframe
read.csv("pDC_47.csv", sep=";", row.names=1)->ex_data


# 42. Transformer en matrice numérique pour heatmap2. 
d<- as.matrix(sapply(ex_data, as.numeric))

# 3. cluster it
# hierarchical clustering on gene expression. Change method to "average" for different results. And euclidean method can also be used.
hr <- hclust(as.dist(1-cor(t(d), method="pearson")), method="complete")  

# 4. hierarchical clustering on cell populations. Change method to "average" for different results. And euclidean method can also be used.
hr.col <- hclust(as.dist(1-cor(d, method="pearson")), method="complete")


#5. Export whole plot
pdf(file= "ex_heatmap.pdf" )
heatmap.2(d, Rowv=as.dendrogram(hr), Colv=as.dendrogram(hr.col), scale="row", col=myheatcol, density.info="none", trace="none", cexRow=1,cexCol=1, srtCol=45, margins= c(12,12))

dev.off()


