#### LIBRARIES ####
library(cluster)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(factoextra)
library(caret)



#### DIRECTORY ####
set.seed(1234)
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#### LOADING DATA ####
dati=read.csv("Songs' sentiments.csv", sep = ",")
#escluding the first column
finalData=dati[,-1]
#naming rows using first column
rownames(finalData)=dati[,1]

#### DESCRIPTIVE ANALYSIS ####
plot(finalData$artist)
plot(finalData[,-1], pch=20, col="#c24450")
plot(finalData[,c(5,6)], pch=20, col="#c24450")


#### K-MEANS ####
cluster.kmeans=kmeans(finalData[,c(9,10)], centers=2)
cluster.kmeans
plot(finalData[,c(5,6)], col=cluster.kmeans$cluster)
text(finalData[,c(5,6)], labels = finalData$artist, cex=0.8)
table(finalData$artist)
table(cluster.kmeans$cluster, finalData$artist)
fviz_cluster(kmeansPCA, data=finalData[,c(5,6)])

#### PCA for K-MEANS ####
pcaData=prcomp(finalData[,-1], center = TRUE)
summary(pcaData)
dataPCA=as.data.frame(-pcaData$x[,1:2])

#### K-MEANS using PCA ####
kmeansPCA = kmeans(dataPCA, centers=2)
kmeansPCA
table(finalData$artist)
table(kmeansPCA$cluster, finalData$artist)
fviz_cluster(kmeansPCA, data = dataPCA)

#### THE RESULTS ARE ALMOST THE SAME (PCA IS SLIGHTLY BETTER, EVEN IF EXPLAINED VARIANCE IS LOWER) BUT USING PCA I DON'T HAVE TO LOOKING FOR THE MOST EXPLANATORY VARIABLES ####

