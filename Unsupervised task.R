suppressMessages(library(plyr))
suppressMessages(library(Amelia))
suppressMessages(library(corrplot))
suppressMessages(library(ggplot2))
suppressMessages(library(MASS))
suppressMessages(library(randomForest))
suppressMessages(library(party))
suppressMessages(library(caret))
suppressMessages(library(GGally))
suppressMessages(library(corrplot))
suppressMessages(library(caTools))
suppressMessages(library(MASS))
suppressMessages(library(car))
suppressMessages(library(dplyr))
suppressMessages(library(pROC))
suppressMessages(library(mboost))
suppressMessages(library(ggalluvial))
suppressMessages(library(gridExtra))
suppressMessages(library(treemapify))
suppressMessages(library(treemap))
suppressMessages(library(e1071))
suppressMessages(library(klaR))
suppressMessages(library(arm))
suppressMessages(library(reticulate))
suppressMessages(library(varhandle))
suppressMessages(library(data.table))
suppressMessages(library(mltools))
options(warn=-1)

install.packages("caret")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("DataExplorer")
install.packages("tidyverse")
install.packages("ClusterR")
install.packages("cluster")

library(ClusterR)
library(cluster)
library(caret)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(tidyverse)

Iris <- read_csv("C:/Users/DELL/Downloads/Iris.csv")

dim(Iris)
View(Iris)
summary(Iris)
head(Iris)
table(irisCluster$cluster, Iris$Species)

 #     Iris-setosa Iris-versicolor Iris-virginica
 #1           0              48              4
 #2           0               2             46
 #3          50               0              0

irisCluster$cluster <- as.factor(irisCluster$cluster)

# Plots for Clusters
 
ggplot(Iris, aes(PetalLengthCm, PetalWidthCm, color = irisCluster$cluster)) + geom_point()

ggplot(Iris,aes(SepalLengthCm, PetalWidthCm, color=Species)) + 
  +     geom_point(size = 8) + 
  +     scale_color_manual(values = c('#FF3EA5FF','#EDFF00FF','#00A4CCFF')) + 
  +     theme(legend.position= "none") + 
  +     theme_minimal() +
  +     theme(text=element_text(size=15,  family="sans"))


ggplot(Iris,aes(PetalWidthCm, PetalLengthCm, color=Species)) + 
  +     geom_point(size = 8) + 
  +     scale_color_manual(values = c('#FF3EA5FF','#EDFF00FF','#00A4CCFF')) + 
  +     theme(legend.position= "none") + 
  +     theme_minimal() +
  +     theme(text=element_text(size=15,  family="sans"))

ggplot(Iris,aes(SepalWidthCm, PetalWidthCm, color=Species)) + 
  +     geom_point(size = 8) + 
  +     scale_color_manual(values = c('#FF3EA5FF','#EDFF00FF','#00A4CCFF')) + 
  +     theme(legend.position= "none") + 
  +     theme_minimal() +
  +     theme(text=element_text(size=15,  family="sans"))

#Removing column of Id and Species from Iris

Iris_1<-Iris[-5]

Iris_1<-Iris_1[-1]
set.seed(240)  
kmeans.re <- kmeans(iris_1, centers = 3, nstart = 20) 
kmeans.re$cluster 

#[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#[38] 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#[75] 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 3 3 3 2 3 3 3 3
#[112] 3 3 2 2 3 3 3 3 2 3 2 3 2 3 3 2 2 3 3 3 3 3 2 3 3 3 3 2 3 3 3 2 3 3 3 2 3
#[149] 3 2

#Confusion Matrix

cm <- table(iris$Species, kmeans.re$cluster) 

#            1  2  3
#setosa     50  0  0
#versicolor  0 48  2
#virginica   0 14 36

plot(iris_1[c("SepalLengthCm", "SepalWidthCm")],  
     +      col = kmeans.re$cluster,  
     +      main = "K-means with 3 clusters")

## Visualizing clusters 

y_kmeans <- kmeans.re$cluster 

clusplot(iris_1[, c("SepalLengthCm", "SepalWidthCm")], 
           +          y_kmeans, 
           +          lines = 0, 
           +          shade = TRUE, 
           +          color = TRUE, 
           +          labels = 2, 
           +          plotchar = FALSE, 
           +          span = TRUE, 
           +          main = paste("Cluster iris"), 
           +          xlab = 'Sepal.Length', 
           +          ylab = 'Sepal.Width') 

kmeans.re

#Model kmeans.re
#K-means clustering with 3 clusters of sizes 50, 62, 38

#Cluster means:
#  SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm
#1      5.006000     3.418000      1.464000     0.244000
#2      5.901613     2.748387      4.393548     1.433871

##Clustering vector:

#[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#[38] 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#[75] 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 3 3 3 2 3 3 3 3
#[112] 3 3 2 2 3 3 3 3 2 3 2 3 2 3 3 2 2 3 3 3 3 3 2 3 3 3 3 2 3 3 3 2 3 3 3 2 3
#[149] 3 2

#Within cluster sum of squares by cluster:
#[1] 15.24040 39.82097 23.87947
#(between_SS / total_SS =  88.4 %)

#Available components:
  
#[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#[6] "betweenss"    "size"         "iter"         "ifault"  

print(cm)

#            1  2  3
#setosa     50  0  0
#versicolor  0 48  2
#virginica   0 14 36

