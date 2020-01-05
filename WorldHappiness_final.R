###################################################
#\begin{enumerate}
#\item Introduction
#\item Data Extraction
#\item In-depth Analysis of the Data
#\item Using clustering method for analysis
#\item Conclusion
#\end{enumerate}
###################################################
#\pagebreak
##################################################
#1. **Introduction**:
#  
#  Analysing the data using different algorithms used as part of the course.
#
#The happiness scores and rankings use data from the Gallup World Poll. The scores are based on answers to the main life evaluation question asked in the poll. This question, known as the Cantril ladder, asks respondents to think of a ladder with the best possible life for them being a 10 and the worst possible life being a 0 and to rate their own current lives on that scale. The scores are from nationally representative samples for the years 2013-2016 and use the Gallup weights to make the estimates representative. The columns following the happiness score estimate the extent to which each of six factors – economic production, social support, life expectancy, freedom, absence of corruption, and generosity – contribute to making life evaluations higher in each country than they are in Dystopia, a hypothetical country that has values equal to the world’s lowest national averages for each of the six factors. They have no impact on the total score reported for each country, but they do explain why some countries rank higher than others. 
#
#We are trying to analyse the data to see what factors depends on the happiness of the people in the country based on the details provided.
###################################################
###################################################
#2. ***Data Extraction***:
#  
#  **Pre-requisites: Loading Packages and Libraries**
#  
#  First, we load all the necessary packages and libraries.
###################################################
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")


library(dplyr)
library(tidyr)
library(factoextra)
library(tidyverse)
library(cluster)
library(caret)
library(data.table)


#Using the file downloaded from https://www.kaggle.com/unsdsn/world-happiness

###################################################
#**Data Extraction**:
#  
#  Data set contains 156 observations with 9 different columns as mentioned below:
#  
#  \begin{enumerate}
#\item Overallrank
#\item Country
#\item Score
#\item GDPCapita
#\item Socialsupport
#\item Healthylifeexpectancy
#\item Freedomtomakelifechoices
#\item Generosity
#\item Perceptionsofcorruption
#\end{enumerate}
###################################################
#Reading data file
world_happiness <- (read.csv("./Data_files/2019.csv", header=TRUE))
#Viewing the first 6 records of the file                              
head(world_happiness)

###################################################
#3. **In-Depth analysis of data**
###################################################  
#  **Perceptionsofcorruption**
###################################################  
#Below graph shows relation between rank and corruption. From graph we can see,
#
#a. Some countries having good ranks though they have high corruption
#
#b. some countries have poor ranking though they have corruption less than mean.
#
#For this we can say that rank is not depending on corruption.

#viewing the Perceptionsofcorruption
world_happiness  %>% ggplot(aes(Overallrank,Perceptionsofcorruption)) +  geom_point(stat="identity") +
  coord_flip() + geom_hline(aes(yintercept = mean(Perceptionsofcorruption)), col="red", lwd=2, lty=2) +
  theme(axis.text.y = element_text(size = 10)) +
  xlab("")

#\pagebreak
###################################################
#**Freedomtomakelifechoices**
###################################################  
#Below graph shows relation between rank and Freedom. From graph we can see,
#
#a. Some countries having good ranks though they have high Freedom
#
#b. some countries have poor ranking though they have Freedom less than mean.
#
#For this we can say that rank is not depending on Freedom


#viewing the Freedomtomakelifechoices
world_happiness  %>% ggplot(aes(Overallrank,Freedomtomakelifechoices)) +  geom_point(stat="identity") +
  coord_flip() + geom_hline(aes(yintercept = mean(Freedomtomakelifechoices)), col="red", lwd=2, lty=2) +
  theme(axis.text.y = element_text(size = 10)) +
  xlab("")

#\pagebreak
################################################### 
#**GDPpercapita**
###################################################   
#Below graph shows relation between rank and GDP. From graph we can see,
#
#a. countries which are having good GDP greater than mean are having good rankings
#
#b. Countries which are having less GDP less than mean are having high rankings
#
#For this we can say that rank is depends on GDP per capita

#viewing GDPpercapita
world_happiness %>% ggplot(aes(Overallrank,GDPpercapita)) +  geom_point(stat="identity") +
  coord_flip() + geom_hline(aes(yintercept = mean(GDPpercapita)), col="red", lwd=2, lty=2) +
  theme(axis.text.y = element_text(size = 10)) +
  xlab("")

#\pagebreak
################################################### 
#**Socialsupport**
###################################################  
#Below graph shows relation between rank and Social Support. From graph we can see,
#
#a. countries which are having good social support greater than mean are having good rankings
#
#b. Countries which are having less social support less than mean are having high rankings
#
#For this we can say that rank is depends on social support

#viewing Socialsupport
world_happiness %>% ggplot(aes(Overallrank,Socialsupport)) +  geom_point(stat="identity") +
  coord_flip() + geom_hline(aes(yintercept = mean(Socialsupport)), col="red", lwd=2, lty=2) +
  theme(axis.text.y = element_text(size = 10)) +
  xlab("")

#\pagebreak
###################################################
#**Healthylifeexpectancy**
###################################################  
# Below graph shows relation between rank and Healthy Life. From graph we can see,
#
#a. countries which are having good healthy life greater than mean are having good rankings
#
#b. Countries which are having less healthy life less than mean are having high rankings
#
#For this we can say that rank is depends on healthy life

#viewing Healthylifeexpectancy
world_happiness %>% ggplot(aes(Overallrank,Healthylifeexpectancy)) +  geom_point(stat="identity") +
  coord_flip() + geom_hline(aes(yintercept = mean(Healthylifeexpectancy)), col="red", lwd=2, lty=2) +
  theme(axis.text.y = element_text(size = 10)) +
  xlab("")

#\pagebreak
###################################################
#**Generosity**
################################################### 
#  Below graph shows relation between rank and Generosity. From graph we can see,
#
#a. Some countries having good ranks though they have high generosity
#
#b. some countries have poor ranking though they have generosity less than mean.
#
#For this we can say that rank is not depending on generosity

#viewing Generosity
world_happiness %>% ggplot(aes(Overallrank,Generosity)) +  geom_point(stat="identity") +
  coord_flip() + geom_hline(aes(yintercept = mean(Generosity)), col="red", lwd=2, lty=2) +
  theme(axis.text.y = element_text(size = 10)) +
  xlab("")

#\pagebreak
################################################### 
#4. **Clustering:**
###################################################  
#  The purpose of clustering analysis is to identify patterns in data and create groups according to those patterns. Therefore, if two points have similar characteristics, that means they have the same pattern and consequently, they belong to the same group. By doing clustering analysis we should be able to check what features usually appear together and see what characterizes a group.
#
#The bigger is the K is choosed, the lower will be the variance within the groups in the clustering. If K is equal to the number of observations, then each point will be a group and the variance will be 0. It’s interesting to find a balance between the number of groups and their variance. A variance of a group means how different the members of the group are. The bigger is the variance, the bigger is the dissimilarity in a group.

set.seed(1)
#creating inut file for clustering
input <- world_happiness[,4:9]
#checking the details using number of centers as 3
kmeans(input, centers = 3, nstart = 20)


#\pagebreak
###################################################
#**Within Sum of Squares(WSS) Plot:**
###################################################  
#  The function below plots a chart showing the “within sum of squares” (withinss) by the number of groups (K value) chosen for several executions of the algorithm. The within sum of squares is a metric that shows how dissimilar are the members of a group., the greater is the sum, the greater is the dissimilarity within a group.

wssplot <- function(data, nc=15, seed=1){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(input, nc = 25)

#\pagebreak
################################################### 
#**K-Means**
###################################################   
#  By Analysing the chart from right to left, we can see that when the number of groups (K) reduces from 4 to 3 there is a big increase in the sum of squares. That means that when it passes from 4 to 3 groups there is a reduction in the clustering compactness. Our goal, however, is not to achieve compactness of 100% — for that, we would just take each observation as a group. The main purpose is to find a fair number of groups that could explain satisfactorily a considerable part of the data.
#
#Using 3 groups (K = 3) we had 68.8% of well-grouped data. Using 4 groups (K = 4) that value raised to 78.3%, which is a good value for us.


set.seed(1)
clustering <- kmeans(input, centers = 4, nstart = 20)
clustering

#\pagebreak

#Using silhouette coefficient (silhouette width) to evaluate the goodness of our clustering.

sil <- silhouette(clustering$cluster, dist(input))
fviz_silhouette(sil)

################################################### 
#5. **Conclusion:**
###################################################   
#  k-means is a pretty good clustering algorithm. But, it has some drawbacks. The biggest disadvantage is that it requires us to pre-specify the number of clusters (k). Hierarchical clustering is an alternative approach that does not require a particular choice of clusters. An additional disadvantage of k-means is that it is sensitive to outliers and different results can occur if you change the ordering of the data.K-means requires a possibly large amount of memory to store the data, and each request involves starting the identification of a local model from scratch.
