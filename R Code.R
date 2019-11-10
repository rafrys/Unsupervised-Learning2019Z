#Installing and loading packages

requiredPackages = c("tidyverse","factoextra","stats","clusterend","flexclust","fpc","cluster","clusterR") # list of required packages
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)} 
for(i in requiredPackages){library(i,character.only = TRUE) } 

## Steps of analysis
#1 Desriptive statistics 
#k-means clustering