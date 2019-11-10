#Installing and loading packages

requiredPackages = c("tidyverse","factoextra","stats","clustertend","flexclust","fpc","cluster","ClusterR") # list of required packages
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)} 
for(i in requiredPackages){library(i,character.only = TRUE) } 

## Steps of analysis
#1 Desriptive statistics 
#k-means clustering
# description of each cluster type characteristics
# ideas of how we can utilize insights from the analysis (e.g. marekting stratgies to apply to each group)

# Loading data
data_full <- read.csv("/Users/rafalelpassion/Unsupervised-Learning2019Z/Dataset/AB_NYC_2019.csv")

# columns of intrest 
columns <- c("neighbourhood_group","neighbourhood","latitude","longitude","room_type","price","minimum_nights","number_of_reviews")
data <- subset(data_full,neighbourhood="",select=columns)
data$neighbourhood_group <- as.factor(data$neighbourhood_group)
data$neighbourhood <- as.factor(data$neighbourhood)
data$room_type <- as.factor(data$room_type)

# Price Bins 
data$price_tier <- ifelse(data$price<=100,1,ifelse(data$price<=250,2,3))
table(data$price_tier)

#Looking fo n/a's
any(is.na(data))

barplot(data$price)
table(data$room_type)


hist(data$price,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)


