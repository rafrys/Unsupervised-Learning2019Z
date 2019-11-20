#Installing and loading packages

requiredPackages = c("tidyverse","factoextra","stats","clustertend","flexclust","ggforce"
                     ,"fpc","cluster","ClusterR","knitr","kableExtra","DataExplorer","reshape2",
                     "mclust","dbscan") 
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)} 
for(i in requiredPackages){library(i,character.only = TRUE) } 

## Steps of analysis
#1 Desriptive statistics 
#k-means clustering
# description of each cluster type characteristics
# ideas of how we can utilize insights from the analysis (e.g. marekting stratgies to apply to each group)

# Loading data
data_full <- read.csv("Dataset/CC GENERAL.csv",
                      stringsAsFactors = F)

# Tables with descriptive statistics
kable(t(summary(data_full)),caption = "Table 1. Summary statistics of the dataset")%>% 
  kable_styling(latex_options="scale_down",bootstrap_options = c("striped", "hover"))

plot_missing(data_full,title="% of NA values in the dataset")

# 313 observations are have NAs, we are dropping them

nrow(data_full)-length(which(data_full$TENURE<12))

data = data_full %>% 
        select(-CUST_ID) %>% 
        drop_na() %>% 
        filter(., TENURE==12) %>% 
        select(-TENURE) 

##Histograms to visualize the data

#Before transformation
gather(data) %>% 
  ggplot(., aes(value)) + 
  geom_histogram(aes(y =..density..), 
                 col="black", 
                 fill="blue", 
                 alpha=.2) + 
  geom_density(col="darkblue")+
  labs(title="Graph 2. Histograms of variables")+
  facet_wrap(~key, scales = 'free')


transformed_var <- c("BALANCE", "CASH_ADVANCE","CASH_ADVANCE_TRX", "CREDIT_LIMIT", "INSTALLMENTS_PURCHASES", 
                     "MINIMUM_PAYMENTS", "ONEOFF_PURCHASES", "PAYMENTS","PURCHASES", "PURCHASES_TRX")
# Log transformation, adding 1 to instances of 0 
data <- data %>% mutate_at(vars(transformed_var), funs(log(1 + .)))

# After transformation 
gather(data) %>% 
  ggplot(., aes(value)) + 
  geom_histogram(aes(y =..density..), 
                 col="black", 
                 fill="blue", 
                 alpha=.2) + 
  geom_density(col="darkblue")+
  labs(title="Graph 3. Histograms of variables after log transformation")+
  facet_wrap(~key, scales = 'free')

# Duda-Hart test for whether a data set should be split into two clusters


test1 <- kmeans(scale(data),4) 
dudahart2(data,test1$cluster)


# K - means  - determining the optimal number of cluster
fviz_nbclust(scale(data), kmeans, method = "wss", k.max = 10)

# K - means clustering

km_fitted <- kmeans(scale(data), centers = 4)

# Visualizng the clustering using PCA
prcomp(scale(data)) %>% 
fviz_pca_ind(., geom = "point",habillage = km_fitted$cluster,alpha.ind=0.4)

# Assesing clustering quality
silhouette(km_fitted$cluster, dist(scale(data), 
                                 method = "euclidean"), lable = FALSE) %>% 
        fviz_silhouette(., print.summary = FALSE)

data_clustered <- data
data_clustered$cluster = km_fitted$cluster


# Descriptive Statistics in regards to assigned clustersu

data_clustered %>% 
  group_by(cluster) %>% 
  summarise_all(mean) %>% 
  as.data.frame() %>%
  kable(digits = 2, caption = "Mean") %>%
  kable_styling(font_size = 9,bootstrap_options = c("striped", "hover"),latex_options=c("HOLD_position","scale_down"))

data_clustered %>% 
  group_by(cluster) %>% 
  summarise_all(median) %>% 
  as.data.frame() %>%
  kable(digits = 2, caption = "Median") %>%
  kable_styling(font_size = 9,bootstrap_options = c("striped", "hover"),latex_options="scale_down")

data_clustered %>% 
  group_by(cluster) %>% 
  summarise_all(sd) %>% 
  as.data.frame() %>%
  kable(digits = 2, caption = "Standard Deviation") %>%
  kable_styling(font_size = 9,bootstrap_options = c("striped", "hover"),latex_options="scale_down")

