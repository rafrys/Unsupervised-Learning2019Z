#Installing and loading packages

requiredPackages = c("tidyverse","factoextra","stats","clustertend","flexclust",
                     "fpc","cluster","ClusterR","knitr","kableExtra") 
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)} 
for(i in requiredPackages){library(i,character.only = TRUE) } 

## Steps of analysis
#1 Desriptive statistics 
#k-means clustering
# description of each cluster type characteristics
# ideas of how we can utilize insights from the analysis (e.g. marekting stratgies to apply to each group)

# Loading data
data_full <- read.csv("/Users/rafalelpassion/Unsupervised-Learning2019Z/Dataset/CC GENERAL.csv")
data_full$CUST_ID <- NULL

# Missing values
kable(summary(data_full),caption = "Summary statistics of the dataset")%>% 
        kable_styling(latex_options="scale_down")


# 313 observations are have NAs, we are dropping them
data_full <- data_full[complete.cases(data_full), ]

# Leaving only observations which have 12 months of tenure      
nrow(data_full)-length(which(data_full$TENURE<12))
data_full %>% select(filter(.,TENURE==12))

class(data_full)

# Outliers treatment 
columns <-c("BALANCE","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE","CREDIT_LIMIT",
        "PAYMENTS","MINIMUM_PAYMENTS")

as.data.frame(data_full) %>%
        gather() %>%                           
        ggplot(aes(value)) +                    
        facet_wrap(~ key, scales = "free") +  
        geom_density() +                      
        theme(strip.text = element_text(size=5))

df <- scale(data_full)


