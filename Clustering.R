# Needed packages
require(factoextra)
require(purrr)

# load the data (3900 records of frequent fliers)
airlines.df <- read.csv("EastWestAirlinesCluster.csv")
colnames(airlines.df)

# normalize data
airlines.df.norm <- sapply(airlines.df[,-1], scale)

# hierarchical cluster analysis
dist.norm <- dist(airlines.df.norm, method = "euclidean")
hc1 <- hclust(dist.norm, method = "ward.D")

plot(hc1, hang = -1, ann = FALSE)
memb <- cutree(hc1, k = 2) # cut to 2 clusters. Also try a 3 and 5 cluster solution

# clusters statistics
centers <- aggregate( . ~ memb, data = airlines.df[,-1], FUN = mean)
centers

# Assess stability by removing 5% of the records and re-evaluating the clusters
set.seed(1)
remove.ind <- sample(1:length(memb), length(memb)*0.05)
dist.norm <- dist(airlines.df.norm[-remove.ind,], method = "euclidean")

hc2 <- hclust(dist.norm, method = "ward.D")
plot(hc2, hang = -1, ann = FALSE)
memb2 <- cutree(hc2, k = 2) # cut to 2 clusters.  Also try a 3 and 5 cluster solution

centers <- aggregate( . ~ memb2, data = airlines.df[-remove.ind,-1], FUN = mean)
centers

# kmeans
km <- kmeans(airlines.df.norm, 2) # also try a 3 and 5 cluster solution

km$cluster

centers.km <- aggregate( . ~ km$cluster, data = airlines.df[,-1], FUN = mean)
centers.km

km$size 

# compare membership between hierarchical and kmeans
table(memb, km$cluster)

fviz_cluster(km, geom = "point", data=airlines.df.norm)

### Examining the elbow for k

# function to compute total within-cluster sum of sq
wss <- function(k) 
  {
  kmeans(airlines.df.norm, k)$tot.withinss
  }

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

#extracts wss for 2 - 15 clusters (wss - Within Sum of Square)
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

