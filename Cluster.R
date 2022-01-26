# Packages
install.packages("factoextra")
require(factoextra)
require(purrr)

pharma.df <- read.csv("Pharmaceuticals.csv")
colnames(pharma.df)

pharma.df <- pharma.df[-(1:2)]

# normalize data
pharma.df.norm <- sapply(pharma.df[1:9], scale)

# hierarchical cluster analysis
dist.norm <- dist(pharma.df.norm , method = "euclidean")
hc1 <- hclust(dist.norm, method = "ward.D")

plot(hc1, hang = -1, ann = FALSE)
memb <- cutree(hc1, k = 2) # Tried different k's

# clusters statistics
centers <- aggregate( . ~ memb, data = pharma.df[1:9], FUN = mean)
centers

# Assess stability by removing 5% of the records and re-evaluating the clusters
set.seed(1)
remove.ind <- sample(1:length(memb), length(memb)*0.05)
dist.norm <- dist(pharma.df.norm[-remove.ind,], method = "euclidean")

hc2 <- hclust(dist.norm, method = "ward.D")
plot(hc2, hang = -1, ann = FALSE)
memb2 <- cutree(hc2, k = 2) # 2 clusters  

# clusters statistics
centers <- aggregate( . ~ memb, data = pharma.df[1:9], FUN = mean)
centers

# kmeans algorithm
km <- kmeans(pharma.df.norm, 2) 

km$cluster

centers.km <- aggregate( . ~ km$cluster, data = pharma.df[1:9], FUN = mean)
centers.km

km$size 

# compare membership between hierarchical and kmeans
table(memb, km$cluster)

fviz_cluster(km, geom = "point", data=pharma.df.norm)

### Examining the elbow for k
# function to compute total within-cluster sum of sq
wss <- function(k) 
{
  kmeans(pharma.df.norm, k)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 8 Can Chose K
k.values <- 1:8

#extracts wss for 2 - 8 clusters (wss - Within Sum of Square)
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#summary of catergorical
table(pharma.df$Median_Recommendation, memb)
table(pharma.df$Location, memb)
table(pharma.df$Exchange, memb)
