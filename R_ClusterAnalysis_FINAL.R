rm(list = ls())

library(readr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering and visualization
library(tidyverse)  # data manipulation

#import data
  df <- read_delim("2ndCities_RStudio_2019.csv", 
                 ";", escape_double = FALSE, trim_ws = TRUE)

df$SG2 <- as.numeric(gsub(df$SG2, pattern = ",", replacement = "."))
df$SG3 <- as.numeric(gsub(df$SG3, pattern = ",", replacement = "."))
df$SG4 <- as.numeric(gsub(df$SG4, pattern = ",", replacement = "."))
df$SG5 <- as.numeric(gsub(df$SG5, pattern = ",", replacement = "."))
df$SG6 <- as.numeric(gsub(df$SG6, pattern = ",", replacement = "."))
df$SG7 <- as.numeric(gsub(df$SG7, pattern = ",", replacement = "."))
df$SG8 <- as.numeric(gsub(df$SG8, pattern = ",", replacement = "."))

NAME <- df$NAME
df <- df[,-1]
row.names(df) <- NAME

distance <- get_dist(df)
fviz_dist(distance)

df <- scale(df)

#start clustering

k5 <- kmeans(df, centers = 3, nstart = 25)
p4 <- fviz_cluster(k5, geom = "point",  data = df, labelsize = 5) + ggtitle("k = 3")
p4

str(k5)
k5

set.seed(123)
final <- kmeans(df, 3, nstart = 25)
print(final)
fviz_cluster(final, data = df, labelsize = 9)

#determine ideal number of clusters

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")

set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#again
k5 <- kmeans(df, centers = 5, nstart = 25)
p4 <- fviz_cluster(k5, geom = "point",  data = df, labelsize = 5) + ggtitle("k = 5")
p4

set.seed(123)
final <- kmeans(df, 3, nstart = 25)
print(final)
fviz_cluster(final, data = df, labelsize = 5)

summary(k5)
summary(df)             

#Examinijg the cluster https://www.guru99.com/r-k-means-clustering.html

pc_cluster_Z <- kmeans(df,3)
center <- pc_cluster_Z$centers
cluster <- c(1:3)
center_df <- data.frame(cluster, center)

center_reshape <- gather(center_df, features, values, SG2:SG8)
head(center_reshape)

library(RColorBrewer)
# Create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

# Plot the heat map
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()

#check silhouette width for cluster validation

head(final$cluster, 10)

sil <- silhouette(final$cluster, dist(df))

fviz_silhouette(sil)

neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]


#PCA analysis (isolated)

df_pca <- prcomp(df, center=TRUE, scale. = TRUE)
summary(df_pca)




