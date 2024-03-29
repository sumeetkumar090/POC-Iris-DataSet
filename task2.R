#Task2  Sk521 - Sumeet Kumar
# install packages, if necessary

library(ggplot2)
theme_set(theme_bw(base_size=12)) # set default ggplot2 theme
library(dplyr)

library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)
library(dendextend)
library(colorspace)


# Iris dataset
data(iris)

#1. Describe your observation on this dataset such as the number of examples, the number of features, and the
#meaning of these features. You shall also use summary() function to help you gain more understanding.
#display the result
summary(iris)
#View(iris)
names(iris)

#2. Plot the scatterplot matrix of Iris dataset to visualize the pairwise relationships between the four attributes

# define the colors
colors <- c("red", "green", "blue")

# draw the plot matrix
pairs(iris[1:4], main = "Fisher's Iris Dataset",
      pch = 21, bg = colors[unclass(iris$Species)])

# set graphical parameter to clip plotting to the figure region
par(xpd = TRUE)

# add legend
legend(0.2, 0.02, horiz = TRUE, as.vector(unique(iris$Species)),
       fill = colors, bty = "n")


#3.Perform K-means clustering analysis on the Iris dataset and report your result. Explain how you choose the number of clusters and justify your choice.

iris_numeric <- select(iris, -Species) # make a data table with only the numeric measurements from iris
wss <- (nrow(iris_numeric)-1)*sum(apply(iris_numeric,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(iris_numeric,
                                     nstart=25,
                                     centers=i)$withinss)

wss_data <- data.frame(centers=1:15, wss)
ggplot(wss_data, aes(x=centers, y=wss)) + geom_point() + geom_line() +
  xlab("Number of Clusters") + ylab("Within groups sum of squares")


#3. Kmean Clustering
data_input = as.data.frame(iris)
kmdata_orig = as.matrix(data_input[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")])
kmdata <- kmdata_orig[,1:4]
kmdata[1:10,]

km = kmeans(kmdata,3, nstart=25)
km


#4. prepare the iris data and clustering results for plotting
df = as.data.frame(iris)
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)

g1=ggplot(data=df, aes(x=Sepal.Length, y=Sepal.Width, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=Sepal.Length,y=Sepal.Width, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g2 =ggplot(data=df, aes(x=Sepal.Length, y=Petal.Length, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=Sepal.Length,y=Petal.Length, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g3 = ggplot(data=df, aes(x=Sepal.Length, y=Petal.Width, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=Sepal.Length,y=Petal.Width, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g4 = ggplot(data=df, aes(x=Sepal.Width, y=Petal.Length, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=Sepal.Width,y=Petal.Length, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g5 = ggplot(data=df, aes(x=Sepal.Width, y=Petal.Width, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=Sepal.Width,y=Petal.Width, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g6 = ggplot(data=df, aes(x=Petal.Length, y=Petal.Width, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=Petal.Length,y=Petal.Width, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)



grid.arrange(arrangeGrob(g1 + theme(legend.position="none"),
                         g2 + theme(legend.position="none"),
                         g3 + theme(legend.position="none"),
                         g4 + theme(legend.position="none"),
                         g5 + theme(legend.position="none"),
                         g6 + theme(legend.position="none"),
                         top ="IRIS Dataset Cluster Analysis", ncol=2, nrow = 4))



#5. Learn to perform hierarchical agglomerative clustering via hclust()
#function and compare the clustering result with that obtained with K-means clustering.
#Ref used: https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html

d_iris <- dist(iris) # method="man" # is a bit better
hc_iris <- hclust(d_iris, method = "complete")
iris_species <- rev(levels(iris[,5]))


dend <- as.dendrogram(hc_iris)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=3) #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(iris[,5])[order.dendrogram(dend)]
  )]

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(iris[,5])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered Iris data set
     (the labels give the true flower species)", 
     horiz =  TRUE,  nodePar = list(cex = .007))
legend("topleft", legend = iris_species, fill = rainbow_hcl(3))