# segmenting brain images using K means clustering

# Data for healthy brain stored in healthy.csv

healthy=read.csv("healthy.csv",header=FALSE)
healthyMatrix=as.matrix(healthy)
str(healthyMatrix)
#566 by 646

image(healthyMatrix,axes=FALSE, col=grey(seq(0,1,length=256)))
healthyVector= as.vector(healthyMatrix)

#computing distance for hierarchical
distance= dist(healthyVector,method="euclidian")
#gives error as too huge. total number of values in brain pixels = 566*646 =365636
#so number of distances to be calculated n(n-1)/2 which is coming out huge

#so let's use k means instead

# let's use k mean clusters
k=5
set.seed(1) #randomly assigns so that's why to get same results everytime
KMC= kmeans(healthyVector,centers=k,iter.max=1000) #itermax to set number of iterations, otherwise might go on
str(KMC)
healthyClusters=KMC$cluster #has an attribute which gives mean values for each cluster. Don't need to manually calculate as in the case of Hierarchical
#mean intensity given in KMC attribute centers
#size of the cluster also given as KMC attribute
dim(healthyClusters)= c(nrow(healthyMatrix),ncol(healthyMatrix)) #to convert to matrix
image(healthyClusters,axes=FALSE,col=rainbow(k)) #multi color. each color represents a cluster

#Detecting a tumor called oligodendroglioma, which usually occurs in front lobe of brain
tumor=read.csv("tumor.csv",headers=FALSE)
tumorMatrix=as.matrix(tumor)
tumorVector=as.vector(tumorMatrix)
install.packages("flexclust") 
library(flexclust)
#package contains object class KCCA, K centroids cluster analysis. Need to convert information from the clustering algorithm to an object of the class KCCA before predicting
KMC.kcca= as.kcca(KMC, healthyVector) 
tumorClusters  = predict(KMC.kcca, newdata=tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix),ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col=rainbow(k))


