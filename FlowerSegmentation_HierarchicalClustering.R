#Greyscale images represented as a matrix of pixel intensity values ranging from 0 (black) to 1 (white)

#For 8 bits/pixel (bpp), 256 color levels. Number of columns corresponds to the width of the image, number of rows corresponds to the heighr of the image.

#This is called an intensity matrix. For eg, a 7 by 7 matrix will have 49 values between 0 and 1. What would be input to clustering algo. There is only one variable, which is the pixel intensity, and it has 49 values. So the input to cluster will be a vector with 49 values.

# So we'll have to convert matrix to vector using as.vector()


#Using hierarchical clustering
#First step is to calculate distance matrix. For each element in the intesnsity vector , we need to calculate its distance from the 48 other elements. so 48 calculations per element.
# total 49*48. But due to symmetry, only half need to be calculated


# for a flower dataset

flower= read.csv("flower.csv",header=FALSE)
flowerMatrix= as.matrix(flower)
str(flowerMatrix) #50 pixel width, 50 pixel height
#convert to vector
flowerVector= as.vector(flowerMatrix) 
#Can't convert data frame to vector directly because treats each row as 1 vector

#Next step to create the distance matrix
distance= dist(flowerVector,method="euclidean")

#next step is to compute hierarchical cluster

clusterIntensity= hclust(distance, method="ward.D") #ward is a min variance method, which tries to find compact and spherical clusters. We can think about it as trying to minimize variance within each cluster adn the distance among cluseters 
plot(clusterIntensity) #makes dendogram. Vertical lines represent distance from the cluster. Usually, choose number of clusters abased on how much space there is for a horizontal line to move up and down
rect.hclust(clusterIntensity,k=3,border="red") #drawing separation on dendogram. k=3

flowerClusters= cutree(clusterIntensity,k=3) # cuts tree into as many clusters we want
flowerClusters #displays a vector with values 1,2,3 for each cluster
tapply(flowerVector,flowerClusters,mean) # mean value in each cluster
dim(flowerClusters)= c(50,50) #convert it into matrix
image(flowerClusters,axes=FALSE) #displays image
image(flowerMatrix,axes=FALSE, col=grey(seq(0,1,length=256))) #Displays original image

