library(foreach)
library(dplyr)
library(gridExtra)
library(doParallel)
library(Rcpp)


## Similarity score calculation with similarity function in C++

# Source CPP similarity function
sourceCpp("lab3_C.cpp")

# Read in lingBinary data
# Subset 10000 data to run on local machine due to limited nodes available on server
load("lingBinary.RData")
X<-lingBinary[,7:ncol(lingBinary)]

# Initialization of vectors
Similarity_loop <- c()
Similarity_result <- list()

# Set sampling ratio to be 0.5
# Set maximum number of k - number of clusters to be 10
kmax <- 10
m <- round(0.5*nrow(X),0)

# Register the parallel backend with the foreach package
# registerDoParallel(ncores)

# Record the total time spent for running similarity score
start.time <- Sys.time()

# Iterate the similarity score calculation for i times
# Also iterate over k - the number of cluster centers from 2 to 10
Similarity_output_C_full <- foreach (k = 2:10) %dopar% {
  for (i in 1:100){
    # First randomly sample the row number 
    # and subset observations from the matrix by specifying the randomly selected row number
    index1<- sample(nrow(X), m)
    sub1 <- X[index1, ]
    # Second randomly sample the row number again
    # and obtain the second subset of observations from the matrix by specifying the randomly slected row number
    index2 <- sample(nrow(X), m)
    sub2 <- X[index2, ]
    # Find the intersect between index1 and index2
    intersect <- intersect(index1, index2)
    # Run kmeans on the first subset 
    # Find the clustering result on the first subset data sub1 for the intersect index
    L1 <- kmeans(x=sub1,centers=k)
    match1<-data.frame(index1,L1$cluster)
    cluster_index1<-match1[match1$index1 %in% intersect,]$L1.cluster
    # Run kmeans on the second subset 
    # Find the clustering result on the second subset data sub2 for the intersect index
    L2 <- kmeans(sub2,k)
    match2<-data.frame(index2,L2$cluster)
    cluster_index2<-match2[match2$index2 %in% intersect,]$L2.cluster
    # Apply similarity function to calculate the similarity score between the two vectors of indices
    Similarity_score<-SimilarityCplus(cluster_index1,cluster_index2)
    # Save all the iterations of similarity score calculation to Similarity_loop
    Similarity_loop[i] <- Similarity_score
  }
  # Save all the similarity score to Similarity_result
  Similarity_result[[k-1]] <- Similarity_loop
}  

Similarity_output_C_full

# Total time spent for running the similarity score
Duration_C <- Sys.time() - start.time
Duration_C
# Total running time is 7 hours

# Write the output into csv file
write.csv(Similarity_output_C,"Similarity_output_C.csv")