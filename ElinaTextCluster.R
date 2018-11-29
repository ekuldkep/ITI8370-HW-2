library(fpc)
library(cluster)

# get the parsed data
source('ReadInTextFile.R')

kmeans <- function(data, cluster_count, distance, iterations=15){
  
  #dimensions count
  columnCount <- ncol(data)
  
  names = row.names(data)
  
  #define new empty matrix where columns count is dimensions count and row count is cluster count
  centroids <- matrix(, ncol=columnCount, nrow=cluster_count)
  
  #generates centroids coordinates by assigning x,y... to be each from random point
  for (clusterIndex in 1:cluster_count) {
    for (columIndex in 1:columnCount) {
      centroid_current_dimension_random_value <- sample(data[, columIndex], size = 1)
      centroids[clusterIndex, columIndex] = centroid_current_dimension_random_value
    }
  }
  
  #https://stackoverflow.com/a/19591226, http://r.789695.n4.nabble.com/help-to-add-a-new-column-filled-with-value-1-td3034361.html
  copied_data_matrix <- matrix(data, ncol=ncol(data))
  #add new column for cluster labels
  data_matrix_extra_column <- cbind(copied_data_matrix, rep(0,nrow(copied_data_matrix)))
  contains_cluster_column <-ncol(data_matrix_extra_column)
  
  #calculate distance and best cluster for each point
  for (simulate in 1:iterations){
    for (i in 1:nrow(data)){
      current_point <- data[i, ]
      best_distance <- Inf
      best_centroid <- NULL
      for (c in sample(1:nrow(centroids))){
        current_centroid <- centroids[c, ]
        d <- distance(current_point, current_centroid)
        if(d < best_distance){
          best_distance <- d
          best_centroid <- current_centroid
          data_matrix_extra_column[i, contains_cluster_column] =  c
        }
      }
    }
    
    # calculate new centroids
    old_centroids = matrix(centroids, ncol=ncol(centroids))
    for (c in 1:nrow(centroids)){
      # a list of indexes where value == c
      mask <- data_matrix_extra_column[,contains_cluster_column] == c
      # select all rows which index is in mask,
      # select all columns, except the last one, which is the c label
      currentClusterPoints <- data_matrix_extra_column[mask,1:columnCount,drop=FALSE]
      # calculate the new mean for this cluster points
      new_mean = colMeans(currentClusterPoints)
      # as some points might not have any points at all, assign them some random one
      claimed_points_count = nrow(currentClusterPoints)
      if (claimed_points_count != 0) {
        centroids[c, ] = new_mean
      }
      if (claimed_points_count == 0 & simulate != iterations)
      {
        # assign random point
        centroids[c, ] = data[c * simulate, ]
      }
    }
    
    difference = old_centroids - centroids
    
    if (sum(abs(difference))<0.2){
      break
    }
  }
  
  if(columnCount < 3){
    for (i in 1:nrow(data_matrix_extra_column)) {
      c <- data_matrix_extra_column[i, contains_cluster_column]
      a <- switch(c, "red", "blue", "purple", "yellow", "green", "orange", "cyan", "coral", "chartreuse4", "aquamarine4", "aquamarine2")
      plot(data_matrix_extra_column[i, 1], data_matrix_extra_column[i, 2], col = a, type = "p", xlim = c(- 5, 5), ylim = c(- 5, 5))
      text(data_matrix_extra_column[i, 1], data_matrix_extra_column[i, 2], labels=names[i], cex= 0.7, pos=3)
      par(new = TRUE)
    }
    
    for (i in 1:cluster_count) {
      plot(centroids[i, 1], centroids[i, 2], col = "black", pch=17, type = "p", xlim = c(- 5, 5), ylim = c(- 5, 5))
      par(new = TRUE)
    }
    
  }
  par(new = FALSE)
  return(data_matrix_extra_column)
}

covariance <- function(data){
  dimension_count <- ncol(data)
  cov_matrix <- matrix( ,nrow = dimension_count, ncol = dimension_count)
  
  for (i in 1:dimension_count) {
    for (j in 1:dimension_count) {
      
      cov <- 0
      if (i == j){
        dim_mean <- mean(data[, i])
        col_vector <- data[, i]
        for (elem in col_vector) {
          value <- (elem - dim_mean)^2
          cov <- cov + value
        }
      }
      else
      {
        i_dim_mean <- mean(data[, i])
        j_dim_mean <- mean(data[, j])
        i_col_vector <- data[, i]
        j_col_vector <- data[, j]
        
        i_value <- 0
        for (r in 1:nrow(data)) {
          i_value <- i_col_vector[r] - i_dim_mean
          j_value <- j_col_vector[r] - j_dim_mean
          value <- i_value * j_value
          
          cov <- cov + value
        }
      }
      
      final <- cov / (nrow(data) - 1)
      
      cov_matrix[i, j] = final
    }
  }
  return (cov_matrix)
  
}

PCA = function(data) {
  #take mean of each column
  M = colMeans(data)
  #centering columns by subtracting means
  C = data - M
  #make covariance matrix
  V = covariance(C)
  #make new object with calculated eigenvectors and its aigenalues
  eigenObject = eigen(V)
  eigenVectors = eigenObject$vectors
  eigenValues = eigenObject$values
  #multiplay initial data matrix with eigenvectors matrix
  P = data %*% eigenVectors
  return(P)  
}

minkowsky = function(element1,element2, p){
  
  dimensions=length(element1)
  sqd<-matrix(,dimensions,1)
  
  if(p ==Inf){
    for(i in seq(along=element1)){
      sqd[i]<-abs(element1[i]-element2[i])
    }
    dist<-max(sqd)
  }
  
  else{
    summ <- 0
    for(i in seq(along=element1)){
      summ<- summ + abs(element1[i]-element2[i])^p
    }
    dist <-summ^(1/p)
  }
  return(dist)
}


cosine = function(A, B) {
  A_len = sqrt(sum(A ^ 2))
  B_len = sqrt(sum(B ^ 2))
  AB_dot = A %*% B
  cos = AB_dot[1, 1] / (A_len * B_len)
  if (is.nan(cos)){
    return(1)
  }
  return(1 - cos)
}

# Exercise 2. Clustering.
# Attempt to visualize centroids in the case of text data. (own implementation is required)

euc = function(a,b){
  return (minkowsky(a, b, 2))
} 

#take only two dimensions with the biggest eigenvalues
data = PCA(as.table(t(dtms)))[, 1:2]

result = kmeans((data), 3, euc, iterations = 50)
# result = kmeans((data), 3, cosine, iterations = 50)
