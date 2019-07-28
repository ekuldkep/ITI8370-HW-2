library(fpc)
library(cluster)
library(class)

# get the parsed data
source('ReadInTextFile.R')


# https://stackoverflow.com/questions/39822505/drawing-decision-boundaries-in-r
# http://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html

PCA = function(data) {
  #take mean of each column
  M = colMeans(data)
  #centering columns by subtracting means
  C = data - M
  #make covariance matrix
  V = cov(C)
  #make new object with calculated eigenvectors and its aigenalues
  eigenObject = eigen(V)
  eigenVectors = eigenObject$vectors
  eigenValues = eigenObject$values
  #multiplay initial data matrix with eigenvectors matrix
  P = data %*% eigenVectors
  return(P)  
}

data = PCA(as.table(dtms))[, 1:2]
labels = spam

train.df <- data

# class labels: simple distance from origin
classes <- labels
res = 8
size = (-5*res):(5*res) / res
row_count = length(size)

# create grid for knn
grid <- expand.grid(x=size, y=-size)

classes.grid <- knn(train.df, grid, classes, k=7, prob=TRUE) 
classes.grid = as.integer(classes.grid)+1L

z = matrix(as.integer(classes.grid), nrow=row_count)
test_z = matrix(z, nrow=row_count)

for (row in 2:(row_count - 1)) {
  for (col in 2:row_count - 1) {
    center = test_z[row,col]
    UP = test_z[row - 1,col]
    DOWN = test_z[row + 1,col]
    LEFT = test_z[row, col - 1]
    RIGTH = test_z[row, col + 1]
    
    if (var(c(center, UP, DOWN, LEFT, RIGTH)) != 0){
      classes.grid[row + col * row_count] = 1
    }
  }
}

print("grid")
print(z)

plot.new()
plot.window( xlim=c(-5,5), ylim=c(-5,5) )

# contour(x=size, y=size, z=z, col="grey", drawlabels=TRUE, lwd=2)

points(grid, col = classes.grid, pch = ".", cex=2)
points(train.df, col=classes + 2, pch = 16, type = "p")

# might want doc
# text(data, labels=rownames(data), cex= 0.7, pos=3)
