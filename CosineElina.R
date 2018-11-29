source('ReadInTextFile.R')
vsize <- function(v) sqrt(sum(v^2))

cosine = function(A, B){
  A_len = sqrt(sum(A ^ 2))
  B_len = sqrt(sum(B ^ 2))
  AB_dot = A %*% B
  cos = AB_dot[1, 1] / (A_len * B_len)
  if (is.nan(cos)){
    return(0)
  }
  return(cos)
}

minkowsky<- function(element1,element2, p){
  
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

euc = function(a, b){
  return(minkowsky(a, b, 2))
}

# http://www.wolframalpha.com/input/?i=CosineDistance%5B%7B1,+2,+3%7D,+%7B3,+5,+7%7D%5D
# CosineDistance[{1, 2, 3}, {3, 5, 7}] = 0.0025851

# C vectors transposed have same dimensions
a = matrix(c(2, 0, 3, 4,6), nrow = 1)
b = matrix(c(3, 1, 7, 5, 6), nrow = 1)

cosineTest = cosine(a, b)
print(cosineTest)

euclideanTest = minkowsky(a,b,2)
print(euclideanTest)

docTermMatrix = as.matrix(dtms)

first = docTermMatrix[1,]  # single document
distance_matrix = matrix(nrow=nrow(docTermMatrix), ncol=2)

for (index in 1:nrow(docTermMatrix)) {  # comapred to all others
  doc = c(docTermMatrix[index,])
  dist = cosine(first, doc)
  # dist = minkowsky(first, doc, 2)
  
  if (!is.na(dist)){
    distance_matrix[index, 1] = index
    distance_matrix[index, 2] = dist
  }
}

distance_matrix = distance_matrix[order(distance_matrix[,2]),]

wf <- data.frame(word=1:nrow(distance_matrix), freq=distance_matrix[,2])
p <- ggplot(wf, aes(word, freq))

p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
print(p) # needed to plot

