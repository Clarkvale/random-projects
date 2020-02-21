library(ggplot2)

euclide <- function(point1 = vector(), point2 = vector()){
  return(sqrt((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2))
}
#Takes the averages of the x and y coordinates and returns the centroid position. 
#This should be used after assignment
CenterCentroid <- function(x_coor,y_coor){
  positions <- matrix( nrow = 2, ncol = length(x_coor), dimnames = list(c('x','y')), byrow = TRUE)
  positions[1,] <- x_coor
  positions[2,] <- y_coor
  meanXY <- apply(positions, MARGIN = 1, mean)
  return(meanXY)
  
}

(CenterCentroid(c(-4,2), c(2,2)))
Centroid1_x <- c(-4,-4,-4,2,2,2)
Centroid1_y <- c(4,3,2,4,3,2)

(CenterCentroid(Centroid1_x,Centroid1_y))

#Q5 data
#coor array such that the values are paired (x,y). 
coor_array <- c(-1.5,-1,-1.5,1,-1.5,3,0,-1,0,1,0,3,1,-1,1,1,1,3,5,-1,5,1,5,3)
coor_matrix <- matrix(data = coor_array, nrow = 2, ncol = length(coor_array)/2, dimnames = list(c('x','y')))
centroid_array <- c(-4,1,5.5,1,6,1)
centroid_matrix <-  matrix(data = centroid_array, nrow = 2, ncol = length(centroid_array)/2, dimnames = list(c('x','y')))

#Returns an  array which represents the distances from the point to each centroid. It expects to values for its coordinates.
getCentroidDistances <- function(cent_matrix, xycoor){
  distances <- c()
  for(i in 1:((length(cent_matrix)/2))){
    distances[i] <- euclide(xycoor, c(cent_matrix[[1,i]], cent_matrix[[2,i]]))
  }
  return(distances)
}
#Assigns coordinates to centroids based on euclidean distance. It expects both centroid coordinates and point
#coordinates to be represented in matrix.
Kcluster <- function(centroid_matrix, coor_matrix){
  clusters <- matrix(nrow = 3, ncol = length(coor_matrix)/2, dimnames = list(c("x","y","centroid")))
  for(i in 1:(length(coor_matrix)/2)){
    coor <- as.vector(coor_matrix[,i])
    centroid <- which.min(getCentroidDistances(centroid_matrix,coor))
    clusters[1:2,i] <- coor
    clusters[3,i] <- centroid    
  }
  return(clusters)
}


##DRIVER FUNCTION
my.kmeans <- function(coor_matrix, num_of_clusters = 2,  max.iter = 10){
  samePos <- FALSE
  iters <- 0
  lastMatrix <- matrix()
  #generate random coors for centroids and make centroid matrix.
  
  #The limits are the maximum and minimum of each coor.
  ranx <- sample(min(coor_matrix[1,]):max(coor_matrix[1,]), num_of_clusters, replace = FALSE)
  
  rany <- sample(min(coor_matrix[2,]):max(coor_matrix[2,]), num_of_clusters, replace = FALSE)
  centroid_matrix <- matrix(NA, nrow = 2, ncol = length(rany), dimnames = list(c("x","y")))
  centroid_matrix[1,] <- ranx
  centroid_matrix[2,] <- rany
  
  
  while(!samePos || iters != max.iter){
    #measure distances from each coor to each centroid
    #assign each xycoor to a centroid
    current_clusters <- Kcluster(centroid_matrix, coor_matrix)
    
    #move the centroid to a new middle
    for(i in 1:num_of_clusters){
      cluster_i <- which(current_clusters[3,] == i)
      centroid_matrix[,i] <- CenterCentroid(current_clusters[1,cluster_i], current_clusters[2,cluster_i])
    }
    #check if each entry is the same as the last
    if(identical(lastMatrix,current_clusters)){
      samePos <- TRUE
    }
    #make a copy of the matrix
    lastMatrix <- current_clusters
    
    #repeat
    iters <- iters + 1
    
    
    
  }
  
  df_out <- as.data.frame(t(current_clusters))
  xcenters <- c()
  ycenters <- c()
  for(i in 1:num_of_clusters){
    xcenters <- append(xcenters,rep(centroid_matrix[1,i], length(which(df_out$centroid == i))))
    ycenters <- append(ycenters,rep(centroid_matrix[2,i], length(which(df_out$centroid == i))))
  }
  df_out["Center_X"] <- xcenters
  df_out["Center_Y"] <- ycenters
  return(df_out)
  
}
#building test matrix
x <- c(3,2.5,2.5,2.5,2,3,2,2,2.5,3,3,3.5,4)
y <- c(0.5,-0.5,-1,-1.5,-1,-1,2.2,3,3.3,2.4,3,3.5,3)
mycoor <- matrix(data = NA, nrow = 2, ncol = length(x), dimnames = list(c("x","y")))
mycoor[1,] <- x
mycoor[2,] <- y
kmeans.test <- (my.kmeans(mycoor, num_of_clusters = 3))
qplot(x,y, data = kmeans.test, color = centroid, cex = 3)
kmeans.test

