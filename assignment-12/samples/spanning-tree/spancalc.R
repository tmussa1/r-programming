#spancalc.R


span.NEcities <- function(){

  lat <- c(42.66575,42.33602,43.23159,41.7658,40.66980,43.66713,43.05685,41.82195,42.26963)
  lon <- c(-73.79901,-71.01789,-071.56007,-72.6734,-073.94384,-070.20716,-070.78201,-071.41973,-071.80892)
  name <- c("Albany","Boston","Concord","Hartford","New York","Portland","Portsmouth","Providence","Worcester")
  DF <- data.frame(name,lat,lon) 
  return(DF)
}
#span.NEcities()

span.NEroads <- function(){
  DF <- data.frame(start = numeric(17), end = numeric(17), dist = numeric(17),intree= logical(17))


  DF[1,] <- c(8,9,42,FALSE)
  DF[2,] <- c(2,9,44,FALSE)
  DF[3,] <- c(2,8,49,FALSE)
  DF[4,] <- c(6,7,50,FALSE)
  DF[5,] <- c(2,7,54,FALSE)
  DF[6,] <- c(4,9,62,FALSE)  
  DF[7,] <- c(3,9,63,FALSE)
  DF[8,] <- c(2,3,74,FALSE)
  DF[9,] <- c(7,9,83,FALSE)
  DF[10,] <- c(3,6,84,FALSE)
  DF[11,] <- c(4,8,86,FALSE)
  DF[12,] <- c(1,4,112,FALSE)
  DF[13,] <- c(4,5,117,FALSE)
  DF[14,] <- c(1,9,134,FALSE)
  DF[15,] <- c(1,5,150,FALSE)
  DF[16,] <- c(1,3,151,FALSE)
  DF[17,] <- c(5,8,185,FALSE)
  return(DF)
}


span.plotmap <- function(cityDF,roadDF, prim = logical(0)){
  west <- min(cityDF$lon); west
  east <- max(cityDF$lon+0.5)
  south <- min(cityDF$lat)
  north <- max(cityDF$lat)
  par(mar = c(1,1,1,1))
  plot(NULL, xlim = c(west,east), ylim = c(south, north), asp = cos(0.6), pch = 20)
  if (length(prim) == 0)
    points(cityDF[,3], cityDF[,2])
  else {
    colors <- ifelse(prim, "red", "blue")
    points(cityDF[,3], cityDF[,2],pch = 19, cex = 2, col = colors)
  }
  text(cityDF[,3]+0.3, cityDF[,2],cityDF[,1])
  for (i in 1:nrow(roadDF)) {
    x1 <- cityDF$lon[roadDF[i,1]]
    y1 <- cityDF$lat[roadDF[i,1]]
    x2 <- cityDF$lon[roadDF[i,2]]
    y2 <- cityDF$lat[roadDF[i,2]]
    if (roadDF[i,4]){
      segments(x1,y1,x2,y2,col = "red",lwd = 2)  
    }
    else{
      segments(x1,y1,x2,y2,lty = 3)
    }
    text((x1+x2)/2,(y1+y2)/2,roadDF[i,3],cex=1.3)
  }
}

span.randomtree <- function(roadDF){
  N <- max(roadDF[,2])   #number of cities
  #Mark all the cities except one randomly chosen one as not in the tree
  roadDF[,4] <- FALSE
  connected <- rep(FALSE,N)
  connected[sample(N,1)] <- TRUE
  #Step 1: Build a tree by joining an unconnected vertex to a connected vertex
  while (sum(connected) < N){
    #Choose a connected vertex
    choices <- which(connected==TRUE)
    #Careful -- sampling from a vector of length 1 does not work as expected
    v1 <- ifelse (length(choices)==1, choices, sample(choices,1))
    #Choose an unconnected vertex
    choices <- which(connected==FALSE)
    v2 <- ifelse (length(choices)==1, choices, sample(choices,1))
    #Arrange the vertices in ascending order
    edge <- c(min(v1,v2),max(v1,v2))
    hasRoad <- which((roadDF[,1]==min(v1,v2))&(roadDF[,2]==max(v1,v2)))
    if (length(hasRoad) == 0)  next               
    #Mark the newly connected vertex
    connected[v2] <- TRUE
    roadDF[hasRoad[1],4] <- TRUE
  }
  return(roadDF)
}

span.treecost <- function(roadDF){
  return (sum(roadDF[,3]*roadDF[,4]))
}

#Use Prim's algorithm to find the lowest cost spanning tree
span.mintree <- function(roadDF){
  N <- max(roadDF[,2])   #number of cities
  #Mark all the cities but the first as not connected
  roadDF[,4] <- FALSE
  connected <- c(TRUE,rep(FALSE,N-1))
  while (sum(connected) < N){
    #Find the shortest road joining a connected city to an unconnected one
    mindist <- 100000
    for (i in 1:nrow(roadDF)) {
      v1 <- roadDF[i,1]
      v2 <- roadDF[i,2]
      if (connected[v1]==connected[v2])
        next
      len <- roadDF[i,3]
      if (len < mindist){
        mindist <- len
        minindex <- i
      }
    }
    v1 <- roadDF[minindex,1]
    v2 <- roadDF[minindex,2]
    connected[v1] <- TRUE
    connected[v2] <- TRUE
    roadDF[minindex,4] <- TRUE
  }
  return (roadDF)
}

  
span.findClosestCity <- function(DF,x,y){
  n <- nrow(DF)
  distsq <- numeric(n)
  for (i in 1:n){
    distsq[i] <- (DF[i,3]-x)^2+(DF[i,2]-y)^2
  }
  k <- which(distsq == min(distsq))
  stopifnot(length(k)==1)
  return(k)
}

span.checkRoad <- function(DF, end1, end2){
  idx <- which((DF[,1]==min(end1,end2))&(DF[,2]==max(end1,end2)))
  if (length(idx) == 0) return (0) #no such road
  if (DF[idx[1],4] == TRUE) return(2)  #in the tree
  return (1)  #road exists and is not in the tree
}


span.addRoad <- function(DF, end1, end2){
  idx <- which((DF[,1]==min(end1,end2))&(DF[,2]==max(end1,end2)))
  if (length(idx) == 0)  return()              
  #Mark the newly added road
  DF[idx[1],4] <- TRUE
  return(DF)
}

span.removeRoad <- function(DF, end1, end2){
  idx <- which((DF[,1]==min(end1,end2))&(DF[,2]==max(end1,end2)))
  if (length(idx) == 0)  return()  
  if(DF[idx[1],4] == FALSE) return()
  #Mark the newly removed road
  DF[idx[1],4] <- FALSE
  return(DF)
}



