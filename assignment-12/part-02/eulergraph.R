library(prodlim) #needed for row.match

Euler.chooseEdges <- function(nVert){
  edges <- matrix(nrow = 0, ncol = 2)
#Mark all the vertices except one randomly chosen one as not connected
  connected <- rep(FALSE,nVert)
  connected[sample((1:nVert),1)]= TRUE
#Step 1: Build a tree by joining an unconnected vertex to a connected vertex
  while (sum(connected==FALSE)>0){
#Choose a connected vertex
    choices <- which(connected==TRUE)
#Careful -- sampling from a vector of length 1 does not work as expected
    v1 <- ifelse (length(choices)==1, choices, sample(choices,1))
#Choose an unconnected vertex
    choices <- which(connected==FALSE)
    v2 <- ifelse (length(choices)==1, choices, sample(choices,1))
#Mark the newly connected vertex
    connected[v2]= TRUE
#Arrange the vertices in ascending order
    edge <- c(min(v1,v2),max(v1,v2))
#Add the edge as the next row in the matrix of edges
    edges <- rbind(edges,edge)
  }

#Step 2: Get rid of all vertices of degree 1
#Connect each "lonely" vertex of degree 1 to a randomly chosen vertex
  degrees <- table(edges)

  lonely <- which(degrees == 1)
  while (length(lonely) >0){
    #Choose a lonely vertex
    v1 <- ifelse (length(lonely)==1, lonely, sample(lonely,1))
    v2 <- sample(nVert,1)   #choose a random vertex
    if (v1 == v2)  next     #if it's the same, try again
    #Arrange the vertices in ascending order to permit easy searching
    edge <- c(min(v1,v2),max(v1,v2))
    if (row.match(edge,edges,nomatch=0) > 0) next
    #Add the edge as the next row in the matrix of edges
    edges <- rbind(edges,edge)
    #Check again for lonely vertices
    degrees <- table(edges)
    lonely <- which(degrees == 1)
  }

#Step 3: Join up or remove edges of odd degree until only two remain
  degrees <- table(edges)
  odd <- which(degrees%%2 == 1)
#If all vertices have even degree we are done
  if (length(odd) == 0)
    return (edges)
#If there are more than two vertices of odd degree, join some up or remove an edge
  while (length(odd) >2){
    edge <- sort(sample(odd,2))
    #Was that edge already in the matrix?
    k <- row.match(edge,edges,nomatch=0)
    #If not, add it to the matrix
    if (k == 0)
      edges <- rbind(edges,edge)
    #Otherwise remove it
    if (k > 0)
      edges <- edges[-k,]
    degrees <- table(edges)
    odd <- which(degrees%%2 == 1)
  }
  return(edges)
}

Euler.placeVertices <- function(nVert){
  radii <- sqrt(runif(nVert,0,1))
#  radii <- 1
  vertices <- data.frame(x=numeric(nVert),y = numeric(nVert), symbol = rep(21,nVert),
                         color = rep("white",nVert),stringsAsFactors =FALSE)
  vertices$x <- radii*cos(2*pi*(1:nVert)/nVert)
  vertices$y <- radii*sin(2*pi*(1:nVert)/nVert)
  return(vertices)
}

Euler.displayGraph <- function(vertices, edges, edgecolors) {
  par(mar=c(0.1,0.1,0.1,0.1))  #don't waste space on margins
  #Find the largest coordinate
  size <- max(c(abs(vertices$x),abs(vertices$y)))
  plot(NULL,axes = FALSE, xlim =c(-size,size),ylim= c(-size,size))
  #Mark the vertices of odd degree with red circles
  degrees <- table(edges)
  colors <- ifelse(degrees%%2 >0, "red", "black")
  points(vertices$x,vertices$y,pch = 19,cex = 2,col = colors,type = "p")
  text(vertices$x-0.05,vertices$y+0.05,as.character(c(1:nrow(vertices))))  
  for (i in 1:nrow(edges)){
    v1 <- edges[i,1]
    v2 <- edges[i,2]
    segments(vertices$x[v1],vertices$y[v1],vertices$x[v2],vertices$y[v2],col = edgecolors[i],lwd = 2)
  }
}

Euler.findClosestVertexNew <- function(vertices,x,y){
  n <- nrow(vertices)
  distsq <- numeric(n)
  for (i in 1:n){
    distsq[i] <- (vertices[i,2]-x)^2+(vertices[i,3]-y)^2
  }
  k <- which(distsq == min(distsq))
  stopifnot(length(k)==1)
  return(k)
}

# edgesDF <- Euler.chooseEdges(6)
# verticesDF <- Euler.placeVertices(6)
# Euler.displayGraph(verticesDF,edgesDF)
# Euler.findClosestVertex(verticesDF,0,0)
