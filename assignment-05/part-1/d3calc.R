#d3calc.R - Symmetries of the equilateral triangle

D3.makeDataFrame <- function() {
  DF <- data.frame(name=rep("",8),cfg=rep("",8),stringsAsFactors = FALSE)
  DF[1,] <- c("i","ABCD")
  DF[2,] <- c("r","DABC")
  DF[3,] <- c("t","CDAB")
  DF[4,] <- c("s","BCDA")
  DF[5,] <- c("x","BADC")
  DF[6,] <- c("y","CBAD")
  DF[7,] <- c("z","ADCB")
  DF[8,] <- c("w","DCBA")
  
  return(DF)
}

BiggsDF <- D3.makeDataFrame()
BiggsDF
D3.showConfigs <- function(DF) {
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,24),ylim = c(-1,4), asp = 1, axes = FALSE)
  for (i in 0:7) {
    points(c(0,2,2,0,0)+3*i,c(0,0,2,2,0),type = "l", lwd = 2)
    lbl <- strsplit(DF[i+1,2],"")[[1]]
    text(c(0.3,1.7,1.7, 0.3)+3*i,c(1.7,1.7,0.3,0.3),lbl)
    text(1+3*i,-0.5,DF[i+1,1])
    segments(c(13,14.7,20.3,21),
             c(-0.5,-0.3,-0.3,1),
             c(13,17,18,23),
             c(2,2,2,1),
             lty = 2)
  }
}
D3.showConfigs(BiggsDF)

#cfg is a string of symbols, reading clockwise from the top of the triangle
D3.showTriangle <- function(cfg){
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,2),ylim = c(0,2), asp = 1, axes = FALSE)
  points(c(0,2,2,0,0),c(0,0,2,2,0),type = "l", lwd = 2)
  lbl <- strsplit(cfg,"")[[1]]
  text(c(0.15,1.85,1.85, 0.15),c(1.85,1.85,0.15,0.15),lbl)
}
D3.showTriangle("ABCD")

#a is one of the Biggs symbols for an operation.

#The return value is the new configuration
D3.apply <- function(a,cfg){
  v <-strsplit(cfg,"")[[1]]   #select first component of list
  w <- switch(a,
              "i" = v,
              "r" = c(v[4],v[1],v[2],v[3]),
              "t" = c(v[3],v[4],v[1],v[2]),
              "s" = c(v[2],v[3],v[4],v[1]),
              "x" = c(v[2],v[1],v[4],v[3]),
              "y" = c(v[3],v[2],v[1],v[4]),
              "z" = c(v[1],v[4],v[3],v[2]),
              "w" = c(v[4],v[3],v[2],v[1])
  )
  s <- paste(w,collapse="") 
  return(s)
}
D3.apply("w","ABCD")


D3.multiply <- function(DF,a,b){
  #Look up the name, which occurs once and only once
  idx <- which.max(DF$name==b)
  #Find the corresponding configuration
  cfg <- DF$cfg[idx]
  #Apply the group operation to it
  newcfg <- D3.apply(a,cfg)
 # Look up the configuration
  idx <- which.max(DF$cfg==newcfg)
  return (DF$name[idx])
}
D3.multiply(BiggsDF,"r","r")

#To use this with outer() we must vectorize it
vD3.multiply <- Vectorize(D3.multiply,c("a","b"))
outer(c("i","r","t","s","x","y","z", "w"),c("i","r","t","s","x","y","z", "w"),"vD3.multiply", DF = BiggsDF)

