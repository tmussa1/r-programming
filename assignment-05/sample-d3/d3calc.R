#d3calc.R - Symmetries of the equilateral triangle

D3.makeDataFrame <- function() {
  DF <- data.frame(name=rep("",6),cfg=rep("",6),stringsAsFactors = FALSE)
  DF[1,] <- c("i","ABC")
  DF[2,] <- c("r","CAB")
  DF[3,] <- c("s","BCA")
  DF[4,] <- c("x","ACB")
  DF[5,] <- c("y","BAC")
  DF[6,] <- c("z","CBA")
  return(DF)
}

#BiggsDF <- D3.makeDataFrame()
#BiggsDF
D3.showConfigs <- function(DF) {
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,18),ylim = c(-1,3), asp = 1, axes = FALSE)
  for (i in 0:5) {
    points(c(0,2,1,0)+3*i,c(0,0,sqrt(3),0),type = "l")
    lbl <- strsplit(DF[i+1,2],"")[[1]]
    text(c(1,1.75,0.25)+3*i,c(sqrt(3)-0.25,0.25,0.25),lbl)
    text(1+3*i,-0.5,DF[i+1,1])
    segments(c(10,12-0.3*sqrt(3),17+0.3*sqrt(3)),c(-0.5,-0.3,-0.3),
             c(10,13.5+0.3*sqrt(3),15.5-0.3*sqrt(3)),
             c(2,sqrt(3)/2+0.3,sqrt(3)/2+0.3),lty = 2)
  }
}
#D3.showConfigs(BiggsDF)

#cfg is a string of symbols, reading clockwise from the top of the triangle
D3.showTriangle <- function(cfg){
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,3),ylim = c(-1,2), asp = 1, axes = FALSE)
  points(c(0,2,1,0),c(0,0,sqrt(3),0),type = "l", lwd = 2)
  lbl <- strsplit(cfg,"")[[1]]
  text(c(1,1.82,0.18),c(sqrt(3)-0.15,0.12,0.12),lbl)
}
#D3.showTriangle("ABC")

#a is one of the Biggs symbols for an operation.

#The return value is the new configuration
D3.apply <- function(a,cfg){
  v <-strsplit(cfg,"")[[1]]   #select first component of list
  w <- switch(a,
              "i" = v,
              "r" = c(v[3],v[1],v[2]),
              "s" = c(v[2],v[3],v[1]),
              "x" = c(v[1],v[3],v[2]),
              "y" = c(v[2],v[1],v[3]),
              "z" = c(v[3],v[2],v[1])
  )
  s <- paste(w,collapse="") 
  return(s)
}
#D3.apply("r","BCA")


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
#D3.multiply(BiggsDF,"r","r")

#To use this with outer() we must vectorize it
vD3.multiply <- Vectorize(D3.multiply,c("a","b"))
#outer(c("i","r","s","x","y","z"),c("i","r","s","x","y","z"),"vD3.multiply", DF = BiggsDF)

