
prime.findCompositeNumbersUpto53 <- function() {
  
  composites <- c()
  
  for(i in (4 : 53)){
    
    uptosqrt <- as.integer(sqrt(i))
    
    for(j in (2 : uptosqrt)){
     
      if(i %% j == 0){
        composites <- cbind(composites, i)
        break;
      }
      
    }
  }
  
  return (composites)
}

gcd.euclid <- function(x,y,details = FALSE) {
  
  N <- 100
  DF <- data.frame(a=numeric(N),b=numeric(N),r=numeric(N),q=numeric(N),
                   m=numeric(N),n=numeric(N))
  i <- 1
  DF[i,] <- c(max(x,y),min(x,y),max(x,y)%%min(x,y),max(x,y)%/%min(x,y),0,0)
  while (DF[i,3] > 0){
    DF[i+1,1] <- DF[i,2]
    DF[i+1,2] <- DF[i,3]
    DF[i+1,4] <- DF[i,2]%/%DF[i,3]
    DF[i+1,3] <- DF[i,2]%%DF[i,3]
    i <- i+1
  }

  gcd <- DF[i,2]

  if(details == FALSE) return(gcd)
  j <- i-1

  DF[j,5] <- 1
  DF[j,6] <- -DF[j,4]

  while (j > 1){
    DF[j-1,5] <- DF[j,6]
    DF[j-1,6] <- DF[j,5]-DF[j,6]*DF[j-1,4]
    j <- j-1
  }
  
  m <- ifelse(x > y, DF[1,5], DF[1,6])
  n <- ifelse(x > y, DF[1,6], DF[1,5])
  stopifnot(gcd ==m*x+n*y)
  return(list(gcd=gcd,m=m,n=n,df=head(DF,i)))
}

prime.findCoPrimes <- function(num){
  
  coPrimes <- c()
  
  for(val in (1 : num)){
    
    if(gcd.euclid(val, num) == 1){
      coPrimes <- cbind(coPrimes, val)
    }
    
  }
  
  return (coPrimes)
}

prime.findCoPrimes(100)

prime.findOrder <- function(num, base){
  
  order <- 1
  
  while(num != 1){
    num = (num ^ order) %% base;
    order = order + 1
  }
  
  if(order == 1){
    return (order)
  }
  
  return (order - 1)
}

