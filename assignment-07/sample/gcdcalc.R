#gcdcalc.R

gcd.euclid <- function(x,y,details = FALSE) {
  #Make a data frame with more rows than we will ever need
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
  #In row i the remainder was 0 and the smaller number b is the gcd.
  gcd <- DF[i,2]
  #If details are not requested we have the answer.
  if(details == FALSE) return(gcd)
  j <- i-1
  #In row j the remainder r2 is the gcd and a2 = b2q2+r2.
  #So r2 = (1)(a2)-(q2)(b2)
  #Therefore m2 = 1 and n2 = -q2
  DF[j,5] <- 1
  DF[j,6] <- -DF[j,4]
  #In row j-1 a1 = b1q1+r1. The remainder r1 becomes b2 and b1 becomes a2.
  #So a1 = a2q1+b2   and b2 = a1-q1a2
  #The gcd is m2a2+n2b2=m2a2+n2a1-n2q1a2=n2a1+(m2-n2q1)a2
  #Therefore m1 = n2 and n1 = m2-n2q1
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
gcd.euclid(30,37)
gcd.euclid(30,37,details=TRUE)$gcd
gcd.euclid(30,37,details=TRUE)$m
gcd.euclid(30,37,details=TRUE)$n
gcd.euclid(3000,370, details = TRUE)
