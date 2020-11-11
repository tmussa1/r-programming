#permtest.R
library(resampledata)   #datasets to accompany Chihara and Hesterberg
#This works with two factor vectors.
#It does N permutations of the second vector, creates a contingency table, and extracts the top left entry.
#The return value is a vector of N top left entries.
simple.test <- function(v1,v2,N) {
  x <- numeric(N)
  for (i in 1:N){
    tbl<- as.data.frame.matrix(table(v1,sample(v2,length(v2))))
    x[i] <- tbl[1,1]
  }
  return(x)
}

#Here is the code from page 56 of Chihara and Hesterberg
chisq <- function(Obs) {
  Expected <- outer(rowSums(Obs),colSums(Obs)/sum(Obs))
  sum((Obs-Expected)^2/Expected)
}

#Education <- GSS2002$Education
#DeathPenalty <- GSS2002$DeathPenalty
#tbl <- table(Education, DeathPenalty);tbl
#observed <- chisq(tbl); observed

permute.test <- function(v1,v2,N){
  #Remove all rows with NA
  index <- which(is.na(v1) | is.na(v2))
  v1x <- v1[-index]
  v2x <- v2[-index]
  result <- numeric(0)
  for (i in 1:N) {
    v2Perm <- sample(v2x)
    GSS.table <- table(v1x,v2Perm)
    result[i] <- chisq(GSS.table)
  }
  return(result)
}

#result <- permute.test(Education, DeathPenalty, 10000)
#result <- c(result,observed)
#hist(result, prob = TRUE)
#abline(v = observed, col = "blue")   #matches figure 3.8
#curve(dchisq(x,df=4), col = "red", add = TRUE)

