#s3calc.R

makeS3data <- function(neutral) {
  N <- 6
  DF <- data.frame(button=character(N),
                   perm =character(N),color=character(N),stringsAsFactors= FALSE)
  DF[1,] <- c("btnI","I",neutral)
  DF[2,] <- c("btn123","(123)",neutral)
  DF[3,] <- c("btn132","(132)",neutral)
  DF[4,] <- c("btn23","(23)",neutral)
  DF[5,] <- c("btn13","(13)",neutral)
  DF[6,] <- c("btn12","(12)",neutral)
  return(DF)
}

#DF <- makeS3data()

