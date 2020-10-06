#s3calc.R

makeS3data <- function(neutral) {
  N <- 6
  DF <- data.frame(button=character(N),
                   perm =character(N),color=character(N),stringsAsFactors= FALSE)
  DF[1,] <- c("btnI","I",neutral)
  DF[2,] <- c("btnr","(123456)",neutral)
  DF[3,] <- c("btnrsquared","(135)(246)",neutral)
  DF[4,] <- c("btnrcubed","(14)(25)(36)",neutral)
  DF[5,] <- c("btnrtothe4th","(153)(264)",neutral)
  DF[6,] <- c("btnrtothe5th","(165432)",neutral)
  DF[7,] <- c("btnfix2and5", "(13)(46)", neutral)
  DF[8,] <- c("btnfix1and4", "(26)(35)", neutral)
  DF[9,] <- c("btnfix3and6", "(15)(24)", neutral)
  DF[10,] <- c("btnflipy", "(12)(36)(45)", neutral)
  DF[11,] <- c("btnflipz", "(14)(23)(56)", neutral)
  DF[12,] <- c("btnflipoppositez", "(16)(25)(34)", neutral)
  return(DF)
}

#DF <- makeS3data()

