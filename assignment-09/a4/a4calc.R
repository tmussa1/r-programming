#a4calc.R

makeA4data <- function(neutral) {
  N <- 12
  neutral = "gray90"  #for testing only
  DF <- data.frame(button=character(N),
                   perm =character(N),color=character(N),stringsAsFactors= FALSE)
  DF[1,] <- c("btn2413","(13)(24)",neutral)
  DF[2,] <- c("btn2314","(14)(23)",neutral)
  DF[3,] <- c("btn3412","(12)(34)",neutral)
  DF[4,] <- c("btn234","(234)",neutral)
  DF[5,] <- c("btn243","(243)",neutral)
  DF[6,] <- c("btn134","(134)",neutral)
  DF[7,] <- c("btn143","(143)",neutral)
  DF[8,] <- c("btn124","(124)",neutral)
  DF[9,] <- c("btn142","(142)",neutral)
  DF[10,] <- c("btn123","(123)",neutral)
  DF[11,] <- c("btn132","(132)",neutral)
  DF[12,] <- c("btnI","I",neutral)
  return(DF)
}

#DF <- makeA4data(neutral)

