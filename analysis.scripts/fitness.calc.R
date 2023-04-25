dat0 <- read.csv("fitness/fitness_h0.csv")[,-1]
dat1 <- read.csv("fitness/fitness_h1.csv")[,-1]

x <- dat0[1,]
h <- 0
s <- .3

getFit <- function(x, h, s){
  fem00 <- (sum(x[,1:3])/sum(x[,1:6])) * (sum(x[,7:9])/sum(x[,7:12]))
  fem01 <- (sum(x[,1:3])/sum(x[,1:6])) * (sum(x[,10:12])/sum(x[,7:12])) +
           (sum(x[,4:6])/sum(x[,1:6])) * (sum(x[,7:9])/sum(x[,7:12]))
  fem11 <- (sum(x[,4:6])/sum(x[,1:6])) * (sum(x[,10:12])/sum(x[,7:12]))
  mal00 <- (sum(x[,1:3])/sum(x[,1:6])) * (sum(x[,13:15])/sum(x[,13:18]))
  mal01 <- (sum(x[,1:3])/sum(x[,1:6])) * (sum(x[,16:18])/sum(x[,13:18])) +
           (sum(x[,4:6])/sum(x[,1:6])) * (sum(x[,13:15])/sum(x[,13:18]))
  mal11 <- (sum(x[,4:6])/sum(x[,1:6])) * (sum(x[,16:18])/sum(x[,13:18]))
  
  femW <- fem00 * 1/(1+s) + 
    fem01 * 1/(1+h*s) + 
    fem11 * 1
  
  malW <- (mal00 * (1+s) + 
    mal01 * (1+h*s) + 
    mal11 * 1)/(1+s)
  fitnesses <- c(femW, malW)
  names(fitnesses) <- c("male.fitness", "female.fitness")
  return(fitnesses)
  
  
}




fem0 <- fem1 <- mal0 <- mal1 <-c()
for(i in 1:100){
  foo <- getFit(dat0[i,], h=0, s=.3)
  mal0[i] <- foo[1]
  fem0[i] <- foo[2]
  foo <- getFit(dat1[i,], h=1, s=.3)
  mal1[i] <- foo[1]
  fem1[i] <- foo[2]
}


