results <- as.data.frame(matrix(NA, 1, 6))
colnames(results) <- c("frqX","frqY","model","h","r","s")
counter <- 1

for(i in 1:length(par.results)){   # models
  for(j in 1:length(par.results[[1]])){ # dominance factors
    for(k in 1:length(par.results[[1]][[1]])){ # recombination rates
      for(m in 1:length(par.results[[1]][[1]][[1]])){ # selection coef
        for(n in 1:100){
          dat <- par.results[[i]][[j]][[k]][[m]][[n]]
          results[counter, 1] <- sum(dat[1000, c(1,2,4,5,7,8,10,11)]) / sum(dat[1000, 1:12])
          results[counter, 2] <- sum(dat[1000, c(13,14,16,17)]) / sum(dat[1000, 13:18])
          results[counter, 3] <- names(par.results)[i]
          results[counter, 4] <- names(par.results[[i]])[j]
          results[counter, 5] <- names(par.results[[i]][[j]])[k]
          results[counter, 6] <- names(par.results[[i]][[j]][[k]])[m]
          counter <- counter + 1
          print(counter)
        }
      }
    }
  }
}

save(results, file = "parseddrift.RData")
write.csv(results, file = "parseddrift.csv")
