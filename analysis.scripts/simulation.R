library(doSNOW)

source("functions.R")

# Initial Population
pop.size <- 1000

# Probability of an individual having a mutation
mut.prob <- 1/1000

# Generations with columns being gamete types
generations <- 1000

# Set number of clusters
NumberClusters <- 4
cl <- makeCluster(NumberClusters, outfile = "")
registerDoSNOW(cl)

# Set number of simulation iterations
iter <- 100
iter.results <- list()

# Set up results table
parsed <- as.data.frame(matrix(NA, 1, 6))
colnames(parsed) <- c("frqX","frqY","model","h","r","s")
counter <- 1


# Type of fusions to introduce
# Possible models: (auto.and.) allXY, allX, allY, nonparX, parX, nonparY, parY
models <- c("auto.and.nonparX", "auto.and.parX", "auto.and.nonparY", "auto.and.parY")
for(o in 1:length(models)){
  
  model <- models[o]    
  # Dominance factor of the female benefit allele (allele 1)
  # 1 = dominant, 0.5 = additive, 0 = recessive
  hs <- c(0,0.5,1)
  for(n in 1:length(hs)){
    
    h <- hs[n]
    # Recombination distance between SAL locus and
    # the point that is fused to the sex chromosome
    rs <- c(0.1, 0.2, 0.4)
    for(m in 1:length(rs)){
      
      r <- rs[m]
      # Selection coefficient for SAL
      ss <- seq(from = 0, to = 1, length = 11)
      for(k in 1:length(ss)){
        s <- ss[k]
        
        # This sets up results with each row being an iteration
        results <- as.data.frame(matrix(NA, 1, 18))
        colnames(results) <- c("XE0N","XE0P","XE0U","XE1N","XE1P","XE1U","XS0N","XS0P","XS0U",
                               "XS1N","XS1P","XS1U","YS0N","YS0P","YS0U","YS1N","YS1P","YS1U")
        
        iter.results <- foreach(j = 1:iter,  .verbose = T) %dopar% {
          
          pop.gam <- getInitialPop(pop.size = pop.size)
          
          # Number of mutant individuals in a given generation
          num.mutes <- rpois(n=generations, lambda = mut.prob * pop.size)
          
          for(i in 1:generations){
            
            # Get Juveniles based on initial gamete pool
            pop.juv <- getJuveniles(pop.gam, pop.size)
            
            # Get population fitnesses
            pop.fits <- popFit(pop.juv, s = s, h = h)
            
            # Viability Selection
            pop.adu <- perfSeln(pop.juv, pop.fits)
            
            # Recombination
            pop.recom <- perfGameto(pop.adu, r = r)
            
            # Gametogenesis
            pop.gam <- StochRound(pop.recom)
            
            # Mutations
            pop.gam <- perfMutation(pop.gam, num.mutes, model)
            return(pop.gam)

          }
          results[j, ] <- iter.results[[j]]
          
          frqX <- sum(results[j, c(1,2,4,5,7,8,10,11)]) / sum(results[j, 1:12])
          frqY <- sum(results[j, c(13,14,16,17)]) / sum(results[j, 13:18])
          parsed[counter, (1:6)] <- c(frqX, frqY, model, h, r, s)
          counter <- counter + 1
          print(counter)
        
        }

      }

    }

  }

}

stopCluster(cl)

# Add fused chromosome data to final table
X <- data.frame(parsed[, c(1,3:6)])
Y <- data.frame(parsed[, 2:6])
colnames(X)[1] <- colnames(Y)[1] <- "Freq"
both <- rbind(X, Y)
both$chromosome <- rep(c("X","Y"), each = nrow(X))

long.results <- both



