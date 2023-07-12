library(doSNOW)

source("functions.R")

# Initial Population
pop.size <- 1000

# Probability of an individual having a mutation
mut.prob <- 1/1000

# Generations with columns being gamete types
generations <- 1000

# Set number of clusters
NumberClusters <- 8
cl <- makeCluster(NumberClusters, outfile = "")
registerDoSNOW(cl)

iter <- 100
iter.results <- list()

# Set up results table
parsed <- as.data.frame(matrix(NA, 1, 6))
colnames(parsed) <- c("frqX","frqY","model","h","r","s")

# This sets up results with each row being an iteration
results <- as.data.frame(matrix(NA, 1, 18))
colnames(results) <- c("XE0N","XE0P","XE0U","XE1N","XE1P","XE1U","XS0N","XS0P","XS0U",
                       "XS1N","XS1P","XS1U","YS0N","YS0P","YS0U","YS1N","YS1P","YS1U")
# Type of fusions to introduce
# Possible models: (auto.and.) allXY, allX, allY, nonparX, parX, nonparY, parY
models <- c("auto.and.nonparX", "auto.and.parX", "auto.and.nonparY", "auto.and.parY")
for(i in 1:length(models)){
  
  model <- models[i]
  # Dominance factor of the female benefit allele (allele 1)
  # 1 = dominant, 0.5 = additive, 0 = recessive
  hs <- c(0,0.5,1)
  for(j in 1:length(hs)){
    
    h <- hs[j]
    # Recombination distance between SAL locus and
    # the point that is fused to the sex chromosome
    rs <- c(0.1, 0.2, 0.4)
    for(k in 1:length(rs)){
      
      r <- rs[k]
      # Selection coefficient for SAL
      ss <- seq(from = 0, to = 1, length = 11)
      for(m in 1:length(ss)){
        s <- ss[m]
        
        iter.results <- foreach(n = 1:iter,  .verbose = T) %dopar% {
          
          pop.gam <- getInitialPop(pop.size = pop.size)
          
          # Number of mutant individuals in a given generation
          num.mutes <- rpois(n=generations, lambda = mut.prob * pop.size)
          
          for(p in 1:generations){
            
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
          }
          return(pop.gam)

        }
        for (q in 1:iter){
          results[q, ] <- iter.results[[q]]
          frqX <- sum(results[q, c(1,2,4,5,7,8,10,11)]) / sum(results[q, 1:12])
          frqY <- sum(results[q, c(13,14,16,17)]) / sum(results[q, 13:18])
          parsed <- rbind(parsed, c(frqX, frqY, model, h, r, s))
        }

        
      }
      
    }
    
  }
  
}
stopCluster(cl)

write.csv(parsed, 'simulation.results.csv')







