library(doSNOW)

source("functions.R")

# Initial Population
pop.size <- 1000

# Probability of an individual having a mutation
mut.prob <- 1/1000

# Generations with columns being gamete types
generations <- 1000

par.results <- list()
iter.results <- list()
s.results <- list()
r.results <- list()
h.results <- list()
model.results <- list()

# Set number of clusters
NumberClusters <- 4
cl <- makeCluster(NumberClusters, outfile = "")
registerDoSNOW(cl)
iter <- 100

# Type of fusions to introduce
# Possible models: (auto.and.) allXY, allX, allY, nonparX, parX, nonparY, parY
models <- c("auto.and.nonparX","auto.and.parX","auto.and.nonparY","auto.and.parY")
for(o in 1:length(models)){
  
  model <- models[o]    
  # Dominance factor of the female benefit allele (allele 1)
  # 1 = dominant, 0.5 = additive, 0 = recessive
  hs <- c(0,0.5,1)
  for(n in 1:length(hs)){
    
    h <- hs[n]
    # Recombination distance between SAL locus and
    # the point that is fused to the sex chromosome
    rs <- c(0.2)
    for(m in 1:length(rs)){
      
      r <- rs[m]
      # Selection coefficient for SAL
      ss <- c(0)
      for(k in 1:length(ss)){
        s <- ss[k]
        
        # Set number of iterations
        iter.results <- foreach(j = 1:iter, .verbose = T ) %dopar% {
          
          pop.gam <- getInitialPop(pop.size = pop.size)
          
          
          # This sets up results with each row being a generation
          results <- as.data.frame(matrix(NA, generations, length(pop.gam)))
          colnames(results) <- names(pop.gam)
          
          # Number of mutant individuals in a given generation
          num.mutes <- rpois(n=generations, lambda = mut.prob * pop.size)
          
          for(i in 1:generations){
            print(i)
            results[i, ] <- pop.gam
            
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
          iter.results[[j]] <- results
        }
        s.results[[k]] <- iter.results
        names(s.results)[k] <- s
      }
      r.results[[m]] <- s.results
      names(r.results)[m] <- r
    }
    h.results[[n]] <- r.results
    names(h.results)[n] <- h
  }
  model.results[[o]] <- h.results
  names(model.results)[o] <- model
}
par.results <- model.results
save(par.results, file = "s0h3r0.2m4data10.RData")
stopCluster(cl)






