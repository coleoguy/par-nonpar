library(doSNOW)

source("functions.R")

# Initial Population
pop.size <- 1000

# Probability of an individual having a mutation
mut.prob <- 1/1000

# Generations with columns being gamete types
generations <- 1000

par.results <- list()
par.results1 <- list()
par.results2 <- list()
par.results3 <- list()
par.results4 <- list()

# Set number of clusters
NumberClusters <- 100
cl <- makeCluster(NumberClusters, outfile = "")
registerDoSNOW(cl)

# Type of fusions to introduce
# Possible models: (auto.and.) allXY, allX, allY, nonparX, parX, nonparY, parY
models <- c("auto.and.nonparX", "auto.and.parX", "auto.and.nonparY", "auto.and.parY")
for(m in 1:length(models)){

model <- models[m]    
# Dominance factor of the female benefit allele (allele 1)
# 1 = dominant, 0.5 = additive, 0 = recessive
hs <- c(0,0.5,1)
  for(o in 1:length(hs)){
  
    h <- hs[o]
  # Recombination distance between SAL locus and
  # the point that is fused to the sex chromosome
  rs <- c(0.1, 0.2, 0.4)
  for(l in 1:length(rs)){
    
    r <- rs[l]
  # Selection coefficient for SAL
    ss <- seq(from = 0.1, to = 1, length = 10)
    for(k in 1:length(ss)){
    s <- ss[k]
        
        # Set number of iterations
          par.results1 <- foreach(j = 1:100, .verbose = T ) %dopar% {
            
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
            par.results1[[j]] <- results
        }
      par.results2[[k]] <- par.results1
      names(par.results2)[k] <- c(paste("s =", s, sep = " "))
      }
    par.results3[[l]] <- par.results2
    names(par.results3)[l] <- c(paste("r =", r, sep = " "))
    }
  par.results4[[o]] <- par.results3
  names(par.results4)[o] <- c(paste("h =", h, sep = " "))
  }
par.results[[m]] <- par.results4
names(par.results)[m] <- c(paste("model =", model, sep = " "))
}
stopCluster(cl)






