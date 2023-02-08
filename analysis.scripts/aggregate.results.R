# this script aggregates results and saves out files for plotting
load("../results/sim.results.RData")

res <- as.data.frame(matrix(NA, 10000, 6))
colnames(res) <- c("freq","s","r","h","mod","chrom")
s <- unique(both$s)
r <- unique(both$r)
h <- unique(both$h)
mod <- unique(both$model)
chrom <- unique(both$chromosome)
currow <- 0
for(i in s){
  print(i)
  for(j in r){
    for(k in h){
      for(m in mod){
        for (n in chrom){
          currow <- currow + 1
          hit <- both$s == i & both$r == j & 
            both$h == k & both$model == m &
            both$chromosome == n
            curres <- both[hit, ]
            res$freq[currow] <- mean(curres$Freq)
            res[currow, c(2,3,4)] <- curres[1, c(5,4,3)]
            res$mod[currow] <- curres$model[1]
            res$chrom[currow] <- curres$chromosome[1]
            
        }
      }
    }
  }
}
res <- res[complete.cases(res),]

res$dev <- NA
for(i in 1:nrow(res)){
  hit <- which(res$r == res$r[i] &
                 res$h == res$h[i] &
                 res$mod == res$mod[i] &
                 res$chrom == res$chrom[i] &
                 res$s == 0)
  res$dev[i] <- res$freq[i] - res$freq[hit]
}



write.csv(res, "../results/agg.dat.csv", row.names = F)
