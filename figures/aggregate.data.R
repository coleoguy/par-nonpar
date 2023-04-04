# this script aggregates results and saves out files for plotting
library(stringr)
both <- read.csv("../results/simulation.results.csv")
res.ver2 <- read.csv("../results/agg.dat.csv")
modchrom <- rep(NA, 792000)
for(i in 1:nrow(both)){
  print(i)
  modchrom[i] <- str_sub(both$model[i], start=-1)
}
both <- both[modchrom == both$chromosome, ]

both <- both[,-7]


foo <- aggregate( Freq ~ model + h + r+s, both, mean)
mutdrift <- foo[foo$s == 0,]

ids <- c()
for(i in 1:nrow(mutdrift)){
  ids[i] <- paste(mutdrift[i,1:3], collapse = "")
}
for(i in 1:nrow(foo)){
  cur.mod <- paste(foo[i,1:3], collapse = "")
  hit <- which(ids == cur.mod)
  foo$Freq[i] <- foo$Freq[i] - mutdrift$Freq[hit]
}
dat <- foo

chrom <- c()
for(i in 1:nrow(dat)){
  chrom[i] <- str_sub(dat$model[i],-1) 
  dat$model[i] <- str_sub(dat$model[i], start = 10, end = -2) 
}
dat$chrom <- chrom

write.csv(dat, file = "figure.data.csv")