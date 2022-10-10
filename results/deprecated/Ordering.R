oldresults <- read.csv("../results/oldparsed.csv")
r1driftresults <- read.csv("../results/parsedr1drift.csv")
r2driftresults <- read.csv("../results/parsedr2drift.csv")
r4driftresults <- read.csv("../results/parsedr4drift.csv")

results1 <- oldresults[,2:7]
results1$h <- as.numeric(str_sub(results1$h, start=5))
results1$r <- as.numeric(str_sub(results1$r, start=5))
results1$s <- as.numeric(str_sub(results1$s, start=5))
results1$model <- str_sub(results1$model, start=9)


results2 <- r1driftresults[,2:7]
results3 <- r2driftresults[,2:7]
results4 <- r4driftresults[,2:7]

results <- rbind(results1, results2, results3, results4)

ordered <- results[order(results$model,results$h,results$r,results$s),]
save(ordered, file = "ordered.RData")
write.csv(ordered, file = "ordered.csv")

X <- data.frame(ordered[, c(1,3:6)])
X$chromosome <- "X"
Y <- data.frame(ordered[, 2:6])
Y$chromosome <- "Y"
colnames(X)[1] <- colnames(Y)[1] <- "Freq"
both <- rbind(X, Y)

save(both, file = "both.RData")
write.csv(both, file = "both.csv")
