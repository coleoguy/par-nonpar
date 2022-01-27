library(stringr)
results <- read.csv("../results/results.csv")
results$h <- as.numeric(str_sub(results$h, start=5))
results$r <- as.numeric(str_sub(results$r, start=5))
results$s <- as.numeric(str_sub(results$s, start=5))





X <- data.frame(results[, c(1,3:6)])
Y <- data.frame(results[, 2:6])
colnames(X)[1] <- colnames(Y)[1] <- "Freq"
both <- rbind(X, Y)
both$chromosome <- rep(c("X","Y"), each = 360000)

# library(ggraptR)
# ggraptR(both)

R1 <- both[both$r == 0.1, ]
R2 <- both[both$r == 0.2, ]
R4 <- both[both$r == 0.4, ]

# TODO
ggplot(R4, aes(y=Freq_median, x=s)) + 
  geom_point(aes(colour=chromosome), stat="identity", position="identity", alpha=0.5, size=3) + 
  facet_grid(model ~ h, scales="free_y") + theme_bw() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + 
  scale_size(range=c(1, 3)) + xlab("s") + ylab("Freq_median")

ggraptR(R1)




foo <- R1[R1$s==.5,]
foo <- foo[foo$h==.5,]
foo <- foo[foo$model=="model = auto.and.nonparX",]
hist(foo$Freq[foo$chromosome=="X"])
hist(foo$Freq[foo$chromosome=="Y"])
