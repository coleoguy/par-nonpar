library(stringr)
library(ggraptR)
library(dplyr)
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

agg <- both %>%
  group_by(r,s,h,model,chromosome) %>%
  summarise_at(vars(Freq), list(Freq_median = ~median(., na.rm=TRUE)))

aggR1 <- R1 %>%
  group_by(s,h,model,chromosome) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE), Freq_median = ~median(., na.rm=TRUE)))

aggR2 <- R2 %>%
  group_by(s,h,model,chromosome) %>%
  summarise_at(vars(Freq), list(Freq_median = ~median(., na.rm=TRUE)))

aggR4 <- R4 %>%
  group_by(s,h,model,chromosome) %>%
  summarise_at(vars(Freq), list(Freq_median = ~median(., na.rm=TRUE))) %>%
  ungroup()


# TODO
p1 <- ggplot(aggR1, aes(y=Freq_median, x=s)) + 
  geom_jitter(data = R1, aes(colour=chromosome, y = Freq, x = s), stat="identity", alpha=0.05, size=0.1,width = 0.01) + 
  geom_point(aes(colour=chromosome, fill=chromosome), shape = 21,colour = "black", stat="identity", position="identity", size=3) +
  facet_grid(model ~ h, scales="free_y") + theme_bw() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + 
  scale_size(range=c(1, 3)) + xlab("s") + ylab("Freq_median")
p1
ggsave(plot = p1, "figure1.png", width = 10, height = 7, type = "cairo-png")





foo <- R1[R1$s==.5,]
foo <- foo[foo$h==.5,]
foo <- foo[foo$model=="model = auto.and.nonparX",]
hist(foo$Freq[foo$chromosome=="X"])
hist(foo$Freq[foo$chromosome=="Y"])
