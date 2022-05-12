library(stringr)
library(ggplot2)
library(dplyr)

drift <- read.csv("../results/parseddrift.csv")
drift <- drift[,2:7]
results <- read.csv("../results/results.csv")

results$h <- as.numeric(str_sub(results$h, start=5))
results$r <- as.numeric(str_sub(results$r, start=5))
results$s <- as.numeric(str_sub(results$s, start=5))
results$model <- str_sub(results$model, start=9)

X <- data.frame(drift[, c(1,3:6)])
Y <- data.frame(drift[, 2:6])
colnames(X)[1] <- colnames(Y)[1] <- "Freq"
drift <- rbind(X, Y)
drift$chromosome <- rep(c("X","Y"), each = 1000)


X <- data.frame(results[, c(1,3:6)])
Y <- data.frame(results[, 2:6])
colnames(X)[1] <- colnames(Y)[1] <- "Freq"
results <- rbind(X, Y)
results$chromosome <- rep(c("X","Y"), each = 360000)
results <- filter(results, (model == "auto.and.parX" & h == 0.5 & r == 0.1))

both <- rbind(drift, results)

# library(ggraptR)
# ggraptR(both)




data <- both
# data$group <- "r1par"

fixed <- filter(data, (Freq >= 0.80))


agg <- data %>%
  group_by(r,s,h,chromosome) %>%
  summarise_at(vars(Freq), list(Freq_median = ~median(., na.rm=TRUE)))


# Dotted line plot
p3 <- ggplot(agg, aes(y=Freq_median, x=s)) + 
  geom_point(data = data, aes(colour=chromosome, fill = chromosome, y = Freq, x = s), stat="identity", alpha=0.2, size=0.1, position=position_jitterdodge(jitter.width = 0.03, dodge.width=0.1)) + 
  geom_point(aes(colour=chromosome, fill=chromosome), shape = 21,colour = "black", alpha = 0.5, stat="identity",position=position_dodge(width = 0.1),  size=2) +
  geom_line(aes(colour=chromosome),stat="identity",position=position_dodge(width = 0.1), ) +
  #geom_point(data = fixed, aes(colour=chromosome, fill = chromosome, y = Freq, x = s), stat="identity", alpha=0.6, size=2, position=position_jitterdodge(jitter.width = 0.03, dodge.width=0.1)) + 
  theme_light() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + 
  xlab("s") + ylab("Fused chromosome frequency")
p3
ggsave(plot = p3, "figure3.png", width = 10, height = 7, type = "cairo-png")
ggsave(plot = p3, "figure3.pdf", width = 10, height = 7)
