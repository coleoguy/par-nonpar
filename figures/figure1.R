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

data0 <- both[both$r != 0.2, ]

data1 <- filter(data0, !(model == "model = auto.and.nonparY" & chromosome == "X"))
data2 <- filter(data1, !(model == "model = auto.and.nonparX" & chromosome == "Y"))
data3 <- filter(data2, !(model == "model = auto.and.parY"))

data4 <- data3
data4$group[(data4$r == 0.1 & (data4$model == "model = auto.and.nonparY"|data4$model == "model = auto.and.nonparX"))] <- "r1nonpar"
data4$group[(data4$r == 0.4 & (data4$model == "model = auto.and.nonparY"|data4$model == "model = auto.and.nonparX"))] <- "r4nonpar"
data4$group[(data4$r == 0.1 & data4$model == "model = auto.and.parX")] <- "r1par"
data4$group[(data4$r == 0.4 & data4$model == "model = auto.and.parX")] <- "r4par"

data <- data4

agg <- data %>%
  group_by(r,s,h,group,chromosome) %>%
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


h.labs <- c("0" = "Recessive SA allele","0.5"= "Additive SA allele","1"= "Dominant SA allele")
group.labs <- c("r1nonpar"="Non-PAR fusion","r1par"= "PAR fusion", "r4nonpar"= "Non-PAR fusion","r4par"= "PAR fusion")
r.labs <- c("0.1" = "r = 0.1","0.4"= "r = 0.4")


# Median point plot
p1 <- ggplot(agg, aes(y=Freq_median, x=s)) + 
  geom_point(data = data, aes(colour=chromosome, fill = chromosome, y = Freq, x = s), stat="identity", alpha=0.05, size=0.1, position=position_jitterdodge(jitter.width = 0.03, dodge.width=0.1)) + 
  geom_point(aes(colour=chromosome, fill=chromosome), shape = 21,colour = "black", alpha = 0.5, stat="identity", position=position_dodge(width = 0.1), size=3) +
  facet_grid(r + group ~ h, scales="free_y", labeller = labeller(h = h.labs, group = group.labs, r = r.labs)) +
  theme_light() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + 
  scale_size(range=c(1, 3)) + xlab("s") + ylab("Fused chromosome frequency")
p1
ggsave(plot = p1, "figure1.png", width = 10, height = 7, type = "cairo-png")


# Dotted line plot
p2 <- ggplot(agg, aes(y=Freq_median, x=s)) + 
  geom_point(data = data, aes(colour=chromosome, fill = chromosome, y = Freq, x = s), stat="identity", alpha=0.05, size=0.1, position=position_jitterdodge(jitter.width = 0.03, dodge.width=0.1)) + 
  geom_point(aes(colour=chromosome, fill=chromosome), shape = 21,colour = "black", alpha = 0.5, stat="identity", position=position_dodge(width = 0.1), size=2) +
  geom_line(aes(colour=chromosome),stat="identity") +
  facet_grid(r + group ~ h, scales="free_y", labeller = labeller(h = h.labs, group = group.labs, r = r.labs)) +
  theme_light() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + 
  scale_size(range=c(1, 3)) + xlab("s") + ylab("Fused chromosome frequency")
p2
ggsave(plot = p2, "figure1.2.png", width = 10, height = 7, type = "cairo-png")
