library(stringr)
library(ggplot2)
library(dplyr)

load("~/par-nonpar/results/both.RData")

agg <- both %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
mean0 <- agg$Freq_mean[1]

data <- filter(both, !(s == 0))

MDE <- data
MDE$Freq <- (MDE$Freq - mean0)

MDE <- filter(MDE, !(h == 0))
MDE <- filter(MDE, !(h == 1))

MDE1 <- filter(MDE, (r == 0.1 & (model == "auto.and.nonparX" | model == "auto.and.nonparY")))
MDE2 <- filter(MDE, (r == 0.2 & (model == "auto.and.nonparX" | model == "auto.and.nonparY")))
MDE3 <- filter(MDE, (r == 0.4 & (model == "auto.and.nonparX" | model == "auto.and.nonparY")))
MDE4 <- filter(MDE, (r == 0.1 & (model == "auto.and.parX" | model == "auto.and.parY")))
MDE5 <- filter(MDE, (r == 0.2 & (model == "auto.and.parX" | model == "auto.and.parY")))
MDE6 <- filter(MDE, (r == 0.4 & (model == "auto.and.parX" | model == "auto.and.parY")))

agg1 <- MDE1 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg1$group <- "NonPAR r=0.1"

agg2 <- MDE2 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg2$group <- "NonPAR r=0.2"

agg3 <- MDE3 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg3$group <- "NonPAR r=0.4"

agg4 <- MDE4 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg4$group <- "PAR r=0.1"

agg5 <- MDE5 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg5$group <- "PAR r=0.2"

agg6 <- MDE6 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg6$group <- "PAR r=0.4"

aggs <- rbind(agg1,agg2,agg3,agg4,agg5,agg6)


p1 <- ggplot(aggs, aes(x=s, y=Freq_mean, group = group, color=group)) + 
  geom_line() +
  geom_hline(yintercept=0) +
  ggtitle("Mutation-Drift Equilibrium") + ylab("Mean Fusion Frequency") + labs(color = "Model") +
  theme_light() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) 
p1

ggsave(plot = p1, "figure2.png", width = 10, height = 7, type = "cairo-png")
ggsave(plot = p1, "figure2.pdf", width = 10, height = 7)


