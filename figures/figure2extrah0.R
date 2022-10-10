library(stringr)
library(ggplot2)
library(wesanderson)
library(dplyr)

# KW - script for making figure for h = 0

load("~par-nonpar/results/both.RData")

both <- filter(both, !(h == 0.5))
both <- filter(both, !(h == 1))

agg0 <- both %>%
  group_by(model,r,s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))

#KW - There is definitely a cleaner way to do this but it works

#auto.and.nonparX 0.1
mean1 <- agg0$Freq_mean[1]
#auto.and.nonparX 0.2
mean2 <- agg0$Freq_mean[12]
#auto.and.nonparX 0.4
mean3 <- agg0$Freq_mean[23]
#auto.and.nonparY 0.1
mean4 <- agg0$Freq_mean[34]
#auto.and.nonparY 0.2
mean5 <- agg0$Freq_mean[45]
#auto.and.nonparY 0.4
mean6 <- agg0$Freq_mean[56]
#auto.and.parX 0.1
mean7 <- agg0$Freq_mean[67]
#auto.and.parX 0.2
mean8 <- agg0$Freq_mean[78]
#auto.and.parX 0.4
mean9 <- agg0$Freq_mean[89]
#auto.and.parY 0.1
mean10 <- agg0$Freq_mean[100]
#auto.and.parY 0.2
mean11 <- agg0$Freq_mean[111]
#auto.and.parY 0.4
mean12 <- agg0$Freq_mean[122]

means <- matrix(nrow = 12, ncol = 1)
means[,] <- c(mean1,mean2,mean3,mean4,mean5,mean6,mean7,mean8,mean9,mean10,mean11,mean12)

MDE <- filter(both, !(s == 0))

MDE1 <- filter(MDE, (r == 0.1 & (model == "auto.and.nonparX")))
MDE2 <- filter(MDE, (r == 0.2 & (model == "auto.and.nonparX")))
MDE3 <- filter(MDE, (r == 0.4 & (model == "auto.and.nonparX")))
MDE4 <- filter(MDE, (r == 0.1 & (model == "auto.and.nonparY")))
MDE5 <- filter(MDE, (r == 0.2 & (model == "auto.and.nonparY")))
MDE6 <- filter(MDE, (r == 0.4 & (model == "auto.and.nonparY")))
MDE7 <- filter(MDE, (r == 0.1 & (model == "auto.and.parX")))
MDE8 <- filter(MDE, (r == 0.2 & (model == "auto.and.parX")))
MDE9 <- filter(MDE, (r == 0.4 & (model == "auto.and.parX")))
MDE10 <- filter(MDE, (r == 0.1 & (model == "auto.and.parY")))
MDE11 <- filter(MDE, (r == 0.2 & (model == "auto.and.parY")))
MDE12 <- filter(MDE, (r == 0.4 & (model == "auto.and.parY")))

agg1 <- MDE1 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg1$group <- "NonPARX r=0.1"
agg1$Freq_mean <- (agg1$Freq_mean - mean1)

agg2 <- MDE2 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg2$group <- "NonPARX r=0.2"
agg2$Freq_mean <- (agg2$Freq_mean - mean2)

agg3 <- MDE3 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg3$group <- "NonPARX r=0.4"
agg3$Freq_mean <- (agg3$Freq_mean - mean3)

agg4 <- MDE4 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg4$group <- "NonPARY r=0.1"
agg4$Freq_mean <- (agg4$Freq_mean - mean4)

agg5 <- MDE5 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg5$group <- "NonPARY r=0.2"
agg5$Freq_mean <- (agg5$Freq_mean - mean5)

agg6 <- MDE6 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg6$group <- "NonPARY r=0.4"
agg6$Freq_mean <- (agg6$Freq_mean - mean6)

agg7 <- MDE7 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg7$group <- "PARX r=0.1"
agg7$Freq_mean <- (agg7$Freq_mean - mean7)

agg8 <- MDE8 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg8$group <- "PARX r=0.2"
agg8$Freq_mean <- (agg8$Freq_mean - mean8)

agg9 <- MDE9 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg9$group <- "PARX r=0.4"
agg9$Freq_mean <- (agg9$Freq_mean - mean9)

agg10 <- MDE10 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg10$group <- "PARY r=0.1"
agg10$Freq_mean <- (agg10$Freq_mean - mean10)

agg11 <- MDE11 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg11$group <- "PARY r=0.2"
agg11$Freq_mean <- (agg11$Freq_mean - mean11)

agg12 <- MDE12 %>%
  group_by(s) %>%
  summarise_at(vars(Freq), list(Freq_mean = ~mean(., na.rm=TRUE)))
agg12$group <- "PARY r=0.4"
agg12$Freq_mean <- (agg12$Freq_mean - mean12)

aggs <- rbind(agg1,agg2,agg3,agg4,agg5,agg6,agg7,agg8,agg9,agg10,agg11,agg12)
aggs$cols <- rep(1:3, each = 10, times = 4)
aggs$cols <- as.factor(aggs$cols)
aggs$linetype <- rep(c("X","Y"), each = 30, times = 2)
aggs$linetype <- as.factor(aggs$linetype)

p1 <- ggplot(aggs, aes(x=s, y=Freq_mean, group = group, color = cols, linetype=linetype)) + 
  geom_line() +
  geom_hline(yintercept=0) +
  ylab("Mean Deviation from MDE") + labs(color = "Model") + xlab("selection coefficient") +
  scale_colour_manual(labels = c("r=0.1", "r=0.2","r=0.4"), values = c(wes_palette("Darjeeling1", n=3, type="discrete"))) +
  scale_linetype_manual("Chromosome",values=c("X"=2,"Y"=1)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  ylim(-0.12,0.275) +
  annotate(geom="text", x=0.15, y=0.25, label="Non-PAR", size=5) +
  annotate(geom="text", x=0.15, y=-0.08, label="PAR       ", size=5) +
  annotate(geom="text", x=0.6, y=0.275, label="h = 0", size=6) +
  theme_light() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) 
p1

ggsave(plot = p1, "figure2extrah0.png", width = 10, height = 7, type = "cairo-png")
ggsave(plot = p1, "figure2extrah0.pdf", width = 10, height = 7)


