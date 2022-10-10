# this script aggregates results and saves out files for plotting

# H=.5
library(stringr)
library(ggplot2)
library(wesanderson)
library(dplyr)

# KW - script for making figure for h = 0.5

load("../results/sim.results.RData")

both <- filter(both, !(h == 0))
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

write.csv(aggs, "../results/agg.dat.h=5.csv")



# H=0.0


# KW - script for making figure for h = 0.5

load("../results/sim.results.RData")

both <- filter(both, !(h == 0))
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

write.csv(aggs, "../results/agg.dat.h=0.csv")

# H=1


load("../results/sim.results.RData")

both <- filter(both, !(h == 0.5))
both <- filter(both, !(h == 0))

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

write.csv(aggs, "../results/agg.dat.h=1.csv")
