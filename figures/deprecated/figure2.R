# this script aggregates results and saves out files for plotting
# it is also vertical not horizontal
library(stringr)
library(dplyr)
library(ggplot2)
library(wesanderson)

load("../results/sim.results.RData")

both <- both[,-6]
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
rm(list=ls()[-4])

facet.labs <- c(`0` = "h = 0", `0.5` = "h = 0.5", `1` = "h = 1")

p1 <- ggplot(dat) + 
  geom_line(dat[dat$model == "nonpar",], mapping = aes(x=s, y=Freq,color = as.character(r), linetype=chrom)) +
  geom_line(dat[dat$model == "par",], mapping = aes(x=s, y=Freq,color = as.character(r), linetype=chrom)) +
  geom_hline(yintercept=0) +
  facet_grid(rows=h ~ ., labeller = as_labeller(facet.labs)) +
  ylab("deviation from MDE") + labs(color = "Distance") + xlab("selection coefficient") +
  scale_colour_manual(labels = c("r=0.1", "r=0.2","r=0.4"), values = c(wes_palette("Darjeeling1", n=3, type="discrete"))) +
  scale_linetype_manual("Chromosome",values=c("X"=2,"Y"=1)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  ylim(-0.12,0.275) +
  annotate(geom="text", x=0.15, y=0.25, label="Non-PAR", size=3) +
  annotate(geom="text", x=0.15, y=-0.08, label="PAR       ", size=3) +
  theme_light() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=10, hjust=0.5, vjust=0.5))  +
  theme(strip.text = element_text(color = "black")) +
  theme(strip.background = element_rect(fill = "white"))

ggsave(plot = p1, "figure2.png", width = 25, height = 7, type = "cairo-png")
ggsave(plot = p1, "figure2.pdf", width = 25, height = 7)
