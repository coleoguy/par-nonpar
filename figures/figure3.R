library(ggplot2)
library(wesanderson)

dat <- read.csv("figure.data.csv")
facet.labs <- c(`0` = "h = 0", `0.5` = "h = 0.5", `1` = "h = 1")
dat_text <- data.frame(label = c("A", "B", "C"), h = c(0, 0.5, 1))

# Vertical Plot

p1 <- ggplot(dat) + 
  geom_line(dat[dat$model == "nonpar",], mapping = aes(x=s, y=Freq,color = as.character(r), linetype=chrom)) +
  geom_line(dat[dat$model == "par",], mapping = aes(x=s, y=Freq,color = as.character(r), linetype=chrom)) +
  geom_hline(yintercept=0) +
  facet_grid(rows=h ~ ., labeller = as_labeller(facet.labs)) +
  ylab("deviation from MDE") + labs(color = "Distance") + xlab("selection coefficient") +
  scale_colour_manual(labels = c("r=0.1", "r=0.2","r=0.4"), values = c(wes_palette("Darjeeling1", n=3, type="discrete"))) +
  scale_linetype_manual("Chromosome",values=c("X"=2,"Y"=1)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  ylim(-0.12,0.54) +
  annotate(geom="text", x=0.1, y=0.45, label="Non-PAR", size=3) +
  annotate(geom="text", x=0.1, y=-0.08, label="PAR       ", size=3) +
  theme_light() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=10, hjust=0.5, vjust=0.5))  +
  theme(strip.text = element_text(color = "black")) +
  theme(strip.background = element_rect(fill = "white")) +
  geom_text(data = dat_text, mapping = aes(x = 0, y = 0.53, label = label, fontface = 'bold'))
p1

ggsave(plot = p1, "figure3.png", width = 4.5, height = 8, type = "cairo-png")
ggsave(plot = p1, "figure3.pdf", width = 4.5, height = 8)


# Horizontal Plot

p2 <- ggplot(dat) + 
  geom_line(dat[dat$model == "nonpar",], mapping = aes(x=s, y=Freq,color = as.character(r), linetype=chrom)) +
  geom_line(dat[dat$model == "par",], mapping = aes(x=s, y=Freq,color = as.character(r), linetype=chrom)) +
  geom_hline(yintercept=0) +
  facet_grid(cols = vars(h), labeller = as_labeller(facet.labs)) +
  ylab("deviation from MDE") + labs(color = "Distance") + xlab("selection coefficient") +
  scale_colour_manual(labels = c("r=0.1", "r=0.2","r=0.4"), values = c(wes_palette("Darjeeling1", n=3, type="discrete"))) +
  scale_linetype_manual("Chromosome",values=c("X"=2,"Y"=1)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  ylim(-0.12,0.54) +
  annotate(geom="text", x=0.1, y=0.45, label="Non-PAR", size=3) +
  annotate(geom="text", x=0.1, y=-0.08, label="PAR       ", size=3) +
  theme_light() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", size=10, hjust=0.5, vjust=0.5))  +
  theme(strip.text = element_text(color = "black")) +
  theme(strip.background = element_rect(fill = "white")) +
  geom_text(data = dat_text, mapping = aes(x = 0, y = 0.53, label = label, fontface = 'bold'))
p2

ggsave(plot = p2, "figure3horizontal.png", width = 10, height = 3.5, type = "cairo-png")
ggsave(plot = p2, "figure3horizontal.pdf", width = 10, height = 3.5)
