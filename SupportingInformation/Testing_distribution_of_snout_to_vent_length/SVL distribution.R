# Read in the data
dat <- read.table(file="/Users/Cici/Dropbox/MSU Post Doc/Dail-Madsen/Chpt 2/Eco Apps/Data/ALL_Cope2008_2014.txt", header = T, sep = "\t")

dat <- dat[dat$AM_PM  == "PM",]
dat <- dat[is.na(dat$SVL)  == FALSE,]
dat <- dat[is.na(dat$Species)  == FALSE,]
dat <- dat[dat$Group == "Frog",]
dat <- dat[dat$Species != "sp.",]

# Remove species with less than 15 individuals
library(plyr)
spec <- ddply(.data = dat, .variable = "Species", .fun = summarize,
      sum = sum(Num))

Species <- spec[spec[,2] > 15, 1]

dat <- dat[dat$Species %in% Species,]

# Format the data
dat$Genus_species <- paste(dat$Genus, dat$Species, sep = " ")

str(dat)

dat <- droplevels(dat)
#
library(ggplot2)

ggplot(data = dat)+ geom_histogram(aes(x = SVL), colour="black", fill="gray67")+
  facet_wrap(~Genus_species, scale = "free_y") + theme_bw()+
  theme(axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 12, color = "black"), 
        axis.title.x =element_text(size = 12, color = "black"),
        legend.title =element_text(size = 12, color = "black"),
        legend.text =element_text(size = 12, color = "black"),
        strip.background = element_rect("white"),
        strip.text = element_text(size = 8, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

ggsave("Species_SVL.pdf", height = 6, width = 10)


#-----

dat$prev <- ifelse(dat$Zoospore_load_NEW > 0, 1, 0)

dat$yr_season <- paste(dat$yr, dat$season, sep = "_")

str(dat)

library(plyr)

prev2 <- ddply(.data = dat, .variable = c("yr_season"), .fun = summarize,
      prev = sum(prev, na.rm = TRUE)/sum(Num),
      abundance = sum(Num))

mean(prev2[,2])
sd(prev2[,2])/sqrt(length((prev2[,2])
))
