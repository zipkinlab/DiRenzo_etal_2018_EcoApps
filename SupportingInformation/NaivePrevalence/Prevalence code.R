# Read in the data
dat <- read.table(file="/Users/Cici/Desktop/Chpt 2/ALL_Cope2008_2014.txt", header = T, sep = "\t")

dat <- dat[dat$AM_PM  == "PM",]
dat <- dat[is.na(dat$SVL)  == FALSE,]
dat <- dat[is.na(dat$Species)  == FALSE,]
dat <- dat[dat$Group == "Frog",]
dat <- dat[dat$Species != "sp.",]

str(dat)

dat$infection <- ifelse(dat$Zoospore_load_NEW > 0, 1, 0)

dat$yr_season <- paste(dat$yr, dat$season, sep = "_")

# Remove species with less than 15 individuals
library(plyr)
spec <- ddply(.data = dat, .variable = c("Species", "yr"), .fun = summarize,
              Prevalence = sum(infection, na.rm = TRUE)/sum(Num, na.rm = TRUE),
              sum = sum(Num))
spec

prev <- spec[spec[,4] > 15, ]

write.csv(prev, file = "/Users/Cici/Desktop/Chpt 2/Eco Apps/Code/prev.csv")
