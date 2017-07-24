
library(ape)
# Load the data
#### Need to run 
  # 10to14_Model_format.R

## All has all hosts captured

All2 <- apply(All, c(1, 3), max, na.rm = TRUE)
All2[All2 == "-Inf"] <- NA

#-- Create matrix that tells you who is next to who

ENV <- read.csv("/Users/Cici/Dropbox/PhD_cope/Chpt2/Data/Cope_data_FINAL/ALL_site_data/site.csv")
# All 1 meter location coordinates of entire site

head(ENV)

site_names <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/IN_WET_2010.csv")[,1]

#-----

dat <- data.frame(site_names = site_names, All2 = All2)
colnames(dat) <- c("tran_mtr", "2010W", "2011W", "2012W", "2013D", "2013W", "2014D")

dat2 <- merge(dat, ENV, by = "tran_mtr")
names(dat2)[10] <- "Lon"
names(dat2)[11] <- "Lat"

dists <- as.matrix(dist(cbind(dat2$Lon, dat2$Lat)))

dists.inv <- 1/dists
diag(dists.inv) <- 0

Moran.I(dat2$'2010W', dists.inv, na.rm = TRUE)
Moran.I(dat2$'2011W', dists.inv, na.rm = TRUE)
Moran.I(dat2$'2012W', dists.inv, na.rm = TRUE)
Moran.I(dat2$'2013D', dists.inv, na.rm = TRUE)
Moran.I(dat2$'2013W', dists.inv, na.rm = TRUE)
Moran.I(dat2$'2014D', dists.inv, na.rm = TRUE)
# null hypothesis = there is no spatial autocorrelation
# p > 0.05 = accept null
# p < 0.05 = reject null