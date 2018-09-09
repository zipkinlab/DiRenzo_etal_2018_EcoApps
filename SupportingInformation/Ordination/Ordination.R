#-----------------------------#
#-----------------------------#
#--------- Download data
#-----------------------------#
#-----------------------------#

library(reshape)

library(reshape2)

library(plyr)

library(lubridate)

frog  	<- 	read.table(file="/Users/Cici/GitHub/2010to2014ElCope/Ordination/Cope_2010_2014.txt", header = T, sep = "\t")

frog$day <- ifelse(frog$day < 10, paste0("0", frog$day), frog$day)


#-------- add a date column

frog <- frog[frog$Group == "Frog" | frog$Group == "Salamander",]

frog$season_yr <- paste(frog$season, frog$yr, sep = "_")
frog$Genus_species <- paste(frog$Genus, frog$Species, sep = "_")
frog$transect_date_time <- paste(frog$trans, frog$day, frog$mo, frog$yr, frog$AM_PM, sep = "_")

frog <- droplevels(frog)

str(frog)

frog$m_d_y <- as.Date(paste(frog$mo, frog$day, frog$yr, sep = "/"), "%m/%d/%Y")

frog$Julian_date <- yday(frog$m_d_y)

frog <- frog[-which(is.na(frog$yr)),]


for(i in 1:nrow(frog)){
  if(frog$yr[i] == 2013){frog$Julian_date[i] <- frog$Julian_date[i] + 365
  
  } else if(frog$yr[i] == 2014){frog$Julian_date[i] <- frog$Julian_date[i] + 730
  
  } else {frog$Julian_date[i] <- frog$Julian_date[i]}
}



str(frog)

frog3 <- frog

frog3$ZL <- frog3$Zoospore_load_NEW
# Copy zoospore load column

frog3[is.na(frog3$ZL) == TRUE, c("ZL")] <- -2
# replace all NAs with "-2"

Pos_neg <- numeric(length(frog3$Zoospore_load))
# create empty vector

for(i in 1:length(Pos_neg)){
  
  if(frog3$ZL[i] > 0){Pos_neg[i] <- 1}
  
  if(frog3$ZL[i] == 0){Pos_neg[i] <- 0}
  
  if(frog3$ZL[i] < 0){Pos_neg[i] <- NA}
  
}

frog3$Pos_neg <- Pos_neg

frog3$TC <- as.numeric(as.character(frog3$TC))

frog3 <- droplevels(frog3)

frog <- frog3


habitat <- ifelse(c(frog$trans == "Cascada"| frog$trans == "Guabal" | frog$trans == "LoopStream" |frog$trans == "Silenciosa"),"stream", "trail")

frog$habitat <- as.factor(habitat) 
frog$mtr_new <- frog$mtr

sub <- which(frog$habitat == "stream" & frog$mtr == 200)

frog[sub,]$mtr_new <- frog[sub,]$mtr_new - 10

frog$round_mtr <- round_any(frog$mtr, 10, f = floor)


frog <- frog[is.na(frog$round_mtr) == FALSE,]


str(frog)

frog$Present <- rep(1, times = nrow(frog))
# trans, yr, season, habitat

frog <- frog[frog$AM_PM == "PM",]
frog <- droplevels(frog)

str(frog)
frog <- frog[frog$Species != "sp."]
frog <- droplevels(frog)

###----- Transform the data

comm <- melt(frog, id.vars = c("trans", "yr", "season", "habitat", "Genus_species"), measure.vars = c("Present"))

comm_all <- dcast(comm, trans+yr+season+habitat ~ Genus_species, mean)
comm_all[is.na(comm_all) == TRUE] <- 0
comm_all <- droplevels(comm_all)
comm_all <- comm_all[-42,]
species <- comm_all[,-c(1:4)]
species[species > 0] <- 1


#--- Perform the ordination

require(vegan)
nMDS <- metaMDS(comm= species, distance = "jaccard", binary = T, k = 2, trymax = 200, autotransform=TRUE, expand = FALSE, trace = 0, plot = FALSE)

nMDS$stress

#-----


#-----

library(BiodiversityR)
library(plotrix)

par(mfrow = c(2,2))
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 0.5, 0.5))
par(mgp = c(2, 0.6, 0))
par(tcl = -0.25)


#----- Seasons

plot(nMDS$points[,1],nMDS$points[,2],type="n", ylim = c(-2.5, 2.5), xlim = c(-2.5, 2.5), ylab="NMDS2", xlab="NMDS1", xaxt = "n")

ordisymbol(nMDS, comm_all, factor= "season", rainbow=F, legend=F)
legend("topright", c("Dry", "Wet"), pch=c(1,2), cex=1)

with(comm_all, ordiellipse(nMDS, season, kind="sd",conf=0.95, col="black", lwd=3))

mtext("A", side = 3, line =-2.5, adj = 0.1, cex = 2, at = -2)

#-------  Habitat

plot(nMDS$points[,1],nMDS$points[,2],type="n", ylim = c(-2.5, 2.5), xlim = c(-2.5, 2.5), ylab="NMDS2", xlab="NMDS1", yaxt = "n", xaxt = "n")

ordisymbol(nMDS, comm_all, factor= "habitat", rainbow=F, legend=F)
legend("topright", c("Stream", "Trail"), pch=c(1,2), cex=1)

with(comm_all, ordiellipse(nMDS, habitat, kind="sd",conf=0.95, col="black", lwd=3))

mtext("NMDS1", side = 1, outer = TRUE, cex = 1.2, line = 2.2, col = "grey20")
mtext("NMDS2", side = 2, outer = TRUE, cex = 1.2, line = 2.2, col = "grey20")
mtext("B", side = 3, line =-2.5, adj = 0.1, cex = 2, at = -2)

#-------  Year
comm_all$yr_season <- paste(comm_all$yr, comm_all$season, sep = "_")

plot(nMDS$points[,1],nMDS$points[,2], type="n", ylim = c(-2.5, 2.5), xlim = c(-2.5, 2.5), ylab="NMDS2", xlab="NMDS1")

ordisymbol(nMDS, comm_all, factor= "yr_season", rainbow=F, legend=F)
legend("topright", c("2010W", "2011W", "2012W", "2013D", "2013W", "2014D"), pch=c(1:6), cex=1)

with(comm_all, ordiellipse(nMDS, yr_season, kind="sd",conf=0.95, col="black", lwd=3))

mtext("NMDS1", side = 1, outer = TRUE, cex = 1.2, line = 2.2, col = "grey20")
mtext("NMDS2", side = 2, outer = TRUE, cex = 1.2, line = 2.2, col = "grey20")
mtext("C", side = 3, line =-2.5, adj = 0.1, cex = 2, at = -2)

#-------  Transect
plot(nMDS$points[,1],nMDS$points[,2], type="n", ylim = c(-2.5, 2.5), xlim = c(-2.5, 2.5), ylab="NMDS2", xlab="NMDS1", yaxt = "n")

ordisymbol(nMDS, comm_all, factor= "trans", rainbow=F, legend=F)
legend("topright", c("Cascada", "Guabal", "LoopStream", "LoopTrail", "MainTrail", "Silenciosa", 
                     "Verrugosa"), pch=c(1:7), cex=1)

with(comm_all, ordiellipse(nMDS, trans, kind="sd",conf=0.95, col="black", lwd=3))

mtext("NMDS1", side = 1, outer = TRUE, cex = 1.2, line = 2.2, col = "grey20")
mtext("NMDS2", side = 2, outer = TRUE, cex = 1.2, line = 2.2, col = "grey20")
mtext("D", side = 3, line =-2.5, adj = 0.1, cex = 2, at = -2)

#--------- Stats

adonis(species ~ season, data = comm_all, perm=1e3, method = "jaccard")
adonis(species ~ yr_season, data = comm_all, perm=1e3, method = "jaccard")
adonis(species ~ habitat, data = comm_all, perm=1e3, method = "jaccard")
adonis(species ~ trans, data = comm_all, perm=1e3, method = "jaccard")

## Bray-Curtis distances between samples
dis <- vegdist(species)

## Calculate multivariate dispersions
mod <- betadisper(dis, comm_all$yr)
anova(mod)
