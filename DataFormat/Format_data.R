#-----------------------------#
#-----------------------------#
#--------- Download data
#-----------------------------#
#-----------------------------#

library(reshape)

library(reshape2)

library(plyr)

library(lubridate)

frog  	<- 	read.table(file="/Users/Cici/Dropbox/PhD_cope/Chpt2/Data/Cope_data_FINAL/Cope2012_2014.txt", header = T, sep = "\t")

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

#------------

true <- seq(from = 0, to = 380, by = 10)

new <- rep(seq(from = 0, to = 380, by = 20), each = 2)

new_mtr <- numeric(nrow(frog))

for(i in 1:nrow(frog)){
  for(j in 1:length(true)){
    if(frog$round_mtr[i] == true[j]){new_mtr[i] <- new[j]}
  }
}

frog$new_mtr <- new_mtr

cbind(frog$new_mtr, frog$round_mtr)

frog$trans_mtr <- paste(frog$trans, frog$new_mtr, sep = "_")


#--------

str(frog)

frog$Genus_species


share <- as.numeric(
unique(frog[frog$habitat == "trail",]$Species) %in% unique(frog[frog$habitat == "stream",]$Species)
)


share <- 
  sum(as.numeric(
  unique(frog[frog$season == "wet",]$Species) %in% unique(frog[frog$season == "dry",]$Species)
))


#--------


trans_mtr_levels <- c(
  paste(rep("Cascada", times = 10), seq(from = 0, to = 190, by=20), sep = "_"), 
	paste(rep("Guabal", times = 10), seq(from = 0, to = 190, by=20), sep = "_"),
	paste(rep("LoopStream", times = 10), seq(from = 0, to = 190, by=20), sep = "_"), 
	paste(rep("Silenciosa", times = 10), seq(from = 0, to = 190, by=20), sep = "_"), 
	paste(rep("MainTrail", times = 20), seq(from = 0, to = 390, by=20), sep = "_"),
	paste(rep("LoopTrail", times = 19), seq(from = 0, to = 360, by=20), sep = "_"),
	paste(rep("Verrugosa", times = 20), seq(from = 0, to = 390, by=20), sep = "_"))

frog$trans_mtr <- factor(frog$trans_mtr, levels = trans_mtr_levels)

frog <- frog[ is.na(frog$trans_mtr) == F & is.na(frog$AM_PM) == F,]


frog$time_date <- paste(frog$AM_PM, frog$mo, frog$day, frog$yr, sep = "_")

frog$time_date <- as.factor(frog$time_date)

str(frog)


# Paste genus and species

trans_date <- paste(frog$trans, frog$time_date, sep = "_")

frog$trans_date <- as.factor(trans_date)

TD <- unique(trans_date)

stream <- c(grep("Cascada", TD), grep("Guabal", TD), grep("LoopStream", TD), grep("Silenciosa", TD)) 

TD_stream <- TD[stream]

trail <- c(grep("MainTrail", TD), grep("LoopTrail", TD), grep("Verrugosa", TD))

TD_trail <- TD[trail]

stream_surveys <- numeric(length(TD_stream) * 10)
	# Stream_time_date * 21 sections in each stream time date

save <- c(1, (seq(from = 10, to = length(TD_stream) * 10, by = 10)+1))

save2 <- seq(from = 10, to = length(TD_stream) * 10, by = 10)

for(i in 1:length(save2)){

	stream_surveys[save[i]: save2[i]] <- paste(rep(TD_stream[i], times = 10), seq(from = 0, to = 180, by=20), sep = "_")

}

trail_surveys <- numeric(length(TD_trail) * 20)

T_save <- c(1, (seq(from = 20, to = length(TD_trail) * 20, by = 20)+1))

T_save2 <- seq(from = 20, to = length(TD_trail) * 20, by = 20)

for(i in 1:length(T_save2)){

	trail_surveys[T_save[i]: T_save2[i]] <- paste(rep(TD_trail[i], times = 20), seq(from = 0, to = 380, by=20), sep = "_")

}

surveys <- c(stream_surveys, trail_surveys)
	# All 10 meter sections that were ever surveyed.

frog$seen <- as.factor(paste(frog$trans, frog$time_date, frog$round_mtr, sep = "_"))

surv <- data.frame(surveys)


surv

trans_mtr <- gsub(pattern= "_.+_" , replacement= "_" , surv[,1])

time_date <- gsub(pattern= "_[[:digit:]]*$" , replacement= "" , surv[,1])
time_date <- gsub(pattern= "^[a-zA-Z]*_" , replacement= "" , time_date)

AM_PM <- gsub(pattern= "_[[:digit:]]*_[[:digit:]]*_[[:digit:]]*$" , replacement= "" , time_date)
date <- sub(pattern = "[A-Z]*_", "", time_date)


wet_2012 <- grep(pattern = "[6-8]_.+_2012", date)
dry_2013 <- grep(pattern = "[0-5]_.+_2013", date)
wet_2013 <- grep(pattern = "[6-8]_.+_2013", date)
dry_2014 <- grep(pattern = "[0-5]_.+_2014", date)

season_yr <- numeric(length(date))


season_yr[wet_2012] <- "wet_2012"
season_yr[dry_2013] <- "dry_2013"
season_yr[wet_2013] <- "wet_2013"
season_yr[dry_2014] <- "dry_2014"

yr <- gsub(pattern= "^[a-zA-Z]*_" , replacement= "" , season_yr)

combin <- data.frame(surv, trans_mtr, time_date, season_yr, AM_PM, date, yr)

str(combin)

combin$m_d_y <- as.Date(gsub("_", "/", combin$date), "%m/%d/%Y")

combin$Julian_date <- yday(combin$m_d_y)


for(i in 1:nrow(combin)){
  if(combin$yr[i] == 2013){combin$Julian_date[i] <- combin$Julian_date[i] + 365
  
  } else if(combin$yr[i] == 2014){combin$Julian_date[i] <- combin$Julian_date[i] + 730
  
  } else {combin$Julian_date[i] <- combin$Julian_date[i]}
}


#------- Need to delete row # 80 Loop trail 380



Frog_data <- merge(combin, frog, all = T)

#Frog_data <- Frog_data[Frog_data$season_yr == "wet_2012",]
#Frog_data <- Frog_data[Frog_data$season_yr == "dry_2013",]
#Frog_data <- Frog_data[Frog_data$season_yr == "wet_2013",]
Frog_data <- Frog_data[Frog_data$season_yr == "dry_2014",]
Frog_data$Zoospore_load_NEW2 <- Frog_data$Zoospore_load_NEW

Frog_data[is.na(Frog_data$Zoospore_load_NEW2) == TRUE,]$Zoospore_load_NEW2 <- 0

str(Frog_data)


Frog_data$P_A <- ifelse(is.na(Frog_data$Species), 0, 1)

Infected <- cbind(Frog_data$P_A, Frog_data$Pos_neg)

IN <- rowSums(Infected, na.rm = T)

Frog_data$IN <- ifelse(IN > 1, 1, 0)

Frog_data$no <- Frog_data$Zoospore_load_NEW

Frog_data[is.na(Frog_data$no) == TRUE, c("no")] <- -100

Frog_data$NAN <- ifelse(Frog_data$no == -100 & Frog_data$P_A == 1, 1, 0)

Frog_data$IN2 <- ifelse(Frog_data$no >0 & Frog_data$P_A == 1, 1, 0)

Frog_data$NOT <- ifelse(Frog_data$no == 0 & Frog_data$P_A == 1, 1, 0)

length(which(is.na(Frog_data$IN) == TRUE))

Frog_data <- Frog_data[Frog_data$AM_PM == "PM",]

Frog_data <- droplevels(Frog_data)

str(Frog_data)

Frog_data$time_date <- factor(Frog_data$time_date, levels = 
  c(levels(Frog_data$time_date)[grep("_2012$", levels(Frog_data$time_date))],
  levels(Frog_data$time_date)[grep("_2013$", levels(Frog_data$time_date))],
  levels(Frog_data$time_date)[grep("_2014$", levels(Frog_data$time_date))]
  )
)

xy <- ddply(.data = Frog_data, .variable = "Species", .fun = summarize, num_sp = length((Species)))

xy <- ddply(.data = Frog_data, .variable = "Genus_species", .fun = summarize, num_sp = length((Species)), zoo_mean = mean(Zoospore_load_NEW, na.rm = T), no_po = length(which(Zoospore_load_NEW > 0)), no_neg = length(which(Zoospore_load_NEW == 0)), prev = length(which(Zoospore_load_NEW > 0))/ length(Zoospore_load_NEW))

ddply(.data = Frog_data, .variable = "season_yr", .fun = summarize, 
      num_sp = length(unique((Genus_species))))
length(
unique(Frog_data[Frog_data$season == "wet",]$Genus_species) %in%
  unique(Frog_data[Frog_data$season == "dry",]$Genus_species)
)

length(
  unique(Frog_data[Frog_data$habitat == "stream",]$Genus_species) %in%
    unique(Frog_data[Frog_data$habitat == "trail",]$Genus_species)
)

length(which(Frog_data$Zoospore_load_NEW2 < 100))/length(Frog_data$Zoospore_load_NEW2)

#write.csv(xy, "/Users/Cici/Desktop/chpt2_sp.csv")

sum(xy[,2])-1718
# Create Site by Species matrix

# IN = Infected
# NAN = unknown
# NOT = NOT infected
# Infection intensity  = Zoospore_load_NEW2

Frog_all <- melt(Frog_data, id.vars = c("trans_mtr", "time_date"), measure.vars = c("Zoospore_load_NEW2"))


# Convert to long form
library("lmomco")

Frog_ALL_1 <- dcast(Frog_all, trans_mtr ~ time_date, function(x) harmonic.mean(x)$harmean)
  Frog_ALL_1[is.na(Frog_ALL_1) == TRUE] <- 0

Frog_ALL_2 <- dcast(Frog_all, trans_mtr ~ time_date, mean)
  Frog_ALL_2[Frog_ALL_2 > 0] <- 1


site_names <- Frog_ALL_2[,1] 

# Get numeric values

Frog_ALL_1 <- Frog_ALL_1[,-1] * Frog_ALL_2[,-1]

# Remove entire NA rows

Frog_ALL_1 <- Frog_ALL_1[,colSums(is.na(Frog_ALL_1)) != nrow(Frog_ALL_1)]

Frog_ALL_1[is.na(Frog_ALL_1)] <- NA

dim(Frog_ALL_1)

# create new matrix to store data
WET12 <- matrix(NA, nrow = nrow(Frog_ALL_1), ncol = ncol(Frog_ALL_1))

#------------ Wet 12
a <- 1

for(i in 1:nrow(Frog_ALL_1)){
  
  a <- 1
  
  for(j in 1:ncol(Frog_ALL_1)){
    
    if(is.na(Frog_ALL_1[i,j]) == F){WET12[i, a] <- Frog_ALL_1[i,j];

        a <- a + 1}
  
    }	

  }

great <- length(which(colSums(WET12) >0 |colSums(WET12, na.rm = T) == 0 ))

colnames(WET12) <- paste("survey", seq(from = 1, to = ncol(WET12)), sep = "_")

rownames(WET12) <- site_names

W12 <- WET12[,1:8]

W12

write.csv(W12, file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/Harmonic_mean_II_DRY_2014.csv")





#------#------#------#------#------#------
#------ How many days pass between surveys?
#------#------#------#------#------#------


sub <- matrix(NA, nrow = nrow(W12), ncol = ncol(W12))

sub[,1] <- 0

sub[,2:ncol(sub)] <- W12[,2:great] - W12[,1:(great-1)]

colnames(sub) <- colnames(W12)
rownames(sub) <- rownames(W12)


write.csv(sub, file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Data/Formated_sites/ALL_FROGS_days.csv")


#------- Summary stats

str(Frog_data)

Frog_data2 <- data.frame(Frog_data, all = rep("all", times = nrow(Frog_data)))
#----- 
library(plyr)
Frog_data <- Frog_data2
ddply(.data = Frog_data2, .variable = c("Species"), .fun = summarize, sum = length(unique(Species)))
xx <- ddply(.data = Frog_data, .variable = c("Species"), .fun = summarize, sum = sum(P_A))
length(which(xx[,2] < 21))/30
hist(xx[,2], xlim = c(0, 100), breaks = 150, las = 1, xlab = "Total number of captures per species", main = "")
