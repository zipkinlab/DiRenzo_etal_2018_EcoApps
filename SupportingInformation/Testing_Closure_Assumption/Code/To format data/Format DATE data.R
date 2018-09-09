#-----------------------------#
#-----------------------------#
#--------- Download data
#-----------------------------#
#-----------------------------#

library(reshape)

library(reshape2)

library(plyr)

library(lubridate)

# Load data
frog  	<- 	read.csv(file="/Users/Cici/GitHub/2010to2014ElCope/Cope_2008to2011.csv")

# Format the transect names- there should only be 7
frog$trans <- gsub("Loop_Stream", "LoopStream", frog$trans)
frog$trans <- gsub("Silensiosa", "Silenciosa", frog$trans)

# Only keep frog and salamander records
# Removing snake and lizard records
frog <- frog[frog$Group == "Frog" | frog$Group == "Salamander", ]

# Attach a 0 in front of days less than 10
frog$day <- ifelse(frog$day < 10, paste0("0", frog$day), frog$day)

# Attach a 0 in front of months less than 10
frog$mo <- ifelse(frog$mo < 10, paste0("0", frog$mo), frog$mo)

# Create composite variables of season_year, Genus_species, transect_date, month_day_year, and Julian date
frog$season_yr <- paste(frog$season, frog$yr, sep = "_")
frog$Genus_species <- paste(frog$Genus, frog$Species, sep = "_")
frog$transect_date <- paste(frog$trans, frog$day, frog$mo, frog$yr, sep = "_")

frog$m_d_y <- as.Date(paste(frog$mo, frog$day, frog$yr, sep = "/"), "%m/%d/%Y")

frog$Julian_date <- yday(frog$m_d_y)

#------ format zoospore data
# Copy the frog data
frog3 <- frog

# Copy the zoospore column
frog3$ZL <- frog3$Largest.Zoo.Estimate

# replace all NAs with "-2"
frog3[is.na(frog3$ZL) == TRUE, c("ZL")] <- -2

# create empty vector to store new data
Pos_neg <- numeric(length(frog3$Largest.Zoo.Estimate))

# Label each individual
  # positive
  # negative
  # or NA
for(i in 1:length(Pos_neg)){
  
  if(frog3$ZL[i] > 0){Pos_neg[i] <- 1}
  if(frog3$ZL[i] == 0){Pos_neg[i] <- 0}
  if(frog3$ZL[i] < 0){Pos_neg[i] <- NA}
  
}

# Add it as a new column
frog3$Pos_neg <- Pos_neg

# Copy frog3 back to frog
frog <- frog3

#------ Format habitat data
# 4 streams = Cascada, LoopStream, Guabal, and Silenciosa
# 3 trails = Guabal, LoopTrail, MainTrail
habitat <- ifelse(c(frog$trans == "Cascada"| frog$trans == "Guabal" | frog$trans == "LoopStream" |frog$trans == "Silenciosa"),"stream", "trail")

# Add as a factor
frog$habitat <- as.factor(habitat) 

# Create new column with meter column duplicated
frog$mtr_new <- frog$mtr

# Find the sites in streams, where a frog was found at the 200 meter marker
sub <- which(frog$habitat == "stream" & frog$mtr == 200)

# Now for all those sites, subtract 10 to put them at 190
frog[sub,]$mtr_new <- frog[sub,]$mtr_new - 10

# Round all the meters down to the tens position.
frog$round_mtr <- round_any(frog$mtr_new, 10, f = floor)

# Remove all records with no meter information
frog <- frog[is.na(frog$round_mtr) == FALSE,]

#----- Placing individuals into 20 meter sections

# All transects can run from 0 to 380 by 10 meters
true <- seq(from = 0, to = 390, by = 10)

# The new transcect sites will break each transect into 20 meter sites
# Repeat it twice so it is the same length as the "true" vector
# And you will know which section each individual will be sorted into
new <- rep(seq(from = 0, to = 380, by = 20), each = 2)

# New vector that will classify each from into it's section
new_mtr <- numeric(nrow(frog))

# Here we use a loop to go through each individual and each meter to classify what section they are in
for(i in 1:nrow(frog)){       # for each frog
  for(j in 1:length(true)){   # And for each meter 
    
    # If the frog meter matches the true meter section
    # then in the new meter vector, store the new meter section
    if(frog$round_mtr[i] == true[j]){new_mtr[i] <- new[j]}
  }
}

# Add the new meter sections to the data file
frog$new_mtr <- new_mtr

# Create a composite vector with the transect and new meter sections
frog$trans_mtr <- paste(frog$trans, frog$new_mtr, sep = "_")

# Now create a vector with all possible transect meter levts
trans_mtr_levels <- c(
  paste(rep("Cascada", times = 10), seq(from = 0, to = 190, by=20), sep = "_"), 
  paste(rep("Guabal", times = 10), seq(from = 0, to = 190, by=20), sep = "_"),
  paste(rep("LoopStream", times = 10), seq(from = 0, to = 190, by=20), sep = "_"), 
  paste(rep("Silenciosa", times = 10), seq(from = 0, to = 190, by=20), sep = "_"), 
  paste(rep("MainTrail", times = 20), seq(from = 0, to = 390, by=20), sep = "_"),
  paste(rep("LoopTrail", times = 19), seq(from = 0, to = 360, by=20), sep = "_"),
  paste(rep("Verrugosa", times = 20), seq(from = 0, to = 390, by=20), sep = "_"))

# Now make sure that the frog$trans_mtr variable has the same factors as all possible combinations
frog$trans_mtr <- factor(frog$trans_mtr, levels = trans_mtr_levels)

# Format the date
frog$time_date <- paste(frog$mo, frog$day, frog$yr, sep = "_")

frog$time_date <- as.factor(frog$time_date)

# Combine the transect and date vectors
trans_date <- paste(frog$trans, frog$time_date, sep = "_")

# Make it into a factor
frog$trans_date <- as.factor(trans_date)

# Find all the unique transect date combinations to determine when each transect was surveyed
TD <- unique(trans_date)

# Extract all the unique stream surveys
stream <- c(grep("Cascada", TD), grep("Guabal", TD), grep("LoopStream", TD), grep("Silenciosa", TD)) 

# Subset only the stream surveys
TD_stream <- TD[stream]

# Extract all the unique trail surveys
trail <- c(grep("MainTrail", TD), grep("LoopTrail", TD), grep("Verrugosa", TD))

# Subset only the trail surveys
TD_trail <- TD[trail]

# Now we are going to create a vector with EACH 20 METER SECTION having a unique survey
stream_surveys <- numeric(length(TD_stream) * 10)

# Create indices to help store the information
save <- c(1, (seq(from = 10, to = length(TD_stream) * 10, by = 10)+1))
save2 <- seq(from = 10, to = length(TD_stream) * 10, by = 10)

# Use a loop to create every transect_date and meter combination
for(i in 1:length(save2)){   # for each unique survey
  # Paste transect_date and the meter section
  stream_surveys[save[i]: save2[i]] <- paste(rep(TD_stream[i], times = 10), seq(from = 0, to = 180, by=20), sep = "_")
}

# Doing the same for trails
# we are going to create a vector with EACH 20 METER SECTION having a unique survey
trail_surveys <- numeric(length(TD_trail) * 20)

# Create indices to help store the information
T_save <- c(1, (seq(from = 20, to = length(TD_trail) * 20, by = 20)+1))
T_save2 <- seq(from = 20, to = length(TD_trail) * 20, by = 20)

# Use a loop to create every transect_date and meter combination
for(i in 1:length(T_save2)){   # for each unique survey
  # Paste transect_date and the meter section
  trail_surveys[T_save[i]: T_save2[i]] <- paste(rep(TD_trail[i], times = 20), seq(from = 0, to = 380, by=20), sep = "_")
}

# Remove Looptrail at meter 380 = not real
trail_surveys <- trail_surveys[-grep("^LoopTrail_.+_380$", trail_surveys)]

# All 20 meter sections that were ever surveyed.
surveys <- c(stream_surveys, trail_surveys)

# Create a variable with the transect, time, date, and round_meter in the frog file (similar to what was just created)
frog$seen <- as.factor(paste(frog$trans, frog$time_date, frog$round_mtr, sep = "_"))

# Make the surveys into a data frame
surv <- data.frame(surveys)

# Now we will decompose the surveys to be able to look at specifics
# Decompose transect_meter
trans_mtr <- gsub(pattern= "_.+_" , replacement= "_" , surv[,1])

# Decompost transect date
trans_date <- gsub(pattern= "_[[:digit:]]*$" , replacement= "" , surv[,1])

# Decompose time and date
time_date <- sub(pattern = "[A-z]*_", "", trans_date)

# Find what rows correspond to specific season and years
wet_2008 <- grep(pattern = "_2008*$", time_date)
wet_2010 <- grep(pattern = "_2010*$", time_date)
wet_2011 <- grep(pattern = "_2011*$", time_date)

# Create an empty vector to store season and year
season_yr <- numeric(length(date))

# Put name into each specific row from above
season_yr[wet_2008] <- "wet_2008"
season_yr[wet_2010] <- "wet_2010"
season_yr[wet_2011] <- "wet_2011"

# Create a vector with just year
yr <- gsub(pattern= "^[a-zA-Z]*_" , replacement= "" , season_yr)

# Combine all the variables we just decomposed:
# Survey combo, transect meter, transect date, time date, season year, and year
combin <- data.frame(surv, trans_mtr, trans_date, time_date, season_yr,   yr)

# Format the month, day, and year variable
combin$m_d_y <- as.Date(gsub("_", "/", combin$time_date), "%m/%d/%Y")

# Create Julian date
combin$Julian_date <- yday(combin$m_d_y)

# Now we are back to the original data set

# Make sure that season_yr is a factor
frog$season_yr <- as.factor(frog$season_yr)

# Make the year a factor
frog$yr <- as.factor(frog$yr)



#----- Start here to create other years


# Merge the combin file (all possible site surveys) with the frog file
Frog_data <- merge(combin, frog, all = T)

# Here specify which each we want to work with
# Frog_data <- Frog_data[Frog_data$season_yr == "wet_2008",]
Frog_data <- Frog_data[Frog_data$season_yr == "wet_2010",]
#Frog_data <- Frog_data[Frog_data$season_yr == "wet_2011",]

# Format the zoospore data
# Copy the zoospore data
Frog_data$Zoospore_load_NEW2 <- Frog_data$Largest.Zoo.Estimate

# If the zoospore info is not avaible, put a 0 there
Frog_data[is.na(Frog_data$Zoospore_load_NEW2) == TRUE,]$Zoospore_load_NEW2 <- 0

# Figure out if the frog was present or absent
# If the species vector has an NA, put a 0 = absent, if frog present = 1
Frog_data$P_A <- ifelse(is.na(Frog_data$Species), 0, 1)

# Now join P_A and Pos_neg
Infected <- cbind(Frog_data$P_A, Frog_data$Pos_neg)

# Sum the P_A and Pos_neg
  # 0 + 0 = Absent
  # 1 + 0 = Present; not infected
  # 1 + NA = Present; unknown
  # 1 + 1 = Present; infected
IN <- rowSums(Infected, na.rm = T)

# If IN is greater than 1 = Present; infected
Frog_data$IN <- ifelse(IN > 1, 1, 0)

# Copy Zoospore values to new column
Frog_data$no <- Frog_data$Largest.Zoo.Estimate

# If zoospores = NA, put a -100 there
Frog_data[is.na(Frog_data$no) == TRUE, c("no")] <- -100

# If no = -100 and P_A = 1; then frog present but unknown
Frog_data$NAN <- ifelse(Frog_data$no == -100 & Frog_data$P_A == 1, 1, 0)

# If zoospore is > 0 and P_A == 1; present and infected
Frog_data$IN2 <- ifelse(Frog_data$no >0 & Frog_data$P_A == 1, 1, 0)

# If zoospore is == 0 and P_A == 1; present and not infected
Frog_data$NOT <- ifelse(Frog_data$no == 0 & Frog_data$P_A == 1, 1, 0)

# Put the dates into the right order for the columns
Frog_data$time_date <- factor(Frog_data$time_date, levels = 
  c(levels(Frog_data$time_date)[grep("_2008$", levels(Frog_data$time_date))],
    levels(Frog_data$time_date)[grep("_2010$", levels(Frog_data$time_date))],
    levels(Frog_data$time_date)[grep("_2011$", levels(Frog_data$time_date))]
  )
)

# Create the long format of the data

# Not infected "NOT"
# Infected     "IN2"
# Unknown      "NAN"
# Zoospore load: "Zoospore_load_NEW2"

# date = Julian_date

# Melt the data first into long format
Frog_all <- melt(Frog_data, id.vars = c("trans_mtr", "time_date"), measure.vars = c("Julian_date"))

# Convert to wide format
# Because you can not have the sum of all frogs + know when surveys aren't done, we need to do this a round about way, because it does tell you the average number of frogs encountered and when a survey was not done
Frog_ALL_1 <- dcast(Frog_all, trans_mtr ~ time_date, mean)

# Sum up the number of frogs found
Frog_ALL_2 <- dcast(Frog_all, trans_mtr ~ time_date, sum)
  Frog_ALL_2[Frog_ALL_2 > 0] <- 1

# Save the site names
site_names <- Frog_ALL_2[,1] 

# Multiply the matricies to find the total number of frogs found and not count it when there was no survey
Frog_ALL_1 <- Frog_ALL_1[,-1] * Frog_ALL_2[,-1]

# Remove entire NA rows
Frog_ALL_1 <- Frog_ALL_1[,colSums(is.na(Frog_ALL_1)) != nrow(Frog_ALL_1)]

# Make the NaN to NA
Frog_ALL_1[is.na(Frog_ALL_1)] <- NA

# We want to get rid of all the NAs we don't need
# Create new matrix to store data
WET12 <- matrix(NA, nrow = nrow(Frog_ALL_1), ncol = ncol(Frog_ALL_1))

# This loop moves all the values over to the left most
a <- 1
for(i in 1:nrow(Frog_ALL_1)){
  a <- 1
  for(j in 1:ncol(Frog_ALL_1)){
    if(is.na(Frog_ALL_1[i,j]) == F){WET12[i, a] <- Frog_ALL_1[i,j];
    a <- a + 1}
  }	
}

# This calculates the greatest number of columns with values
great <- length(which(colSums(WET12) >0 |colSums(WET12, na.rm = T) == 0 ))

# Create column names
colnames(WET12) <- paste("survey", seq(from = 1, to = ncol(WET12)), sep = "_")

# Add site names to rows
rownames(WET12) <- site_names

# Subset to column 8
W12 <- WET12[,1:5]

# Save the transformed data :)
write.csv(W12, 
          file = 
"/Users/Cici/GitHub/2010to2014ElCope/Closure Assumption/DATA Survey dates/DATE_WET_2010.csv")

# Fin

