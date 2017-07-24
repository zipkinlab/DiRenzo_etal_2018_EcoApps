#------ Testing closure assumption between surveys
# We have 6 seasons
  # 2010 wet
  # 2011 wet
  # 2012 wet
  # 2013 dry
  # 2013 wet
  # 2014 dry

# Read in the data
#------ Read in the data

W10 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Closure Assumption/DATA Survey dates/DATE_WET_2010.csv")[,-1]
W11 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Closure Assumption/DATA Survey dates/DATE_WET_2011.csv")[,-1]
W12 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Closure Assumption/DATA Survey dates/DATE_WET_2012.csv")[,-1]
D13 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Closure Assumption/DATA Survey dates/DATE_DRY_2013.csv")[,-1]
W13 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Closure Assumption/DATA Survey dates/DATE_WET_2013.csv")[,-1]
D14 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Closure Assumption/DATA Survey dates/DATE_DRY_2014.csv")[,-1]

# ------ Bundle data
# Initial host values
R <- nrow(W12)
T <- ncol(W12)
K <- 6

#-------------- Not infected Host 

W10 <- data.frame(W10, 
                  survey_6 = rep(NA, times = nrow(W10)),
                  survey_7 = rep(NA, times = nrow(W10)),
                  survey_8 = rep(NA, times = nrow(W10)))
W11 <- data.frame(W11, 
                  survey_6 = rep(NA, times = nrow(W11)),
                  survey_7 = rep(NA, times = nrow(W11)),
                  survey_8 = rep(NA, times = nrow(W11)))


dim(W10)
dim(W11)
dim(W12)
dim(D13)
dim(W13)
dim(D14)

#-------------

host <- array(NA, dim = c(R, T, K))

host[,,1] <- as.matrix(W10)
host[,,2] <- as.matrix(W11)
host[,,3] <- as.matrix(W12)
host[,,4] <- as.matrix(D13)
host[,,5] <- as.matrix(W13)
host[,,6] <- as.matrix(D14)

# Calculate the number of days between surveys

host

# Surveys done over 3 nights
host[,,1]
host[,,2]

# First 3 columns lump together; last 4 columns lump together
rowMeans(host[,4:8,3], na.rm = TRUE) - rowMeans(host[,1:3,3])
rowMeans(host[,4:8,4], na.rm = TRUE) - rowMeans(host[,1:3,4])

# This one is weird
rowMeans(host[1:10,5:8,5], na.rm = TRUE) - rowMeans(host[1:10,1:4,5])
rowMeans(host[11:30,4:8,5], na.rm = TRUE) - rowMeans(host[11:30,1:3,5])
rowMeans(host[31:49,5:8,5], na.rm = TRUE) - rowMeans(host[31:49,1:4,5])
rowMeans(host[50:69,6:8,5], na.rm = TRUE) - rowMeans(host[50:69,1:5,5])
rowMeans(host[70:99,4:8,5], na.rm = TRUE) - rowMeans(host[70:99,1:3,5])

# First 4 columns lump together; last 3 columns lump together
rowMeans(host[,5:8,6], na.rm = TRUE) - rowMeans(host[,1:4,6])




# Run File:
  # 10to14_Model_format.R





XX <- apply(All, c(1, 3), sum, na.rm = TRUE)

# First 3 columns lump together; last 4 columns lump together
wet_12 <- rowMeans(All[,4:8,3], na.rm = TRUE) - rowMeans(All[,1:3,3])
dry_13 <- rowMeans(All[,4:8,4], na.rm = TRUE) - rowMeans(All[,1:3,4])

# This one is weird
wet_13 <- rowMeans(All[1:10,5:8,5], na.rm = TRUE) -  rowMeans(All[1:10,1:4,5])
wet_13 <- c(wet_13, rowMeans(All[11:30,4:8,5], na.rm = TRUE) - rowMeans(All[11:30,1:3,5]))
wet_13 <- c(wet_13, rowMeans(All[31:49,5:8,5], na.rm = TRUE) - rowMeans(All[31:49,1:4,5]))
wet_13 <- c(wet_13, rowMeans(All[50:69,6:8,5], na.rm = TRUE) - rowMeans(All[50:69,1:5,5]))
wet_13 <- c(wet_13, rowMeans(All[70:99,4:8,5], na.rm = TRUE) - rowMeans(All[70:99,1:3,5]))

# First 4 columns lump together; last 3 columns lump together
dry_14 <- rowMeans(All[,5:8,6], na.rm = TRUE) - rowMeans(All[,1:4,6])

#---- Averages
mu <- matrix(NA, ncol = 3, nrow = 4)
colnames(mu) <- c("Year", "Mean", "Standard_deviation")

mu[,1] <- c("wet_12", "dry_13", "wet_13", "dry_14")

mu[1,2] <- mean(wet_12)
mu[1,3] <- sd(wet_12)

mu[2,2] <- mean(dry_13)
mu[2,3] <- sd(dry_13)
mu[3,2] <-mean(wet_13)
mu[3,3] <- sd(wet_13)
mu[4,2] <-mean(dry_14)
mu[4,3] <- sd(dry_14)

mu <- as.data.frame(mu)
mu[,2] <- as.numeric(as.character(mu[,2]))
mu[,3] <- as.numeric(as.character(mu[,3]))

tab <- mu
#------- 4 plots
# Infecteds

par(mfrow = c(2,2))
hist(wet_12, 
     xlab = "Difference in observed abundance Wet 2012",
     main = "", las = 1)
text(x = -2, y = 35, lab = "A", cex = 2)     
text(x = 2, y = 35, lab = expression(paste(mu, " = ", -0.04)))
text(x = 2, y = 32, lab = expression(paste(sigma^2, " = ", 0.81)))

hist(dry_13, 
     xlab = "Difference in observed abundance Dry 2013",
     main = "", las = 1)
text(x = -1.25, y = 53, lab = "B", cex = 2)     
text(x = 1, y = 53, lab = expression(paste(mu, " = ", 0.07)))
text(x = 1, y = 50, lab = expression(paste(sigma^2, " = ", 0.41)))

hist(wet_13, 
     xlab = "Difference in observed abundance Wet 2013",
     main = "", las = 1)
text(x = -3.5, y = 50, lab = "C", cex = 2)     
text(x = 2, y = 50, lab = expression(paste(mu, " = ", 0.01)))
text(x = 2, y = 47, lab = expression(paste(sigma^2, " = ", 0.74)))

hist(dry_14, 
     xlab = "Difference in observed abundance Dry 2014",
     main = "", las = 1, xlim = c(-2, 3))
text(x = -1.5, y = 40, lab = "D", cex = 2)     
text(x = 2, y = 40, lab = expression(paste(mu, " = ", 0.07)))
text(x = 2, y = 37, lab = expression(paste(sigma^2, " = ", 0.52)))


