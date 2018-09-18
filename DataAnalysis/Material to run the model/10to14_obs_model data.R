#------ Read in the data
W10_obs <- read.csv(file = "/Users/Cici/GitHub/2000to2014ElCope/Data_Model files/obs_WET_2010.csv")[-48,-1]
W11_obs <- read.csv(file = "/Users/Cici/GitHub/2000to2014ElCope/Data_Model files/obs_WET_2011.csv")[-48,-c(1, 9, 10, 11)]
W12_obs <- read.csv(file = "/Users/Cici/GitHub/2000to2014ElCope/Data_Model files/obs_WET_2012.csv")[-48,-c(1, 9, 10, 11)]
D13_obs <- read.csv(file = "/Users/Cici/GitHub/2000to2014ElCope/Data_Model files/obs_DRY_2013.csv")[-48,-c(1, 9, 10, 11)]
W13_obs <- read.csv(file = "/Users/Cici/GitHub/2000to2014ElCope/Data_Model files/obs_WET_2013.csv")[-48,-c(1, 9, 10, 11)]
D14_obs <- read.csv(file = "/Users/Cici/GitHub/2000to2014ElCope/Data_Model files/obs_DRY_2014.csv")[-48,-c(1, 9, 10, 11)]

# ------ Bundle data

#-------------- Not infected Host 
W10_obs <- data.frame(W10_obs, 
                  survey_7 = rep(mean(apply(W10_obs, c(2), mean)), times = nrow(W10_obs)),
                  survey_8 = rep(mean(apply(W10_obs, c(2), mean)), times = nrow(W10_obs)))

dim(W10_obs)
dim(W11_obs)
dim(W12_obs)
dim(D13_obs)
dim(W13_obs)
dim(D14_obs)


#-------------
# Initial host values
R <- nrow(W12_obs)
T <- ncol(W12_obs)
K <- 6

obs <- array(NA, dim = c(R, T, K))

obs[,,1] <- as.matrix(W10_obs)
obs[,,2] <- as.matrix(W11_obs)
obs[,,3] <- as.matrix(W12_obs)
obs[,,4] <- as.matrix(D13_obs)
obs[,,5] <- as.matrix(W13_obs)
obs[,,6] <- as.matrix(D14_obs)


obs[which(is.na(obs) == TRUE)] <- mean(apply(obs, c(1), mean, na.rm = TRUE))
