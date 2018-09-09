#------ Read in the data

#--- These files have the average pathogen load of individuals in the survey
W08P <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/II_WET_2008.csv")[,-1]
W10P <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/II_WET_2010.csv")[,-1]
W11P <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/II_WET_2011.csv")[,-1]
W12P <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/II_14-01-2016_WET_2012.csv")[,-1]
D13P <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/II_14-01-2016_DRY_2013.csv")[,-1]
W13P <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/II_14-01-2016_WET_2013.csv")[,-1]
D14P <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/II_14-01-2016_DRY_2014.csv")[,-1]


# ------ Bundle data
# Initial host values
R <- nrow(W12P)
T <- ncol(W12P)
K <- 8

#-------------- Not infected Host 
D13P <- data.frame(D13P, survey_8 = rep(NA, times = nrow(D13P)))
W13P <- data.frame(W13P, survey_8 = rep(NA, times = nrow(W13P)))
D14P <- data.frame(D14P)

dim(W10P)
dim(W11P)
dim(W12P)
dim(D13P)
dim(W13P)
dim(D14P)

#-------------

II <- array(NA, dim = c(R, T, K))

II[,,1] <- as.matrix(W08P)
# 2009 data missing
II[,,3] <- as.matrix(W10P)
II[,,4] <- as.matrix(W11P)
II[,,5] <- as.matrix(W12P)
II[,,6] <- as.matrix(D13P)
II[,,7] <- as.matrix(W13P)
II[,,8] <- as.matrix(D14P)

II2 <- log(II+0.01)
  # Same transformation as in the Double swab data

II_log <- log10(II+1)

II_log[is.na(II_log) == TRUE] <- mean(II_log, na.rm = T)

#------ Detection infection intensity----- z-score

hist()

II_mean <- mean(II2[,,], na.rm = T)

II_sd <- sd(II2[,,], na.rm = T)

II3 <- (II2-II_mean)/II_sd

hist(II3)
hist(II2)

#------ calculate theta

# From Miller et al. 2012 & DiRenzo et al. in prep = 
# probability of detecting the pathogen on an infected individual = 
# probability of correctly identifying an infected individual

# 1 - pd = misclassfied

alpha_I <- 0.77
  #4.744
# -0.19
# Miller = 

beta_I <- 0.32
  #2.320
# 0.69
# Miller = 

theta <- array(NA, dim = c(dim(II2)))

for(i in 1:dim(II2)[1]){
  for(j in 1:dim(II2)[2]){
    for(t in 1:dim(II2)[3]){
      theta[i, j, t] <- (1- plogis(alpha_I + beta_I * II2[i,j,t]))
    }
  }
}

Theta_mean <- mean(theta[,,], na.rm = T)

theta[is.na(theta) == TRUE] <- Theta_mean
II2[is.na(II2) == TRUE] <- II_mean


