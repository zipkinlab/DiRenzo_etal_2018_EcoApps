#------ Read in the data

#--- These files have the average pathogen load of individuals in the survey
W10P <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/II_WET_2010.csv")[,-1]
W11P <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/II_WET_2011.csv")[,-1]
W12P <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/II_WET_2012.csv")[,-1]
D13P <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/II_DRY_2013.csv")[,-1]
W13P <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/II_WET_2013.csv")[,-1]
D14P <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/II_DRY_2014.csv")[,-1]


# ------ Bundle data
# Initial host values
R <- nrow(W12P)
T <- ncol(W12P)
K <- 6

#-------------- Not infected Host 
W10P <- data.frame(W10P, survey_7 = rep(NA, times = nrow(W10P)),
                         survey_8 = rep(NA, times = nrow(W10P)))
W11P <- data.frame(W11P, survey_7 = rep(NA, times = nrow(W11P)),
                         survey_8 = rep(NA, times = nrow(W11P)))

dim(W10P)
dim(W11P)
dim(W12P)
dim(D13P)
dim(W13P)
dim(D14P)

#-------------

II <- array(NA, dim = c(R, T, K))

II[,,1] <- as.matrix(W10P)
II[,,2] <- as.matrix(W11P)
II[,,3] <- as.matrix(W12P)
II[,,4] <- as.matrix(D13P)
II[,,5] <- as.matrix(W13P)
II[,,6] <- as.matrix(D14P)

II_log <- log(II+0.01)
  # Same transformation as in the Double swab data

II_logten <- log10(II+1)

II_logten[is.na(II_logten) == TRUE] <- mean(II_logten, na.rm = T)

#------ calculate theta

# From Miller et al. 2012 & DiRenzo et al. in prep = 
# probability of detecting the pathogen on an infected individual = 
# probability of correctly identifying an infected individual

# 1 - pd = misclassfied

alpha_I <- 0.77

beta_I <- 0.32


theta <- array(NA, dim = c(dim(II_log)))

for(i in 1:dim(II_log)[1]){
  for(j in 1:dim(II_log)[2]){
    for(t in 1:dim(II_log)[3]){
      theta[i, j, t] <- (1- plogis(alpha_I + beta_I * II_log[i,j,t]))
    }
  }
}

Theta_mean <- mean(theta[,,], na.rm = T)

theta[is.na(theta) == TRUE] <- Theta_mean
II_log[is.na(II_log) == TRUE] <- mean(II_log, na.rm = TRUE)


