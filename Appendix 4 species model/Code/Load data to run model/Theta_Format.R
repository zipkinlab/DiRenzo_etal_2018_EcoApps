#------ Read in the data

#--- These files have the average pathogen load of individuals in the survey
W10P_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/II_WET_2010.csv")[,-1]
W11P_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/II_WET_2011.csv")[,-1]
W12P_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/II_WET_2012.csv")[,-1]
D13P_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/II_DRY_2013.csv")[,-1]
W13P_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/II_WET_2013.csv")[,-1]
D14P_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/II_DRY_2014.csv")[,-1]

W10P_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/II_WET_2010.csv")[,-1]
W11P_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/II_WET_2011.csv")[,-1]
W12P_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/II_WET_2012.csv")[,-1]
D13P_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/II_DRY_2013.csv")[,-1]
W13P_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/II_WET_2013.csv")[,-1]
D14P_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/II_DRY_2014.csv")[,-1]

W10P_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/II_WET_2010.csv")[,-1]
W11P_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/II_WET_2011.csv")[,-1]
W12P_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/II_WET_2012.csv")[,-1]
D13P_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/II_DRY_2013.csv")[,-1]
W13P_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/II_WET_2013.csv")[,-1]
D14P_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/II_DRY_2014.csv")[,-1]

W10P_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/II_WET_2010.csv")[,-1]
W11P_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/II_WET_2011.csv")[,-1]
W12P_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/II_WET_2012.csv")[,-1]
D13P_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/II_DRY_2013.csv")[,-1]
W13P_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/II_WET_2013.csv")[,-1]
D14P_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/II_DRY_2014.csv")[,-1]

# ------ Bundle data
# Initial host values
R <- nrow(W12P_1)
T <- ncol(W12P_1)
K <- 6
S <- 4

#-------------- Not infected Host 
W10P_1 <- data.frame(W10P_1, 
                          survey_5 = rep(NA, times = nrow(W10P_1)),
                          survey_6 = rep(NA, times = nrow(W10P_1)),
                          survey_7 = rep(NA, times = nrow(W10P_1)),
                          survey_8 = rep(NA, times = nrow(W10P_1)))

W11P_1 <- data.frame(W11P_1, 
                          survey_6 = rep(NA, times = nrow(W11P_1)),
                          survey_7 = rep(NA, times = nrow(W11P_1)),
                          survey_8 = rep(NA, times = nrow(W11P_1)))

W10P_2 <- data.frame(W10P_2, 
                     survey_5 = rep(NA, times = nrow(W10P_2)),
                     survey_6 = rep(NA, times = nrow(W10P_2)),
                     survey_7 = rep(NA, times = nrow(W10P_2)),
                     survey_8 = rep(NA, times = nrow(W10P_2)))

W11P_2 <- data.frame(W11P_2, 
                     survey_6 = rep(NA, times = nrow(W11P_2)),
                     survey_7 = rep(NA, times = nrow(W11P_2)),
                     survey_8 = rep(NA, times = nrow(W11P_2)))

W10P_3 <- data.frame(W10P_3, 
                     survey_5 = rep(NA, times = nrow(W10P_3)),
                     survey_6 = rep(NA, times = nrow(W10P_3)),
                     survey_7 = rep(NA, times = nrow(W10P_3)),
                     survey_8 = rep(NA, times = nrow(W10P_3)))

W11P_3 <- data.frame(W11P_3, 
                     survey_5 = rep(NA, times = nrow(W11P_3)),
                     survey_6 = rep(NA, times = nrow(W11P_3)),
                     survey_7 = rep(NA, times = nrow(W11P_3)),
                     survey_8 = rep(NA, times = nrow(W11P_3)))

W10P_4 <- data.frame(W10P_4, 
                     survey_5 = rep(NA, times = nrow(W10P_4)),
                     survey_6 = rep(NA, times = nrow(W10P_4)),
                     survey_7 = rep(NA, times = nrow(W10P_4)),
                     survey_8 = rep(NA, times = nrow(W10P_4)))

W11P_4 <- data.frame(W11P_4, 
                     survey_5 = rep(NA, times = nrow(W11P_4)),
                     survey_6 = rep(NA, times = nrow(W11P_4)),
                     survey_7 = rep(NA, times = nrow(W11P_4)),
                     survey_8 = rep(NA, times = nrow(W11P_4)))


dim(W10P_4)
dim(W11P_4)
dim(W12P_2)
dim(D13P_2)
dim(W13P_2)
dim(D14P_2)

#-------------

II <- array(NA, dim = c(R, T, K, S))

II[,,1,1] <- as.matrix(W10P_1)
II[,,2,1] <- as.matrix(W11P_1)
II[,,3,1] <- as.matrix(W12P_1)
II[,,4,1] <- as.matrix(D13P_1)
II[,,5,1] <- as.matrix(W13P_1)
II[,,6,1] <- as.matrix(D14P_1)

II[,,1,2] <- as.matrix(W10P_2)
II[,,2,2] <- as.matrix(W11P_2)
II[,,3,2] <- as.matrix(W12P_2)
II[,,4,2] <- as.matrix(D13P_2)
II[,,5,2] <- as.matrix(W13P_2)
II[,,6,2] <- as.matrix(D14P_2)

II[,,1,3] <- as.matrix(W10P_3)
II[,,2,3] <- as.matrix(W11P_3)
II[,,3,3] <- as.matrix(W12P_3)
II[,,4,3] <- as.matrix(D13P_3)
II[,,5,3] <- as.matrix(W13P_3)
II[,,6,3] <- as.matrix(D14P_3)

II[,,1,4] <- as.matrix(W10P_4)
II[,,2,4] <- as.matrix(W11P_4)
II[,,3,4] <- as.matrix(W12P_4)
II[,,4,4] <- as.matrix(D13P_4)
II[,,5,4] <- as.matrix(W13P_4)
II[,,6,4] <- as.matrix(D14P_4)

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
      for(s in 1:dim(II_log)[4]){
        theta[i, j, t, s] <- (1- plogis(alpha_I + beta_I * II_log[i,j,t,s]))
      }
    }
  }
}

Theta_mean <- mean(theta[,,,], na.rm = T)

theta[is.na(theta) == TRUE] <- Theta_mean
II_log[is.na(II_log) == TRUE] <- mean(II_log, na.rm = TRUE)


