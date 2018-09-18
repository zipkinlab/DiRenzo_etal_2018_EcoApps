#------ Read in the data

W10I <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/INF_WET_2010.csv")[,-1]
W11I <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/INF_WET_2011.csv")[,-1]
W12I <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/INF_14-01-2016_WET_2012.csv")[,-1]
D13I <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/INF_14-01-2016_DRY_2013.csv")[,-1]
W13I <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/INF_14-01-2016_WET_2013.csv")[,-1]
D14I <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/INF_14-01-2016_DRY_2014.csv")[,-1]


W10 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/NOT_WET_2010.csv")[,-1]
W11 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/NOT_WET_2011.csv")[,-1]
W12 <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/NOT_INF_14-01-2016_WET_2012.csv")[,-1]
D13 <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/NOT_INF_14-01-2016_DRY_2013.csv")[,-1]
W13 <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/NOT_INF_14-01-2016_WET_2013.csv")[,-1]
D14 <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/NOT_INF_14-01-2016_DRY_2014.csv")[,-1]


W10U <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/NA_WET_2010.csv")[,-1]
W11U <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/NA_WET_2011.csv")[,-1]
W12U <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/UNK_31-01-2016_WET_2012.csv")[,-1]
D13U <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/UNK_31-01-2016_DRY_2013.csv")[,-1]
W13U <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/UNK_31-01-2016_WET_2013.csv")[,-1]
D14U <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Models/Data_20m/UNK_31-01-2016_DRY_2014.csv")[,-1]

# ------ Bundle data
# Initial host values
R <- nrow(W12)
T <- ncol(W12)
K <- 6

#-------------- Not infected Host 

D13 <- data.frame(D13, survey_8 = rep(NA, times = nrow(D13)))
W13 <- data.frame(W13, survey_8 = rep(NA, times = nrow(W13)))
D14 <- data.frame(D14)


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

#------------ Infected host 

W12I <- W12I
D13I <- data.frame(D13I, survey_8 = rep(NA, times = nrow(D13I)))
W13I <- data.frame(W13I, survey_8 = rep(NA, times = nrow(W13I)))
D14I <- data.frame(D14I)

dim(W10I)
dim(W11I)
dim(W12I)
dim(D13I)
dim(W13I)
dim(D14I)

#-------------

Ihost <- array(NA, dim = c(R, T, K))

Ihost[,,1] <- as.matrix(W10I)
Ihost[,,2] <- as.matrix(W11I)
Ihost[,,3] <- as.matrix(W12I)
Ihost[,,4] <- as.matrix(D13I)
Ihost[,,5] <- as.matrix(W13I)
Ihost[,,6] <- as.matrix(D14I)


#------------ Unknown host

W12U <- W12U
D13U <- data.frame(D13U)
W13U <- data.frame(W13U)
D14U <- data.frame(D14U)

dim(W10U)
dim(W11U)
dim(W12U)
dim(D13U)
dim(W13U)
dim(D14U)

#-------------

Uhost <- array(NA, dim = c(R, T, K))

Uhost[,,1] <- as.matrix(W10U)
Uhost[,,2] <- as.matrix(W11U)
Uhost[,,3] <- as.matrix(W12U)
Uhost[,,4] <- as.matrix(D13U)
Uhost[,,5] <- as.matrix(W13U)
Uhost[,,6] <- as.matrix(D14U)


#---------- Total number of captures and surveys

sum(colSums(apply(host, c(1, 3), sum, na.rm = T)))+
  sum(colSums(apply(Ihost, c(1, 3), sum, na.rm = T)))+
  sum(colSums(apply(Uhost, c(1, 3), sum, na.rm = T)))

#----- Table 3
#---- Average prevalence across seasons

All <- Ihost + host + Uhost

max(All, na.rm = T)

