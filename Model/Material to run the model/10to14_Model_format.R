#------ Read in the data

W10I <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/IN_WET_2010.csv")[,-1]
W11I <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/IN_WET_2011.csv")[,-1]
W12I <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/IN_WET_2012.csv")[,-1]
D13I <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/IN_DRY_2013.csv")[,-1]
W13I <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/IN_WET_2013.csv")[,-1]
D14I <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/IN_DRY_2014.csv")[,-1]


W10 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NOT_WET_2010.csv")[,-1]
W11 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NOT_WET_2011.csv")[,-1]
W12 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NOT_WET_2012.csv")[,-1]
D13 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NOT_DRY_2013.csv")[,-1]
W13 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NOT_WET_2013.csv")[,-1]
D14 <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NOT_DRY_2014.csv")[,-1]


W10U <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NAN_WET_2010.csv")[,-1]
W11U <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NAN_WET_2011.csv")[,-1]
W12U <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NAN_WET_2012.csv")[,-1]
D13U <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NAN_DRY_2013.csv")[,-1]
W13U <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NAN_WET_2013.csv")[,-1]
D14U <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/NAN_DRY_2014.csv")[,-1]

# ------ Bundle data
# Initial host values
R <- nrow(W12)
T <- ncol(W12)
K <- 6

#-------------- Not infected Host 

W10 <- data.frame(W10, survey_7 = rep(NA, times = nrow(W10)),
                   survey_8 = rep(NA, times = nrow(W10)))
W11 <- data.frame(W11, survey_7 = rep(NA, times = nrow(W11)),
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

#------------ Infected host 

W10I <- data.frame(W10I, survey_7 = rep(NA, times = nrow(W10I)),
                   survey_8 = rep(NA, times = nrow(W10I)))
W11I <- data.frame(W11I, survey_7 = rep(NA, times = nrow(W11I)),
                   survey_8 = rep(NA, times = nrow(W11I)))

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


W10U <- data.frame(W10U, survey_7 = rep(NA, times = nrow(W10U)),
                   survey_8 = rep(NA, times = nrow(W10U)))
W11U <- data.frame(W11U, survey_7 = rep(NA, times = nrow(W11U)),
                   survey_8 = rep(NA, times = nrow(W11U)))

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

All <- Ihost + host

sum(All[,,1], na.rm = T)
sum(All[,,2], na.rm = T)
sum(All[,,3], na.rm = T)
sum(All[,,4], na.rm = T)
sum(All[,,5], na.rm = T)
sum(All[,,6], na.rm = T)

max(All, na.rm = T)

