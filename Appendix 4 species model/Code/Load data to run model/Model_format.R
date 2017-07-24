#------ Read in the data

# Species 1 = CP
W10I_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/IN_WET_2010.csv")[,-1]
W11I_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/IN_WET_2011.csv")[,-1]
W12I_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/IN_WET_2012.csv")[,-1]
D13I_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/IN_DRY_2013.csv")[,-1]
W13I_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/IN_WET_2013.csv")[,-1]
D14I_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/IN_DRY_2014.csv")[,-1]

W10_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NOT_WET_2010.csv")[,-1]
W11_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NOT_WET_2011.csv")[,-1]
W12_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NOT_WET_2012.csv")[,-1]
D13_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NOT_DRY_2013.csv")[,-1]
W13_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NOT_WET_2013.csv")[,-1]
D14_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NOT_DRY_2014.csv")[,-1]


W10U_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NAN_WET_2010.csv")[,-1]
W11U_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NAN_WET_2011.csv")[,-1]
W12U_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NAN_WET_2012.csv")[,-1]
D13U_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NAN_DRY_2013.csv")[,-1]
W13U_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NAN_WET_2013.csv")[,-1]
D14U_1 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/CP/NAN_DRY_2014.csv")[,-1]

# Species 2 = Pcerasinus
W10I_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/IN_WET_2010.csv")[,-1]
W11I_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/IN_WET_2011.csv")[,-1]
W12I_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/IN_WET_2012.csv")[,-1]
D13I_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/IN_DRY_2013.csv")[,-1]
W13I_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/IN_WET_2013.csv")[,-1]
D14I_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/IN_DRY_2014.csv")[,-1]

W10_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NOT_WET_2010.csv")[,-1]
W11_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NOT_WET_2011.csv")[,-1]
W12_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NOT_WET_2012.csv")[,-1]
D13_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NOT_DRY_2013.csv")[,-1]
W13_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NOT_WET_2013.csv")[,-1]
D14_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NOT_DRY_2014.csv")[,-1]


W10U_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NAN_WET_2010.csv")[,-1]
W11U_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NAN_WET_2011.csv")[,-1]
W12U_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NAN_WET_2012.csv")[,-1]
D13U_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NAN_DRY_2013.csv")[,-1]
W13U_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NAN_WET_2013.csv")[,-1]
D14U_2 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcerasinus/NAN_DRY_2014.csv")[,-1]

# Species 3 = Pcruentus
W10I_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/IN_WET_2010.csv")[,-1]
W11I_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/IN_WET_2011.csv")[,-1]
W12I_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/IN_WET_2012.csv")[,-1]
D13I_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/IN_DRY_2013.csv")[,-1]
W13I_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/IN_WET_2013.csv")[,-1]
D14I_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/IN_DRY_2014.csv")[,-1]

W10_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NOT_WET_2010.csv")[,-1]
W11_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NOT_WET_2011.csv")[,-1]
W12_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NOT_WET_2012.csv")[,-1]
D13_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NOT_DRY_2013.csv")[,-1]
W13_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NOT_WET_2013.csv")[,-1]
D14_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NOT_DRY_2014.csv")[,-1]


W10U_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NAN_WET_2010.csv")[,-1]
W11U_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NAN_WET_2011.csv")[,-1]
W12U_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NAN_WET_2012.csv")[,-1]
D13U_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NAN_DRY_2013.csv")[,-1]
W13U_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NAN_WET_2013.csv")[,-1]
D14U_3 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Pcruentus/NAN_DRY_2014.csv")[,-1]

# Species 4 = Salbo
W10I_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/IN_WET_2010.csv")[,-1]
W11I_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/IN_WET_2011.csv")[,-1]
W12I_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/IN_WET_2012.csv")[,-1]
D13I_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/IN_DRY_2013.csv")[,-1]
W13I_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/IN_WET_2013.csv")[,-1]
D14I_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/IN_DRY_2014.csv")[,-1]

W10_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NOT_WET_2010.csv")[,-1]
W11_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NOT_WET_2011.csv")[,-1]
W12_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NOT_WET_2012.csv")[,-1]
D13_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NOT_DRY_2013.csv")[,-1]
W13_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NOT_WET_2013.csv")[,-1]
D14_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NOT_DRY_2014.csv")[,-1]


W10U_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NAN_WET_2010.csv")[,-1]
W11U_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NAN_WET_2011.csv")[,-1]
W12U_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NAN_WET_2012.csv")[,-1]
D13U_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NAN_DRY_2013.csv")[,-1]
W13U_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NAN_WET_2013.csv")[,-1]
D14U_4 <- read.csv(file = "/Users/Cici/GitHub/4speciesmodel/Data/Salbo/NAN_DRY_2014.csv")[,-1]

# ------ Bundle data
# Initial host values
R <- nrow(W12_1)   # Number of sites
T <- ncol(W12_1)   # Number of replicate surveys
K <- 6    # 5 seasons
S <- 4    # 4 species

#-------------- Not infected Host 

W10_1 <- data.frame(W10_1, survey_6 = rep(NA, times = nrow(W10_1)), 
                           survey_7 = rep(NA, times = nrow(W10_1)),
                           survey_8 = rep(NA, times = nrow(W10_1)))
W11_1 <- data.frame(W11_1, survey_6 = rep(NA, times = nrow(W11_1)),
                           survey_7 = rep(NA, times = nrow(W11_1)),
                           survey_8 = rep(NA, times = nrow(W11_1)))

W10_2 <- data.frame(W10_2, survey_6 = rep(NA, times = nrow(W10_2)), 
                           survey_7 = rep(NA, times = nrow(W10_2)),
                           survey_8 = rep(NA, times = nrow(W10_2)))
W11_2 <- data.frame(W11_2, survey_6 = rep(NA, times = nrow(W11_2)),
                            survey_7 = rep(NA, times = nrow(W11_2)),
                           survey_8 = rep(NA, times = nrow(W11_2)))

W10_3 <- data.frame(W10_3, survey_6 = rep(NA, times = nrow(W10_3)), 
                            survey_7 = rep(NA, times = nrow(W10_3)),
                            survey_8 = rep(NA, times = nrow(W10_3)))
W11_3 <- data.frame(W11_3, survey_6 = rep(NA, times = nrow(W11_3)),
                            survey_7 = rep(NA, times = nrow(W11_3)),
                            survey_8 = rep(NA, times = nrow(W11_3)))

W10_4 <- data.frame(W10_4, survey_6 = rep(NA, times = nrow(W10_4)), 
                            survey_7 = rep(NA, times = nrow(W10_4)),
                            survey_8 = rep(NA, times = nrow(W10_4)))
W11_4 <- data.frame(W11_4, survey_6 = rep(NA, times = nrow(W11_4)),
                            survey_7 = rep(NA, times = nrow(W11_4)),
                            survey_8 = rep(NA, times = nrow(W11_4)))

dim(W10_4)
dim(W11_4)
dim(W12_4)
dim(D13_4)
dim(W13_4)
dim(D14_4)

#-------------

host <- array(NA, dim = c(R, T, K, S))

host[,,1,1] <- as.matrix(W10_1)
host[,,2,1] <- as.matrix(W11_1)
host[,,3,1] <- as.matrix(W12_1)
host[,,4,1] <- as.matrix(D13_1)
host[,,5,1] <- as.matrix(W13_1)
host[,,6,1] <- as.matrix(D14_1)

host[,,1,2] <- as.matrix(W10_2)
host[,,2,2] <- as.matrix(W11_2)
host[,,3,2] <- as.matrix(W12_2)
host[,,4,2] <- as.matrix(D13_2)
host[,,5,2] <- as.matrix(W13_2)
host[,,6,2] <- as.matrix(D14_2)

host[,,1,3] <- as.matrix(W10_3)
host[,,2,3] <- as.matrix(W11_3)
host[,,3,3] <- as.matrix(W12_3)
host[,,4,3] <- as.matrix(D13_3)
host[,,5,3] <- as.matrix(W13_3)
host[,,6,3] <- as.matrix(D14_3)

host[,,1,4] <- as.matrix(W10_4)
host[,,2,4] <- as.matrix(W11_4)
host[,,3,4] <- as.matrix(W12_4)
host[,,4,4] <- as.matrix(D13_4)
host[,,5,4] <- as.matrix(W13_4)
host[,,6,4] <- as.matrix(D14_4)

#------------ Infected host 

W10I_1 <- data.frame(W10I_1, 
                   survey_6 = rep(NA, times = nrow(W10I_1)),
                   survey_7 = rep(NA, times = nrow(W10I_1)),
                   survey_8 = rep(NA, times = nrow(W10I_1)))
W11I_1 <- data.frame(W11I_1, 
                   survey_6 = rep(NA, times = nrow(W11I_1)),
                   survey_7 = rep(NA, times = nrow(W11I_1)),
                   survey_8 = rep(NA, times = nrow(W11I_1)))

W10I_2 <- data.frame(W10I_2, 
                     survey_6 = rep(NA, times = nrow(W10I_2)),
                     survey_7 = rep(NA, times = nrow(W10I_2)),
                     survey_8 = rep(NA, times = nrow(W10I_2)))
W11I_2 <- data.frame(W11I_2, 
                     survey_6 = rep(NA, times = nrow(W11I_2)),
                     survey_7 = rep(NA, times = nrow(W11I_2)),
                     survey_8 = rep(NA, times = nrow(W11I_2)))

W10I_3 <- data.frame(W10I_3, 
                     survey_6 = rep(NA, times = nrow(W10I_3)),
                     survey_7 = rep(NA, times = nrow(W10I_3)),
                     survey_8 = rep(NA, times = nrow(W10I_3)))
W11I_3 <- data.frame(W11I_3, 
                     survey_6 = rep(NA, times = nrow(W11I_3)),
                     survey_7 = rep(NA, times = nrow(W11I_3)),
                     survey_8 = rep(NA, times = nrow(W11I_3)))

W10I_4 <- data.frame(W10I_4, 
                     survey_6 = rep(NA, times = nrow(W10I_4)),
                     survey_7 = rep(NA, times = nrow(W10I_4)),
                     survey_8 = rep(NA, times = nrow(W10I_4)))
W11I_4 <- data.frame(W11I_4, 
                     survey_6 = rep(NA, times = nrow(W11I_4)),
                     survey_7 = rep(NA, times = nrow(W11I_4)),
                     survey_8 = rep(NA, times = nrow(W11I_4)))

dim(W10I_4)
dim(W11I_4)
dim(W12I_4)
dim(D13I_4)
dim(W13I_4)
dim(D14I_4)

#-------------

Ihost <- array(NA, dim = c(R, T, K, S))

Ihost[,,1,1] <- as.matrix(W10I_1)
Ihost[,,2,1] <- as.matrix(W11I_1)
Ihost[,,3,1] <- as.matrix(W12I_1)
Ihost[,,4,1] <- as.matrix(D13I_1)
Ihost[,,5,1] <- as.matrix(W13I_1)
Ihost[,,6,1] <- as.matrix(D14I_1)

Ihost[,,1,2] <- as.matrix(W10I_2)
Ihost[,,2,2] <- as.matrix(W11I_2)
Ihost[,,3,2] <- as.matrix(W12I_2)
Ihost[,,4,2] <- as.matrix(D13I_2)
Ihost[,,5,2] <- as.matrix(W13I_2)
Ihost[,,6,2] <- as.matrix(D14I_2)

Ihost[,,1,3] <- as.matrix(W10I_3)
Ihost[,,2,3] <- as.matrix(W11I_3)
Ihost[,,3,3] <- as.matrix(W12I_3)
Ihost[,,4,3] <- as.matrix(D13I_3)
Ihost[,,5,3] <- as.matrix(W13I_3)
Ihost[,,6,3] <- as.matrix(D14I_3)

Ihost[,,1,4] <- as.matrix(W10I_4)
Ihost[,,2,4] <- as.matrix(W11I_4)
Ihost[,,3,4] <- as.matrix(W12I_4)
Ihost[,,4,4] <- as.matrix(D13I_4)
Ihost[,,5,4] <- as.matrix(W13I_4)
Ihost[,,6,4] <- as.matrix(D14I_4)

#------------ Unknown host


W10U_1 <- data.frame(W10U_1, 
                   survey_6 = rep(NA, times = nrow(W10U_1)),
                   survey_7 = rep(NA, times = nrow(W10U_1)),
                   survey_8 = rep(NA, times = nrow(W10U_1)))
W11U_1 <- data.frame(W11U_1, 
                   survey_6 = rep(NA, times = nrow(W11U_1)),
                   survey_7 = rep(NA, times = nrow(W11U_1)),
                   survey_8 = rep(NA, times = nrow(W11U_1)))

W10U_2 <- data.frame(W10U_2, 
                     survey_6 = rep(NA, times = nrow(W10U_2)),
                     survey_7 = rep(NA, times = nrow(W10U_2)),
                     survey_8 = rep(NA, times = nrow(W10U_2)))
W11U_2 <- data.frame(W11U_2, 
                     survey_6 = rep(NA, times = nrow(W11U_2)),
                     survey_7 = rep(NA, times = nrow(W11U_2)),
                     survey_8 = rep(NA, times = nrow(W11U_2)))

W10U_3 <- data.frame(W10U_3, 
                     survey_6 = rep(NA, times = nrow(W10U_3)),
                     survey_7 = rep(NA, times = nrow(W10U_3)),
                     survey_8 = rep(NA, times = nrow(W10U_3)))
W11U_3 <- data.frame(W11U_3, 
                     survey_6 = rep(NA, times = nrow(W11U_3)),
                     survey_7 = rep(NA, times = nrow(W11U_3)),
                     survey_8 = rep(NA, times = nrow(W11U_3)))

W10U_4 <- data.frame(W10U_4, 
                     survey_6 = rep(NA, times = nrow(W10U_4)),
                     survey_7 = rep(NA, times = nrow(W10U_4)),
                     survey_8 = rep(NA, times = nrow(W10U_4)))
W11U_4 <- data.frame(W11U_4, 
                     survey_6 = rep(NA, times = nrow(W11U_4)),
                     survey_7 = rep(NA, times = nrow(W11U_4)),
                     survey_8 = rep(NA, times = nrow(W11U_4)))
dim(W10U_4)
dim(W11U_4)
dim(W12U_1)
dim(D13U_1)
dim(W13U_1)
dim(D14U_1)

#-------------

Uhost <- array(NA, dim = c(R, T, K, S))

Uhost[,,1,1] <- as.matrix(W10U_1)
Uhost[,,2,1] <- as.matrix(W11U_1)
Uhost[,,3,1] <- as.matrix(W12U_1)
Uhost[,,4,1] <- as.matrix(D13U_1)
Uhost[,,5,1] <- as.matrix(W13U_1)
Uhost[,,6,1] <- as.matrix(D14U_1)

Uhost[,,1,2] <- as.matrix(W10U_2)
Uhost[,,2,2] <- as.matrix(W11U_2)
Uhost[,,3,2] <- as.matrix(W12U_2)
Uhost[,,4,2] <- as.matrix(D13U_2)
Uhost[,,5,2] <- as.matrix(W13U_2)
Uhost[,,6,2] <- as.matrix(D14U_2)

Uhost[,,1,3] <- as.matrix(W10U_3)
Uhost[,,2,3] <- as.matrix(W11U_3)
Uhost[,,3,3] <- as.matrix(W12U_3)
Uhost[,,4,3] <- as.matrix(D13U_3)
Uhost[,,5,3] <- as.matrix(W13U_3)
Uhost[,,6,3] <- as.matrix(D14U_3)

Uhost[,,1,4] <- as.matrix(W10U_4)
Uhost[,,2,4] <- as.matrix(W11U_4)
Uhost[,,3,4] <- as.matrix(W12U_4)
Uhost[,,4,4] <- as.matrix(D13U_4)
Uhost[,,5,4] <- as.matrix(W13U_4)
Uhost[,,6,4] <- as.matrix(D14U_4)
#---------- Total number of captures and surveys

#----- Table 3
#---- Average prevalence across seasons

All <- Ihost + host

sum(All[,,1,], na.rm = T)
sum(All[,,2,], na.rm = T)
sum(All[,,3,], na.rm = T)
sum(All[,,4,], na.rm = T)
sum(All[,,5,], na.rm = T)
sum(All[,,6,], na.rm = T)


