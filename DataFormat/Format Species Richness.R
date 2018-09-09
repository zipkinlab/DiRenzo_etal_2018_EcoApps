#------ Read in the data

#--- These files have the average pathogen load of individuals in the survey
W10SR <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/SR_WET_2010.csv")[,-1]
W11SR <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/SR_WET_2011.csv")[,-1]
W12SR <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/SR_WET_2012.csv")[,-1]
D13SR <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/SR_DRY_2013.csv")[,-1]
W13SR <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/SR_WET_2013.csv")[,-1]
D14SR <- read.csv(file = "/Users/Cici/GitHub/2010to2014ElCope/Data files Nov 2016/SR_DRY_2014.csv")[,-1]


# ------ Bundle data
# Initial host values
R <- nrow(W12SR)
T <- ncol(W12SR)
K <- 6

#-------------- Not infected Host 
W10SR <- data.frame(W10SR, survey_7 = rep(NA, times = nrow(W10SR)),
                         survey_8 = rep(NA, times = nrow(W10SR)))

dim(W10SR)
dim(W11SR)
dim(W12SR)
dim(D13SR)
dim(W13SR)
dim(D14SR)

#-------------

SR <- array(NA, dim = c(R, T, K))

SR[,,1] <- as.matrix(W10SR)
SR[,,2] <- as.matrix(W11SR)
SR[,,3] <- as.matrix(W12SR)
SR[,,4] <- as.matrix(D13SR)
SR[,,5] <- as.matrix(W13SR)
SR[,,6] <- as.matrix(D14SR)

SpecRich <- apply(SR, c(1, 3), max, na.rm = TRUE)
SpecRich[SpecRich == -Inf] <- NA

