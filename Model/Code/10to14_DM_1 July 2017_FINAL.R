# Citation:
# DiRenzo, G. V., E. Zipkin, E. C. Grant, J. A. Royle, A. V. Longo, K. Z. Zamudio, and K. R. Lips. In review. Eco-evolutionary rescue promotes host-pathogen coexistence. Ecology.

# This is the final model presented in the main body of the text.

# Before running this code, please run the following R files:
  # 10to14_Format_theta.R
  # 10to14_Model_format.R
  # 10to14_Months_between_seasons.R
  # 10to14_obs_model data.R

sink("model2.txt")
cat("
model{
    
#------------ Priors

#------- Recruitment
gammaN ~ dnorm(0, 0.001)
gammaI ~ dnorm(0, 0.001)

beta.g ~ dnorm(0, 0.001)

#----- Initial population
alpha.lamN ~ dnorm(0, 0.001)
alpha.lamI ~ dnorm(0, 0.001)

#------ Detection
for(g in 1:2){
  alpha.pI[g] ~ dnorm(0, 0.368)
  alpha.pN[g] ~ dnorm(0, 0.368)
}

#------ Survival prob
alpha_N ~ dnorm(0, 0.368)
alpha_I ~ dnorm(0, 0.368)


#-------- Transition
alpha_IN ~ dnorm(0, 0.368)
alpha_NI ~ dnorm(0, 0.368)

q ~ dbern(0.5)
beta_NI ~ dnorm(0, 0.368)

#------ Detection
beta.pN ~ dnorm(0, 0.368)
beta.II ~ dnorm(0, 0.368)

#------ Imperfect pathogen detection
a_I ~ dunif(0.25, 1.32)
b_I ~ dunif(0.14, 0.51)

#------------------------------- Logit -> Probability scale
for(k in 1:(K - 1)){

    for(i in 1:R){

     cov[i, k] <- ((NI[i, k]/(Ntot[i, k]^q +0.001)))
     covScale[i, k] <- (cov[i, k] - 2.85)

#---------------- Logit Scale
    lpsi_NI[i, k] <- alpha_NI + beta_NI * covScale[i, k]
    lpsi_IN[i, k] <- alpha_IN

    lphi_N[i, k] <- alpha_N
    lphi_I[i, k] <- alpha_I

#---------------- Probability scale
    psi_NI[i, k] <- exp(lpsi_NI[i, k]) / (1+ exp(lpsi_NI[i, k]))
    psi_IN[i, k] <- exp(lpsi_IN[i, k]) / (1+ exp(lpsi_IN[i, k]))

    phiN[i, k] <- exp(lphi_N[i, k]) / (1+ exp(lphi_N[i, k]))
    phiI[i, k] <- exp(lphi_I[i, k]) / (1+ exp(lphi_I[i, k]))

  }

}

#-----------------First year
for(i in 1:R){								

#------ Not infected
    NN[i, 1] ~ dpois(lambdaN[i])	
      log(lambdaN[i]) <- alpha.lamN

#----- Infected
    NI[i, 1] ~ dpois(lambdaI[i])	   		
      log(lambdaI[i]) <- alpha.lamI

#-------- Population Size
    Ntot[i, 1] <- NN[i, 1] + NI[i, 1]
    Prev1[i, 1] <- NI[i, 1] / (Ntot[i, 1]+0.001)

}

#------------------------------------ Second year

for(k in 2:K){
  for(i in 1:R){

#------- NOT Infected

    SN[i,k-1] ~ dbin(phiN[i,k-1]^days[i,k-1], NN[i, k-1])

    TN[i,k-1] ~ dbin(psi_NI[i,k-1]^days[i,k-1], SN[i, k-1])

    GN[i,k-1] ~ dpois(GaN[i, k-1])
    	log(GaN[i, k-1]) <- gammaN + beta.g * daysS[i,k-1]

#------- Infected

    SI[i,k-1] ~ dbin(phiI[i,k-1]^days[i,k-1], NI[i, k-1])

    TI[i,k-1] ~ dbin(psi_IN[i,k-1]^days[i,k-1], SI[i, k-1])
 
    GI[i,k-1] ~ dpois(GaI[i, k-1])
    	log(GaI[i, k-1]) <- gammaI + beta.g * daysS[i,k-1]

#-------- Population Size

    NN[i, k] <- SN[i,k-1] + GN[i,k-1] + TI[i,k-1] - TN[i,k-1]

    NI[i, k] <- SI[i,k-1] + GI[i,k-1] - TI[i,k-1] + TN[i,k-1]

    Ntot[i, k] <- NN[i, k] + NI[i, k]
    Prev1[i, k] <- NI[i, k] / (Ntot[i, k]+0.001)

  }

}

#------------- Obervation model

for(i in 1:R){

  for(k in 1:K){
  
    for(j in 1:T){

#--------- Probability of detecting a host

     yN[i, j, k] ~ dbin(pN[i, j, k], NN[i, k])
        logit(pN[i, j, k]) <- alpha.pN[Se[k]] + beta.pN * obs[i,j,k]
     yN.new[i, j, k] ~ dbin(pN[i, j, k], NN[i, k])

     yI[i, j, k] ~ dbin(pI[i, j, k], NI[i, k]) 
        logit(pI[i, j, k]) <- alpha.pI[Se[k]] + beta.pN * obs[i,j,k] + beta.II * II2[i, j, k]

     yI.new[i, j, k] ~ dbin(pI[i, j, k], NI[i, k]) 

#-------- Probability of detecting the pathogen on a host

    cI[i, j, k] ~ dsum(yI[i, j, k], (-1 * mis[i, j, k])) 

    cU[i, j, k] ~ dsum(yN[i, j, k], mis[i, j, k]) 

     mis[i, j, k] ~ dbin(theta2[i, j, k], x[i, j, k]) 	
        theta2[i, j, k] <- 1 - theta[i, j, k]
        logit(theta[i, j, k]) <- a_I + b_I * II2[i, j, k]

    }
  }
}


##------------ Bayesian p-value

for(k in 1:K){
  
  for(i in 1:R){

  for(j in 1:T){
 
    evalN[i,j,k] <- pN[i, j, k] * NN[i, k] 						
    evalI[i,j,k] <- pI[i, j, k] * NI[i, k] 								

    EN[i,j,k] <- pow((yN[i, j, k] - evalN[i,j,k]),2) / (evalN[i,j,k] + 0.5) 	
    EI[i,j,k] <- pow((yI[i, j, k] - evalI[i,j,k]),2) / (evalI[i,j,k] + 0.5) 	

    E.newN[i,j,k] <- pow((yN.new[i, j,k] - evalN[i,j,k]),2) / (evalN[i,j,k] + 0.5)  
    E.newI[i,j,k] <- pow((yI.new[i, j,k] - evalI[i,j,k]),2) / (evalI[i,j,k] + 0.5)  

  } #js									

  } #is

} # ks


zzzfitN 		<- sum(EN[,,]) 	
zzzfitN.new  <- sum(E.newN[,,])

zzzfitI		<- sum(EI[,,]) 	
zzzfitI.new  <- sum(E.newI[,,])

for(k in 1:K){
  zzzfitII[k]		<- sum(EI[,,k]) 	
  zzzfitII.new[k]  <- sum(E.newI[,,k])
}


#---------- Derived quantities
#---- Entire population size

for(k in 1:K){
  NT[k] <- sum(Ntot[,k])
  NNT[k]<- sum(NN[,k])
  NIT[k]<- sum(NI[,k])
  Prev2[k] <- mean(NI[, k] / (Ntot[, k]+0.001))
}

}
", fill = TRUE)
sink()

#----- Indicies

K = dim(host)[3]
R = nrow(host)
T = ncol(host)

#----- Host

H2 <- host
H2[is.na(H2[,,])] <- round(mean(c(H2[,,1:6]), na.rm = T))

#---- Standardize number of days

daysS <- (days3-mean(days3))/sd(days3)

#---- Standardize number of observers

obsS <- (obs - mean(obs))/ sd(obs)

#------ Bundle data

win.data <- list(cI = Ihost,
				         cU = host,
				         x = H2,
				         
				         R = nrow(host), 
                 T = ncol(host),
                 K = dim(host)[3],
				         
				         days = days3,   # Used for Survival & Transistions
				         daysS = daysS,  # Used for Recruitment
				         
				         obs = obs,  # Used for host detection model
				         
				         Se = c(1, 1, 1, 2, 1, 2), # Used in detection
				         
				         II2 = II_log   # Used for theta and in detection
				         )
#--------------------- Initial values

Nt <- colSums(apply(host, c(1, 3), sum, na.rm = TRUE))+
colSums(apply(Ihost, c(1, 3), sum, na.rm = TRUE))

nI <-colSums(apply(Ihost, c(1, 3), sum, na.rm = TRUE))
nn <-colSums(apply(host, c(1, 3), sum, na.rm = TRUE))

#---
theta2 <- theta

yI <- round(win.data$cI + (win.data$x * theta2))
yN <- round(win.data$cU - (win.data$x * theta2))


NIst <- apply(yI, c(1, 3), max, na.rm = TRUE)+1
NIst1 <- apply(yI, c(1, 3), max, na.rm = TRUE)

NNst <- apply(yN, c(1, 3), max, na.rm = TRUE)+1
NNst2 <- apply(yN, c(1, 3), sum, na.rm = TRUE)

SI <- GI <- array(NA, c(R, K-1))
SN <- GN <- array(NA, c(R, K-1))
TNn <- TIi <- array(NA, c(R, K-1))

SI[] <- 1
SN[] <- 1

TIi[] <- 0
TNn[] <- 0

GI <- NIst[,-1] - SI 
GN <- NNst[,-1] - SN 

NNst2[ , -1] <- NA
NNst[ , -1] <- NA
NIst1[ , -1] <- NA
NIst[ , -1] <- NA

NIst1[NIst1 == 0] <- 1

NNst[NNst =="-Inf"] <- 1
NIst[NIst == "-Inf"] <- 1

GI[GI =="-Inf"] <- 1
GN[GN =="-Inf"] <- 1

inits <- function() {list(NN = NNst, 
                          NI = NIst, 
                          
                       GI = GI,  
                       GN = GN, 
                       
                       TI = TIi, 
                       TN = TNn, 
                       
                       mis = round(win.data$x * theta2), 
                       
                       SN = SN,
                      
                       yI = yI,
                       yN = yN,
                                               
                       alpha.lamN = runif(1, -1, 0),
                       alpha.lamI = runif(1, -1, 0),
                       
                       alpha_N = runif(1, 2, 3),
                       alpha_I = runif(1, 2, 3),

                       gammaN = runif(1, -1, 0),
                       gammaI = runif(1, -1, 0), 
                       
                       alpha_IN = runif(1, -2, 0),
                       alpha_NI = runif(1, -2, 0),

                       alpha.pI = runif(2, -3, -1),
                       alpha.pN = runif(2, -3, -1),
                       
                       q = runif(1, 0, 1),
                       beta_NI = runif(1, -1, 1),
                       
                       beta.g = runif(1, 0, 1),
                       beta.pN = runif(1, 0, 1),
                       beta.II = runif(1, 0, 1)
                       
)}

#--------- Monitor Parameters

params <- c("alpha.lamN", "alpha.lamI",
            "alpha_N", "alpha_I",
            "gammaN", "gammaI", "beta.g",
            
            "alpha_IN",
            "alpha_NI", 
            "q", "beta_NI",

            "alpha.pN","alpha.pI",
            "beta.pN", "beta.II",
            
           "zzzfitN", "zzzfitN.new",
           "zzzfitI", "zzzfitI.new",
           "zzzfitII", "zzzfitII.new",
           "NT",
           "NNT",
           "NIT",
           "a_I", "b_I",
           "Prev2"
	          )

#--------- MCMC settings

ni <- 500000
nb <- 50000
nt <- 50
nc <- 3

#--------- Model Run

library("jagsUI")

out <- jags(win.data, inits, params, "model2.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb, parallel = TRUE)
