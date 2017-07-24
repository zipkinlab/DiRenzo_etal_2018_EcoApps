sink("model2.txt")
cat("
model{
    
#------------ Priors

# Null = dnorm(0, 0.01)
# Covariate = dnorm(0, 0.1)

for(s in 1:nspecies){
  #------- Recruitment
    gammaN[s] ~ dnorm(0, 0.1)
    gammaI[s] ~ dnorm(0, 0.1)
    beta.g[s] ~ dnorm(0, 0.1)

  #----- Initial population
    alpha.lamN[s] ~ dnorm(0, 0.1)
    alpha.lamI[s] ~ dnorm(0, 0.1)

  #------ Survival prob
    alpha_N[s] ~ dnorm(0, 0.368)
    alpha_I[s] ~ dnorm(0, 0.368)

}

#-------- Transition
  alpha_IN ~ dnorm(0, 0.368)
  alpha_NI ~ dnorm(0, 0.368)
  beta.SR ~ dnorm(0, 0.368)

#---- indicator variable
  q ~ dbern(0.5)

#------ Detection
for(g in 1:nseason){  # by seasons
  alpha.pI[g] ~ dnorm(0, 0.368)
  alpha.pN[g] ~ dnorm(0, 0.368)
}
beta.pN ~ dnorm(0, 0.368)
beta.II ~ dnorm(0, 0.368)

#------ Imperfect pathogen detection -> same across species
  a_I ~ dunif(0.25, 1.32)
  b_I ~ dunif(0.14, 0.51)

#------------------------------- Logit -> Probability scale
for(k in 1:(K - 1)){
  for(i in 1:R){

     cov[i, k] <- ((NN_all[i, k]^(1-q)) +  (NI_all[i, k]/(Ntot_all[i, k]^q +0.001)))
     covScale[i, k] <- cov[i, k] - 2.85

    for(s in 1:nspecies){

#---------------- Logit Scale
    lpsi_NI[i, k, s] <- alpha_NI + beta.SR * covScale[i, k]
    lpsi_IN[i, k, s] <- alpha_IN

    lphi_N[i, k, s] <- alpha_N[s]
    lphi_I[i, k, s] <- alpha_I[s]

#---------------- Probability scale
    psi_NI[i, k, s] <- exp(lpsi_NI[i, k, s]) / (1+ exp(lpsi_NI[i, k, s]))
    psi_IN[i, k, s] <- exp(lpsi_IN[i, k, s]) / (1+ exp(lpsi_IN[i, k, s]))

    phiN[i, k, s] <- exp(lphi_N[i, k, s]) / (1+ exp(lphi_N[i, k, s]))
    phiI[i, k, s] <- exp(lphi_I[i, k, s]) / (1+ exp(lphi_I[i, k, s]))
    }
  }
}

#-----------------First year
for(i in 1:R){
  for(s in 1:nspecies){								

#------ Not infected
    NN[i, 1, s] ~ dpois(lambdaN[i,s])	
      log(lambdaN[i,s]) <- alpha.lamN[s]

#----- Infected
    NI[i, 1, s] ~ dpois(lambdaI[i, s])	   		
      log(lambdaI[i, s]) <- alpha.lamI[s]

#-------- Population Size
    Ntot[i, 1, s] <- NN[i, 1, s] + NI[i, 1, s]
    Prev[i, 1, s] <- NI[i, 1, s] / (Ntot[i, 1, s]+0.001)

  }

#-------- Entire population
    NN_all[i, 1] <- sum(NN[i, 1, ])
    NI_all[i, 1] <- sum(NI[i, 1, ])
    Ntot_all[i, 1] <- NN_all[i, 1] + NI_all[i, 1]

}

#------------ Second year
for(i in 1:R){
  for(k in 2:K){
    for(s in 1:nspecies){
#------- NOT Infected

    SN[i,k-1, s] ~ dbin(phiN[i,k-1, s]^days[i,k-1], NN[i, k-1, s])
    TN[i,k-1, s] ~ dbin(psi_NI[i,k-1, s]^days[i,k-1], SN[i, k-1, s])
    GN[i,k-1, s] ~ dpois(GaN[i, k-1, s])
    	log(GaN[i, k-1, s]) <- gammaN[s] + beta.g[s] * daysS[i,k-1]

#------- Infected

    SI[i,k-1, s] ~ dbin(phiI[i,k-1, s]^days[i,k-1], NI[i, k-1, s])
    TI[i,k-1, s] ~ dbin(psi_IN[i,k-1, s]^days[i,k-1], SI[i, k-1, s])
    GI[i,k-1, s] ~ dpois(GaI[i, k-1, s])
    	log(GaI[i, k-1, s]) <- gammaI[s] + beta.g[s] * daysS[i,k-1]

#-------- Population Size

    NN[i, k, s] <- SN[i,k-1, s] + GN[i,k-1, s] + TI[i,k-1, s] - TN[i,k-1, s]
    NI[i, k, s] <- SI[i,k-1, s] + GI[i,k-1, s] - TI[i,k-1, s] + TN[i,k-1, s]

    Ntot[i, k, s] <- NN[i, k, s] + NI[i, k, s]
    Prev[i, k, s] <- NI[i, k, s] / (Ntot[i, k, s]+0.001)

    }
    NN_all[i, k] <- sum(NN[i, k, ])
    NI_all[i, k] <- sum(NI[i, k, ])
    Ntot_all[i, k] <- NN_all[i, k] + NI_all[i, k]

  }
}

#------------- Obervation model

for(i in 1:R){
  for(k in 1:K){
    for(j in 1:T){
      for(s in 1:nspecies){

#--------- Probability of detecting a host

     yN[i, j, k, s] ~ dbin(pN[i, j, k, s], NN[i, k, s])
        logit(pN[i,j,k,s]) <- alpha.pN[Se[k]] + beta.pN * obs[i,j,k]
     yN.new[i, j, k, s] ~ dbin(pN[i, j, k, s], NN[i, k, s])

     yI[i, j, k, s] ~ dbin(pI[i, j, k, s], NI[i, k, s]) 
        logit(pI[i, j, k, s]) <- alpha.pI[Se[k]] + beta.pN * obs[i,j,k] + beta.II * II2[i, j, k, s]

     yI.new[i, j, k, s] ~ dbin(pI[i, j, k, s], NI[i, k, s]) 

#-------- Probability of detecting the pathogen on a host

    cI[i, j, k, s] ~ dsum(yI[i, j, k, s], (-1 * mis[i, j, k, s])) 
    cU[i, j, k, s] ~ dsum(yN[i, j, k, s], mis[i, j, k, s]) 

     mis[i, j, k, s] ~ dbin(theta2[i, j, k, s], x[i, j, k, s]) 	
        theta2[i, j, k, s] <- 1 - theta[i, j, k, s]
        logit(theta[i, j, k, s]) <- a_I + b_I * II2[i, j, k, s]
      }
    }
  }
}


##------------ Bayesian p-value

for(k in 1:K){
  for(i in 1:R){
    for(j in 1:T){
      for(s in 1:nspecies){
    evalN[i,j,k,s] <- pN[i,j, k,s] * NN[i,k,s] 						
    evalI[i,j,k,s] <- pI[i,j, k,s] * NI[i,k,s] 								

    EN[i,j,k,s] <- pow((yN[i, j, k,s] - evalN[i,j,k,s]),2) / (evalN[i,j,k,s] + 0.5) 	
    EI[i,j,k,s] <- pow((yI[i, j, k,s] - evalI[i,j,k,s]),2) / (evalI[i,j,k,s] + 0.5) 	

    E.newN[i,j,k,s] <- pow((yN.new[i,j,k,s] - evalN[i,j,k,s]),2) / (evalN[i,j,k,s] + 0.5)  
    E.newI[i,j,k,s] <- pow((yI.new[i,j,k,s] - evalI[i,j,k,s]),2) / (evalI[i,j,k,s] + 0.5)  
      } #ss
    } #js									
  } #is
} # ks


zzzfitN 		<- sum(EN[,,,]) 	
zzzfitN.new  <- sum(E.newN[,,,])

zzzfitI		<- sum(EI[,,,]) 	
zzzfitI.new  <- sum(E.newI[,,,])

for(k in 1:K){
  zzzfitII[k]		<- sum(EI[,,k,]) 	
  zzzfitII.new[k]  <- sum(E.newI[,,k,])
}


#---------- Derived quantities
#---- Entire population size

for(s in 1:nspecies){
  for(k in 1:K){
    NT[k,s] <- sum(Ntot[,k,s])
    NNT[k,s]<- sum(NN[,k,s])
    NIT[k,s]<- sum(NI[,k,s])
  }
}

}
", fill = TRUE)
sink()

#----- Indicies

K = dim(host)[3]
R = nrow(host)
T = ncol(host)
nspecies = 4

#----- Host

H2 <- host
H2[is.na(H2[,,,])] <- round(mean(c(H2[,,1:6,]), na.rm = T))

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
				         nseason = 2,
				         nspecies = 4,

				         days = days3,   # Used for Survival & Transistions
				         daysS = daysS,  # Used for Recruitment
				         
				         obs = obs,  # Used for host detection model
				         
				         Se = c(1, 1, 1, 2, 1, 2), # Used in detection
				         
				         II2 = II_log   # Used for theta and in detection
				         )
#--------------------- Initial values

Nt <- colSums(apply(host, c(1, 3, 4), sum, na.rm = TRUE))+
colSums(apply(Ihost, c(1, 3), sum, na.rm = TRUE))

nI <-colSums(apply(Ihost, c(1, 3, 4), sum, na.rm = TRUE))
nn <-colSums(apply(host, c(1, 3, 4), sum, na.rm = TRUE))

#---
theta2 <- theta

yI <- round(win.data$cI + (win.data$x * theta2))
yN <- round(win.data$cU - (win.data$x * theta2))


NIst <- apply(yI, c(1, 3, 4), max, na.rm = TRUE)+1
  NIst[NIst == "-Inf"] <- 1
NIst1 <- apply(yI, c(1, 3, 4), max, na.rm = TRUE)
  NIst1[NIst1 == "-Inf"] <- 1

NNst <- apply(yN, c(1, 3, 4), max, na.rm = TRUE)+1
  NNst[NNst == "-Inf"] <- 1
NNst2 <- apply(yN, c(1, 3, 4), sum, na.rm = TRUE)
  NNst2[NNst2 == "-Inf"] <- 1

SI <- GI <- array(NA, c(R, K-1, nspecies))
SN <- GN <- array(NA, c(R, K-1, nspecies))
TNn <- TIi <- array(NA, c(R, K-1, nspecies))

SI[] <- 1
SN[] <- 1

TIi[] <- 0
TNn[] <- 0

GI <- NIst[,-1,] - SI 
GN <- NNst[,-1,] - SN 

NNst2[ , -1,] <- NA
NNst[ , -1,] <- NA
NIst1[ , -1,] <- NA
NIst[ , -1,] <- NA

NIst1[NIst1 == 0] <- 1

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
                                               
                       alpha.lamN = runif(nspecies, -1, 0),
                       alpha.lamI = runif(nspecies, -1, 0),
                       
                       alpha_N = runif(nspecies, 2, 3),
                       alpha_I = runif(nspecies, 2, 3),

                       gammaN = runif(nspecies, -1, 0),
                       gammaI = runif(nspecies, -1, 0), 
                       beta.g = runif(nspecies, 0, 1),
                       
                       alpha_IN = runif(1, -2, 0),
                       alpha_NI = runif(1, -2, 0),
                       beta.SR = runif(1, 0, 1),
                       
                       alpha.pI = runif(2, -2, 0),
                         #matrix(runif(nspecies * 2, -3, -1), nrow = 2, ncol = nspecies),
                       alpha.pN = runif(2, -2, 0),
                         #matrix(runif(nspecies * 2, -3, -1), nrow = 2, ncol = nspecies),
                       
                       beta.pN = runif(1, 0, 1),
                       beta.II = runif(1, 0, 1),
                       q = runif(1, 0, 1)

)}

#--------- Monitor Parameters

params <- c("alpha.lamN", "alpha.lamI",
            "alpha_N", "alpha_I",
            "gammaN", "gammaI", "beta.g",
            
            "alpha_IN",
            "alpha_NI", "beta.SR",

            "q",

            "alpha.pN","alpha.pI",
            "beta.pN", "beta.II",
            
           "zzzfitN", "zzzfitN.new",
           "zzzfitI", "zzzfitI.new",
           "zzzfitII", "zzzfitII.new",
           
           "NT",
           "NNT",
           "NIT",
           "a_I", "b_I"
	          )

#--------- MCMC settings

ni <- 200000
nb <- 20000
nt <- 200
nc <- 3

#--------- Model Run

library("jagsUI")

out3 <- jags(win.data, inits, params, "model2.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb, parallel = TRUE)







#--- out = saving Ntot and Prevalence;
#--- Need to 


setwd("/Users/Cici/GitHub/4speciesmodel/Model_output/")
save(out3, file = "years2010to2014_aggregated_p_q_alpha_NI_beta_SR.rda")

