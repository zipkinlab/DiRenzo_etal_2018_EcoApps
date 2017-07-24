#------ Function to simulate the data

data.fn <- function(R = 100, # Number of sites
                    T = 3,   # Number of replicate surveys per site
                    K = 3,   # Number of seasons
                    
                    alpha.lam = 3,   # Average number of hosts per site the first season
                    phi = c(0.9, 0.7),  # Apparent host survival probability of uninfected and infected hosts
                    gamma = c(2, 1), # Average number of individuals arriving at each site for uninfected and infected hosts
                    p = c(0.8, 0.8, 0.8),  # Detection probability during each survey
                    recovery = 0.1,  # Recovery probability
                    infection = 0.1  # Infection probability
                    ){
  
# Empty matrices to hold the data    
  yN <- yI <- array(NA, dim = c(R, T, K))   # Observed abundance data
  NI <- NN <- array(NA, dim = c(R, K))      # True abundance data
  
  
#-------- First season 
  NN[,1] <- rpois(n = R, lambda = alpha.lam)
  NI[,1] <- rpois(n = R, lambda = alpha.lam)
  
  for(i in 1:R){
    for(j in 1:T){
      yN[i,j, 1] <- rbinom(n = 1, NN[i,1], p[1])
      yI[i,j, 1] <- rbinom(n = 1, NI[i,1], p[1])
    }
  }
  
  #------ Second season  
  # Empty matrices to hold latent abundance variables 
  # i.e., number of hosts surviving, arriving, and transitioning
  
  SN <- SI <- GI <- GN <- TrN <- TrI <- array(0, dim = c(R, K-1))  
  
  for(k in 2:K){  
    
    for(i in 1:R){
      
      if(NN[i,k-1]>0){
        SN[i, k-1] <- rbinom(n=1, size=NN[i,k-1], prob=phi[1])       
             # Survival of not infecteds
        TrN[i,k-1] <- rbinom(n=1, size=SN[i,k-1], prob=infection)    
             # Getting infected
      }
      if(NI[i,k-1]>0){
        SI[i, k-1] <-  rbinom(n=1, size=NI[i,k-1], prob=phi[2])   
             # Survival of infecteds
        TrI[i, k-1] <- rbinom(n=1, size=SI[i,k-1], prob=recovery) 
             # Losing infection
      }
      # Recruitment
      GI[i, k-1] <- rpois(1, lambda = gamma[2])
      GN[i, k-1] <- rpois(1, lambda = gamma[1])
      
    }
    
    # Total
    NI[,k] <-  SI[,k-1] + GI[,k-1] + TrN[,k-1] - TrI[,k-1]
    NN[,k] <-  SN[,k-1] + GN[,k-1] + TrI[,k-1] - TrN[,k-1]
    
  }
  
# Obervation process
  
  for(i in 1:R){
    
    for(j in 1:T){
      
      for(k in 2:K){
        
        yN[i, j, k] <- rbinom(n = 1, NN[i,k], p[k])
        yI[i, j, k] <- rbinom(n = 1, NI[i,k], p[k])
        
      }
    }
  }
  
  
  return(list(R = R, T = T, K = K,
              alpha.lam= alpha.lam,
              phi = phi,
              gamma = gamma,
              infection = infection,
              recovery = recovery,
              SN = SN,
              SI = SI,
              GN = GN,
              GI = GI,
              TrI = TrI,
              TrN = TrN,
              NN = NN, 
              NI = NI,
              p = p, 
              yN = yN,
              yI = yI))
}


# --------------- Code model in BUGS language


sink("model.txt")
cat("
model{

# Priors

#------- NOT Infected
alpha.lamN  ~ dnorm(0,0.01)
pN     ~ dunif(0,1)
gammaN ~ dnorm(0,0.01) 
phiN   ~ dunif(0,1) 
psi_NI ~ dunif(0,1)

#------- Infected
alpha.lamI  ~ dnorm(0,0.01)
pI     ~ dunif(0,1)
gammaI ~ dnorm(0,0.01)  
phiI   ~ dunif(0,1) 
psi_IN ~ dunif(0,1) 

#------------ Ecological model
#---- First season

for(i in 1:R){
  #------ Not infected
    NN[i, 1] ~ dpois(lambdaN[i])
      log(lambdaN[i]) <- alpha.lamN

  #----- Infected
    NI[i, 1] ~ dpois(lambdaI[i])  
      log(lambdaI[i]) <- alpha.lamI
}

#------ All other seasons
for(k in 2:K){

  for(i in 1:R){

    #------- Nost Infected

    SN[i,k] ~ dbin(phiN, NN[i,k-1])  # Total survivors
    TN[i,k] ~ dbin(psi_NI, SN[i,k])  # Survive, become infected

    GN[i,k] ~ dpois(GaN[i, k])   # Recruits
      log(GaN[i, k]) <- gammaN

    #------- Infected

    SI[i,k] ~ dbin(phiI, NI[i,k-1] ) # Infecteds who survive
    TI[i,k] ~ dbin(psi_IN, SI[i,k] ) # Get better, transition to uninfected

    GI[i,k] ~ dpois(GaI[i, k])  # Recruits
      log(GaI[i, k]) <- gammaI

# Totals

  NN[i, k]  <- SN[i,k] - TN[i,k] + GN[i,k] + TI[i,k]  
  NI[i, k] <- SI[i,k] - TI[i,k] + GI[i,k]  + TN[i,k]  

  }

}

#------------- Obervation model

for(i in 1:R){

  for(j in 1:T){

    for(k in 1:K){

      yN[i, j, k] ~ dbin(pN, NN[i, k]) 
      yI[i, j, k] ~ dbin(pI, NI[i, k]) 

      y.newN[i, j, k] ~ dbin(pN, NN[i, k])  
      y.newI[i, j, k] ~ dbin(pI, NI[i, k])  

    }
  }
}

# Posterior predictive check

for(k in 1:K){

  for(i in 1:R){

    evalN[i,k] <-  pN * NN[i,k]
    evalI[i,k] <-  pI * NI[i,k]

    for(j in 1:T){

      EN[i,j,k] <- pow((yN[i,j,k] - evalN[i,k]),2) / (evalN[i,k] + 0.5)
      EI[i,j,k] <- pow((yI[i,j,k] - evalI[i,k]),2) / (evalI[i,k] + 0.5)

      E.newN[i,j,k] <- pow((y.newN[i,j,k] - evalN[i,k]),2) / (evalN[i,k] + 0.5)  
      E.newI[i,j,k] <- pow((y.newI[i,j,k] - evalI[i,k]),2) / (evalI[i,k] + 0.5)  

    } #js

  } #is

} #ks

zzzfitN <- sum(EN[,,])
zzzfitN.new  <- sum(E.newN[,,])

zzzfitI <- sum(EI[,,])
zzzfitI.new  <- sum(E.newI[,,])


}
", fill = TRUE)
sink()

# Simulate the data
sodata<- data.fn(R = 100, T = 3, K = 3,
                 alpha.lam = 3,
                 phi = c(0.9, 0.7),
                 gamma = c(2,1),
                 p = c(0.8, 0.8, 0.8),
                 recovery = 0.1,
                 infection = 0.1)

# Bundle data
win.data <- list(yN = sodata$yN, 
                 yI = sodata$yI,
                 R = nrow(sodata$yN), 
                 T = ncol(sodata$yN),
                 K = dim(sodata$yN)[3])

# Initial values
NIst <- apply(sodata$yI, c(1, 3), max, na.rm = TRUE)
NNst <- apply(sodata$yN, c(1, 3), max, na.rm = TRUE)

inits <- function() {list( 
  alpha.lamN = runif(1, 2, 3),
  pN = runif(1, 0.9, 1),
  phiN = runif(1, 0.7, 1),
  gammaN = runif(1, 2, 3),
  psi_NI = runif(1, 0, 0.3),
  
  alpha.lamI = runif(1, 2, 3),
  pI = runif(1, 0.9, 1),
  phiI = runif(1, 0.7, 1),
  gammaI = runif(1, 2, 3),                        
  psi_IN = runif(1, 0, 0.3) 
  
)}

# Monitor Parameters
params <- c("alpha.lamN", "alpha.lamI",
            "pN", "pI",
            "phiN", "phiI",
            "gammaN", "gammaI",
            "psi_NI", "psi_IN",
            
            "zzzfitN", "zzzfitN.new",
            "zzzfitI", "zzzfitI.new"
            
)

# MCMC settings
ni <- 24000
nb <- 4000
nt <- 10
nc <- 3


library("jagsUI")

out <- jags(win.data, inits, params, "model.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb, parallel = TRUE)