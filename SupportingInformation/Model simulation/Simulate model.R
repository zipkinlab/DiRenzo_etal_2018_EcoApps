# In this code, we simulate the data under 2 scenarios
  # high variance in parameter estimates across species
  # low variance in parameter estimates across species

# Need to simulate species data
# Then aggregate for the analysis

#------ Function to simulate the data
#R = 100 # Number of sites
#T = 8   # Number of replicate surveys per site
#K = 10   # Number of seasons
#S = 34  # Number of species
#
#lam = c(4, 3)     # Average number of hosts per site the first season, #uninfected & infected
#
#phi = c(2.2, 0.84)  # Apparent host survival probability of uninfected and #infected hosts
#stdev = c(0.1, 0.1)
#
#gamma = c(2, 2) # Average number of individuals arriving at each site for #uninfected and infected hosts
#stdev_g = c(0.1, 0.1)
#
#p = c(1.38, 0.84) # Detection probability during each survey
#stdev_p = c(0.1, 0.1)
#
#recovery = -2.2  # Recovery probability
#stdev_rec = 0.1
#
#infection = -2.2  # Infection probability
#stdev_inf = 0.1


data.fn <- function(R = 300, # Number of sites
                    T = 4,   # Number of replicate surveys per site
                    K = 6,   # Number of seasons
                    S = 10,  # Number of species
                    
                    lam = c(1, 1),   # Average number of hosts per site the first season
                    stdev_lam = c(0.1, 0.1),
                    
                    phi = c(2.2, 0.84),  # Apparent host survival probability 
                    stdev = c(0.1, 0.1),
                    
                    gamma = c(1, 1), # Average number of individuals arriving at each site 
                    stdev_g = c(0.1, 0.1),
                    
                    p = c(1.38, 0.84),  # Detection probability during each survey
                    stdev_p = c(0.1, 0.1),
                    
                    recovery = -2.2,  # Recovery probability
                    stdev_rec = 0.1,
                    infection = -2.2,  # Infection probability
                    stdev_inf = 0.1
                    ){
  
# Empty matrices to hold the data    
  NI <- NN <- array(NA, dim = c(S, R, K))   # True abundance data
  
  
#-------- First season 
  S_lamN <- rnorm(n = S, mean = lam[1], sd= stdev_lam[1])
  S_lamI <- rnorm(n = S, mean = lam[2], sd= stdev_lam[2])
  
  for(s in 1:S){
    NN[s, ,1] <- rpois(n = R, lambda = S_lamN[s])
    NI[s, ,1] <- rpois(n = R, lambda = S_lamI[s])
  }
  
#------ Second season  
# Empty matrices to hold latent abundance variables 
# i.e., number of hosts surviving, arriving, and transitioning

SN <- SI <- GI <- GN <- TrN <- TrI <- array(0, dim = c(S, R, K-1))  

S_phiN <- plogis(rnorm(n = S, mean = phi[1], sd = stdev[1]))
S_phiI <- plogis(rnorm(n = S, mean = phi[2], sd = stdev[2]))

S_infection <- plogis(rnorm(n = S, mean = infection, sd = stdev_inf))
S_recovery  <- plogis(rnorm(n = S, mean = recovery, sd = stdev_rec))

S_gammaN <- rnorm(n = S, mean = gamma[1], sd = stdev_g[1])
S_gammaI <- rnorm(n = S, mean = gamma[2], sd = stdev_g[2])

for(k in 2:K){  
  
  for(s in 1:S){
    
    for(i in 1:R){
    
    if(NN[s, i, k-1]>0){
      SN[s, i, k-1] <- rbinom(n=1, size=NN[s, i, k-1], prob=S_phiN[s])       
           # Survival of not infecteds
      TrN[s, i,k-1] <- rbinom(n=1, size=SN[s, i, k-1], prob=S_infection[s])    
           # Getting infected
    }
      
    if(NI[s, i, k-1]>0){
      SI[s, i, k-1] <-  rbinom(n=1, size=NI[s, i, k-1], prob=S_phiI[s])  
           # Survival of infecteds
      TrI[s, i, k-1] <- rbinom(n=1, size=SI[s, i, k-1], prob=S_recovery[s]) 
           # Losing infection
    }
    # Recruitment
    GI[s, i, k-1] <- rpois(1, lambda = S_gammaN[s])
    GN[s, i, k-1] <- rpois(1, lambda = S_gammaI[s])
    
  }
  
  # Total
  NI[s, ,k] <-  SI[s, ,k-1] + GI[s, ,k-1] + TrN[s, ,k-1] - TrI[s, ,k-1]
  NN[s, ,k] <-  SN[s, ,k-1] + GN[s, ,k-1] + TrI[s, ,k-1] - TrN[s, ,k-1]
  
  }
}

# Aggregate the data across species
# We would sum it
NN2 <- apply(NN, c(2, 3), sum)
NI2 <- apply(NI, c(2, 3), sum)

# 
yN <- yI <- array(NA, dim = c(R, T, K)) # Observed abundance data

# Obervation process
#S_pN <- plogis(rnorm(n = S, mean = p[1], sd = stdev_p[1]))
#S_pI <- plogis(rnorm(n = S, mean = p[2], sd = stdev_p[2]))
  
for(i in 1:R){
  for(j in 1:T){
    for(k in 1:K){
      yN[i, j, k] <- rbinom(n = 1, NN2[i,k], plogis(p[1]))
      yI[i, j, k] <- rbinom(n = 1, NI2[i,k], plogis(p[2]))
    }
  }
}

  return(list(R = R, T = T, K = K, S = S,
              lam = lam,
              stdev_lam = stdev_lam,
              phi = phi,
              stdev = stdev,
              gamma = gamma,
              stdev_g = stdev_g,
              infection = infection, stdev_inf = stdev_inf,
              recovery = recovery, stdev_rec = stdev_rec,
              SN = SN,
              SI = SI,
              GN = GN,
              GI = GI,
              TrI = TrI,
              TrN = TrN,
              NN = NN, 
              NI = NI,
              p = p, 
              stdev_p = stdev_p,
              yN = yN,
              yN2 = yN2,
              yI = yI,
              yI2 = yI2))
}

# Simulate the data
sodata<- data.fn()

# --------------- Code model in BUGS language


sink("model.txt")
cat("
model{

# Priors

#------- NOT Infected
alpha.lamN  ~ dnorm(0,0.001)
pN     ~ dunif(0,1)
gammaN ~ dnorm(0,0.001) 
phiN   ~ dunif(0,1) 
psi_NI ~ dunif(0,1)

#------- Infected
alpha.lamI  ~ dnorm(0,0.001)
pI     ~ dunif(0,1)
gammaI ~ dnorm(0,0.001)  
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

  NN[i, k] <- SN[i,k] - TN[i,k] + GN[i,k] + TI[i,k]  
  NI[i, k] <- SI[i,k] - TI[i,k] + GI[i,k] + TN[i,k]  

  }
}

#------------- Obervation model

for(i in 1:R){
  for(j in 1:T){
    for(k in 1:K){

      yN[i, j, k] ~ dbin(pN, NN[i, k]) 
      yI[i, j, k] ~ dbin(pI, NI[i, k]) 

   #   y.newN[i, j, k] ~ dbin(pN, NN[i, k])  
  #    y.newI[i, j, k] ~ dbin(pI, NI[i, k])  

    }
  }
}

# Posterior predictive check

#for(k in 1:K){
#  for(i in 1:R){
#
#    evalN[i,k] <-  pN * NN[i,k]
#    evalI[i,k] <-  pI * NI[i,k]
#
#    for(j in 1:T){
#
#      EN[i,j,k] <- pow((yN[i,j,k] - evalN[i,k]),2) / (evalN[i,k] + 0.5)
#      EI[i,j,k] <- pow((yI[i,j,k] - evalI[i,k]),2) / (evalI[i,k] + 0.5)
#
#      E.newN[i,j,k] <- pow((y.newN[i,j,k] - evalN[i,k]),2) / (evalN[i,k] + 0#.5)  
#      E.newI[i,j,k] <- pow((y.newI[i,j,k] - evalI[i,k]),2) / (evalI[i,k] + 0#.5)  
#
#    } #js
#  } #is
#} #ks
#
#zzzfitN <- sum(EN[,,])
#zzzfitN.new  <- sum(E.newN[,,])
#
#zzzfitI <- sum(EI[,,])
#zzzfitI.new  <- sum(E.newI[,,])

}
", fill = TRUE)
sink()

# Bundle data
win.data <- list(yN = sodata$yN2, 
                 yI = sodata$yI2,
                 R = dim(sodata$yN2)[1], 
                 T = dim(sodata$yN2)[2],
                 K = dim(sodata$yN2)[3])

inits <- function() {list( 
  
  alpha.lamN = sodata$lam[1] * sodata$S,
  pN = runif(1, 0.9, 1),
  phiN = runif(1, 0.7, 1),
  gammaN = sodata$gamma[1] * sodata$S,
  psi_NI = runif(1, 0, 0.3),
  
  alpha.lamI = sodata$lam[2] * sodata$S,
  pI = runif(1, 0.9, 1),
  phiI = runif(1, 0.7, 1),
  gammaI = sodata$gamma[2] * sodata$S,                        
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
ni <- 10000
nb <- 1000
nt <- 10
nc <- 3


library("jagsUI")

out <- jags(win.data, inits, params, "model.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb, parallel = TRUE)



