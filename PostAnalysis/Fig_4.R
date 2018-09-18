# This code will create a 3 panel figure
 # A: transmission vs. covariate
 # B: denisty vs. prevalence
 # C: Summation of Bd vs. density


#---- Before running this code, please run:
# Format Species Richness.R
# 10to14_Model_Format.R

#--- Load library
library(coda)

#-- Set plotting specifications

par(mfrow = c(4, 1))

#--- Load data
load(file = "/Users/Cici/GitHub/2010to2014ElCope/Model_output_w_covariates/years2010to2014.rda")

# Stack the three chains for every model run
den <- rbind(as.matrix(out$samples[[1]]), 
             as.matrix(out$samples[[2]]), 
             as.matrix(out$samples[[3]]))
den <- as.mcmc(den)

#---- Figure A: Transmission vs. density
NN <- runif(100, 0, 6)
NI <- runif(100, 0, 6)

Ntot <- NI + NN


#----- Calculate means and 95% Credible intervals
pr <- 0.95
HD <- cbind(colMeans(den), HPDinterval(den, prob = pr))

#----- Create mean lines for the graphs

q <- out$mean$q

host_density <- ((NN^(1-q)) +  (NI/(Ntot^q +0.001)))

host_densityScale <- (host_density - 2.85)

HOST_tr <-  plogis(HD["alpha_NI", 1] + HD["beta.t", 1]* host_densityScale)

mcmc.sample <- nrow(den)

array.pred.host.tr <- array(NA, dim = c(length(host_density), mcmc.sample))

covariate <- array(NA, dim = c(length(host_density), mcmc.sample+1))

covariate[,1] <- host_densityScale

for(i in 1:mcmc.sample){
  
  q <- den[i, grep("q", colnames(den))]
  
  host_density <- ((NN^(1-q)) +  (NI/(Ntot^q +0.001)))
  
  host_densityScale <- (host_density - 2.85)
  
  covariate[,i+1] <- host_densityScale
  
  array.pred.host.tr[,i] <- plogis(den[i, grep("alpha_NI", colnames(den))]+ den[i, grep("beta.t", colnames(den))]* host_densityScale)
  
}


#sub.set <- sort(sample(1:mcmc.sample, size = 150))

ord <- order(covariate[,1])


#--- Run the file name:
# 10to14_Model_format.R
# Format Species Richness.R
# Format_theta.r

#---- Start plot here

sub.set <- sort(sample(1:mcmc.sample, size = 150))

par(mfrow = c(4,1))
par(mar = c(0, 2, 1, 50), oma = c(4, 4, 0.5, 0.5))
par(mgp = c(2, 0.6, 0))
par(tcl = -0.25)


plot(HOST_tr[ord] ~ covariate[ord,1], 
     las = 1, ylab = "",
     xlab = "",
     lwd = 4, type = "l",  main = "",
     cex.lab = 1, cex.axis = 1.2, ylim = c(0, 1),
     xlim = c(-0.85, 8),
     yaxt = "n",
     col = "white",     
     xaxt = "n"
)

mtext("Transmission probability", side = 2, line = 2.5, cex = 1)
#axis(1, at = (1:10 - 1.85), labels = 1:10)
axis(side = 1, at = c(1:10-1.85), labels = FALSE, tck = -0.02)
axis(side = 1, at = c(1:10-1.85), labels = FALSE, tck = 0.02)
axis(side = 2, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, cex.axis = 1.2)

for(i in sub.set){
  ord <- order(covariate[,i+1])
  
  lines(covariate[ord,i+1], array.pred.host.tr[ord,i+1], type = "l", lwd = 1, col = "gray65")
  
}

lines(HOST_tr[ord] ~ covariate[ord,1], type = "l", lwd = 4, col = "black")
lines(HOST_tr[ord] ~ covariate[ord,1], type = "l", lwd = 3, col = "steelblue")

text("A", x = 9.9-1.85, y = 0.95, cex = 3)

#---- Figure B: Correlation between host abundance and pathogen prevalence 


#-- Looking at differences between season t+1 and season t
#-- if it is positive the pop grew
#-- if it is negative the pop declined


Ihost2 <- apply(Ihost, c(1, 3), max, na.rm = TRUE) 
Ihost2[Ihost2 == "-Inf"] <- 0

All2 <- apply(All, c(1, 3), max, na.rm = TRUE) 
All2[All2 == "-Inf"] <- 0

difference <- array(NA, dim = c(dim(Ihost)[1], dim(Ihost)[3]-1))

for(i in 1:dim(Ihost)[1]){
  for(k in 2:6){
    difference[i, k-1] <- (Ihost2[i, k] - Ihost2[i, k-1])/(Ihost2[i, k-1]+1)
  }
}

plot(jitter(c(difference)) ~ jitter(c(All2[,-dim(All2)[2]])), 
     pch = 21, col = "black", bg = "steelblue",
     xlim = c(1, 10),
   #  ylim = c(-4, 4),
     xaxt = "n",
    # yaxt = "n",
     xlab = "", 
     ylab = "",
     las = 1)

axis(side = 1, at = c(1:10), labels = FALSE, tck = -0.02)
axis(side = 1, at = c(1:10), labels = FALSE, tck = 0.02)

mtext(expression(paste(Delta, " per capita number ")), side = 2, line = 4, cex = 1)
mtext("of infected hosts", side = 2, line = 3.1, cex = 1)
mtext(expression(paste("(season ", italic(t-1), " to ", italic(t), ")")), side = 2, line = 1.6, cex = 1)

text("B", x = 10, y = 3.7, cex = 3)

abline(lm(c(difference)~ c(All2[,-dim(All2)[2]])), col = "black", lwd = 4)
abline(lm(c(difference)~ c(All2[,-dim(All2)[2]])), col = "steelblue", lwd = 3)


#----- Figure C.
#--- All

All <- host + Ihost + Uhost
  # All hosts ever captured at a site
  # All uninfected, infected, and unknown hosts

Infection <- log10(II +1)
  # apply(II, c(1,3), mean, na.rm = TRUE)+1)
  # The summation of all hosts during each site, survey, year
  # Then taking the average

abundance <- All
  # apply(All, c(1,3), max, na.rm = TRUE)
  # Taking the maximum number of hosts found
abundance[abundance == "-Inf"] <- NA

plot(jitter(c(Infection/(abundance+0.01)))~ jitter(c(abundance)), pch = 21, col = "black", bg = "steelblue",
     xlab = "", 
     ylab = "",
     xlim = c(1, 10),
     xaxt = "n",
     yaxt = "n",
     las = 1)
mtext(expression(paste("Mean ", italic(Bd), " load per host", sep = "")), side = 2, line = 2.5, cex = 1)
axis(2, at = c(0, 1, 2, 3, 4), lab = c(0, 1, 10, 100, 1000), cex.axis = 1.2, las = 1)
text("C", x = 10, y = 3.9, cex = 3)

#--- remove sites with abundance = 0
meanBd <- c(Infection/(abundance))
abundance2 <- abundance

remove <- which(c(abundance) == 0)
remove2 <- which(c(meanBd) == 0)


meanBd <- meanBd[-c(remove, remove2)]
abundance2 <- abundance2[-c(remove, remove2)]

# Plot the lines
abline(lm(meanBd~ abundance2), col = "black", lwd = 4)
abline(lm(meanBd~ abundance2), col = "steelblue", lwd = 3)
       
axis(side = 1, at = c(1:10), labels = FALSE, tck = -0.02)
axis(side = 1, at = c(1:10), labels = FALSE, tck = 0.02)


#----- Bottom x-axis
axis(side = 1, at = c(1:10), labels = FALSE, tck = -0.02)
axis(side = 1, at = c(1:10), labels = 1:10, tck = 0.02, cex.axis = 1)

mtext(side = 1, "Host abundance (frogs/20m site)", line = 2, cex = 1)

