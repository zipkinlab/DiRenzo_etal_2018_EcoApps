# Citation:
# DiRenzo, G. V., E. Zipkin, E. C. Grant, J. A. Royle, A. V. Longo, K. Z. Zamudio, and K. R. Lips. In review. Eco-evolutionary rescue promotes host-pathogen coexistence. Ecology.

# This file creates Figure 2 in the paper.

# Load library
library(coda)
library(ggplot2)

# Load data
load(file = "/Users/Cici/GitHub/2010to2014ElCope/Model_output_w_covariates/years2010to2014_Ntot_prev_out_thanksgiving.rda")

# Stack all the chains
allchains2 <- rbind(as.matrix(out$samples[[1]]), 
                    as.matrix(out$samples[[2]]), 
                    as.matrix(out$samples[[3]]))

#----------#----------#----------#----------#
#----------# Recruitment figure #----------#----------#
#----------#----------#----------#----------#

# Subset the gamma columns
gammaN <- allchains2[ , grep("gammaN", colnames(allchains2))]
gammaI <- allchains2[ , grep("gammaI", colnames(allchains2))]

# Convert to MCMC object
gammaN <- as.mcmc(gammaN)
gammaI <- as.mcmc(gammaI)

# Create an empty matrix
lam <- matrix(NA, nrow = 2, ncol = 4)

# Label the columns
colnames(lam) <- c("Population", "Mean", "ylo", "yhi")

# Fill in the matrix with the mean and 95% Credible interval for parameter estiamtes
lam[1, 2] <- mean(gammaN)
lam[1, 3] <- HPDinterval(as.mcmc(gammaN))[,1]
lam[1, 4] <- HPDinterval(as.mcmc(gammaN))[,2]

lam[2, 2] <- mean(gammaI)
lam[2, 3] <- HPDinterval(as.mcmc(gammaI))[,1]
lam[2, 4] <- HPDinterval(as.mcmc(gammaI))[,2]

# Give the row names
lam[,1] <- rep(c("Uninfected", "Infected"), each  = 1)

# Convert to data frame from matrix
lam <- as.data.frame(lam)

# Convert to numeric and backtransform the variables
for (i in 2:4){
  lam[,i] <- as.numeric(as.character(lam[,i]))
  lam[,i] <- exp(lam[,i])
}

# Convert column to factor
lam$Population <- factor(lam$Population, levels = c("Uninfected", "Infected"))

# Create the plot and save it as an object
recru <- ggplot(data = lam, aes(x = Population, y = Mean)) + 
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = ylo, ymax = yhi), width = 0.2)+
  xlab('Disease Class') +
  ylab(expression(paste("Expected number of hosts recruited per site(", gamma,")", sep = "")))+  
  theme_bw() + 
  ylim(c(0, 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        legend.title = element_text(size=9),
        legend.text = element_text(size=9),
        legend.position = c(0.8, 0.85),
        legend.direction = "vertical")+  
  annotate("text", x = 0.6, y = 1, label = "B", size = 8)

#----------#----------#----------#----------#
#----------# Survival figure #----------#----------#
#----------#----------#----------#----------#

# Subset the survival columns
alphaN <- allchains2[ , grep("alpha_N", colnames(allchains2))][,1]
alphaI <- allchains2[ , grep("alpha_I", colnames(allchains2))][,1]

# Convert to MCMC object
alphaN <- as.mcmc(alphaN)
alphaI <- as.mcmc(alphaI)

# Create an empty matrix
lam <- matrix(NA, nrow = 2, ncol = 4)

# Label the columns
colnames(lam) <- c("Population", "Mean", "ylo", "yhi")

# Fill in the matrix with the mean and 95% Credible interval for parameter estiamtes
lam[1, 2] <- mean(alphaN)
lam[1, 3] <- HPDinterval(as.mcmc(alphaN))[,1]
lam[1, 4] <- HPDinterval(as.mcmc(alphaN))[,2]

lam[2, 2] <- mean(alphaI)
lam[2, 3] <- HPDinterval(as.mcmc(alphaI))[,1]
lam[2, 4] <- HPDinterval(as.mcmc(alphaI))[,2]

lam[,1] <- c("Uninfected", "Infected")

lam <- as.data.frame(lam)

for (i in 2:4){
  lam[,i] <- as.numeric(as.character(lam[,i]))
  lam[,i] <- plogis(lam[,i])
}

lam$Population <- factor(lam$Population, levels = c("Uninfected", "Infected"))

library(ggplot2)

surv <- ggplot(data = lam, aes(x = Population, y = Mean)) + 
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = ylo, ymax = yhi), width = 0.2)+
  xlab('Disease Class') +
  ylab(expression(paste("Apparent monthly survival probability(", phi,")", sep = "")))+  
  theme_bw() + 
  ylim(c(0.8, 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        legend.position = "none")+  
  annotate("text", x = 0.6, y = 1, label = "A", size = 8)


#----------#----------#----------#----------#
#----------# Transmission #----------#----------#
#----------#----------#----------#----------#


#---- Combine
library(Rcpp)
library(ggplot2)
require(gridExtra)
library(cowplot)

plot_grid(surv, recru, align = "h", nrow = 1)
#ggsave("Surv_rec28Nov2016.pdf", width = 8, height = 5)
 