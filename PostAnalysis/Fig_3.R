library(coda)


load(file = "/Users/Cici/GitHub/2010to2014ElCope/Model_output_w_covariates/years2010to2014_Ntot_prev_out_thanksgiving.rda")

allchains2 <- rbind(as.matrix(out$samples[[1]]), 
                    as.matrix(out$samples[[2]]), 
                    as.matrix(out$samples[[3]]))

#----------#----------#----------#----------#
#----------# Recruitment figure #----------#----------#
#----------#----------#----------#----------#

gammaN <- allchains2[ , grep("gammaN", colnames(allchains2))]
gammaI <- allchains2[ , grep("gammaI", colnames(allchains2))]

gammaN <- as.mcmc(gammaN)
gammaI <- as.mcmc(gammaI)


lam <- matrix(NA, nrow = 2, ncol = 4)

colnames(lam) <- c("Population", "Mean", "ylo", "yhi")

lam[1, 2] <- mean(gammaN)
lam[1, 3] <- HPDinterval(as.mcmc(gammaN))[,1]
lam[1, 4] <- HPDinterval(as.mcmc(gammaN))[,2]

lam[2, 2] <- mean(gammaI)
lam[2, 3] <- HPDinterval(as.mcmc(gammaI))[,1]
lam[2, 4] <- HPDinterval(as.mcmc(gammaI))[,2]

lam[,1] <- rep(c("Uninfected", "Infected"), each  = 1)

lam <- as.data.frame(lam)

for (i in 2:4){
  lam[,i] <- as.numeric(as.character(lam[,i]))
  lam[,i] <- exp(lam[,i])
}

lam$Population <- factor(lam$Population, levels = c("Uninfected", "Infected"))

library(ggplot2)

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


alphaN <- allchains2[ , grep("alpha_N", colnames(allchains2))][,1]
alphaI <- allchains2[ , grep("alpha_I", colnames(allchains2))][,1]

alphaN <- as.mcmc(alphaN)
alphaI <- as.mcmc(alphaI)


lam <- matrix(NA, nrow = 2, ncol = 4)

colnames(lam) <- c("Population", "Mean", "ylo", "yhi")

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
 


### What about the slope

beta.g <- allchains2[ , grep("beta.g", colnames(allchains2))]

exp(mean(beta.g))
exp(HPDinterval(as.mcmc(beta.g), prob = 95))

alpha_NI <- allchains2[ , grep("alpha_NI", colnames(allchains2))]
plogis(mean(alpha_NI))
plogis(HPDinterval(as.mcmc(alpha_NI), prob = 95))

alpha_IN <- allchains2[ , grep("alpha_IN", colnames(allchains2))]
plogis(mean(alpha_IN))
plogis(HPDinterval(as.mcmc(alpha_IN), prob = 95))

beta.p <- allchains2[ , grep("beta.p", colnames(allchains2))]
exp(mean(beta.p))
exp(HPDinterval(as.mcmc(beta.p), prob = 95))
