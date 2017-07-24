library(coda)


load(file = "/Users/Cici/GitHub/4speciesmodel/Model_output/years2010to2014_aggregated_p_q_alpha_NI_beta_SR.rda")

allchains2 <- rbind(as.matrix(out3$samples[[1]]), 
                    as.matrix(out3$samples[[2]]), 
                    as.matrix(out3$samples[[3]]))

#----------#----------#----------#----------#
#----------# Recruitment figure #----------#----------#
#----------#----------#----------#----------#

gammaN <- allchains2[ , grep("gammaN", colnames(allchains2))]
gammaI <- allchains2[ , grep("gammaI", colnames(allchains2))]

gammaN <- as.mcmc(gammaN)
gammaI <- as.mcmc(gammaI)


lam <- matrix(NA, nrow = 8, ncol = 5)

colnames(lam) <- c("Population", "Mean", "ylo", "yhi", "Species")

lam[1:4, 2] <- colMeans(gammaN)
lam[1:4, 3] <- HPDinterval(as.mcmc(gammaN))[,1]
lam[1:4, 4] <- HPDinterval(as.mcmc(gammaN))[,2]

lam[5:8, 2] <- colMeans(gammaI)
lam[5:8, 3] <- HPDinterval(as.mcmc(gammaI))[,1]
lam[5:8, 4] <- HPDinterval(as.mcmc(gammaI))[,2]

lam[,1] <- rep(c("Uninfected", "Infected"), each  = 4)
lam[,5] <- rep(c("E. prosoblepon", "P. cerasinus", "P. cruentus", "S. albomaculata"), times  = 2)

lam <- as.data.frame(lam)

for (i in 2:4){
  lam[,i] <- as.numeric(as.character(lam[,i]))
  lam[,i] <- exp(lam[,i])
}

lam$Population <- factor(lam$Population, levels = c("Uninfected", "Infected"))

lam$Species <- factor(lam$Species, levels = c( "P. cerasinus", "P. cruentus", "E. prosoblepon", "S. albomaculata"))
library(ggplot2)

recru <- ggplot(data = lam, aes(x = Population, y = Mean, col = Species)) + 
  geom_point(size = 3, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = ylo, ymax = yhi), width = 0.2, position=position_dodge(width=0.5))+
  scale_color_manual(values = c("gold", "skyblue", "black", "orange")) +
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
        legend.position = c(0.75, 0.85),
        legend.direction = "vertical")+  
  annotate("text", x = 0.6, y = 1, label = "B", size = 8)

#----------#----------#----------#----------#
#----------# Survival figure #----------#----------#
#----------#----------#----------#----------#


alphaN <- allchains2[ , grep("alpha_N", colnames(allchains2))][,1:4]
alphaI <- allchains2[ , grep("alpha_I", colnames(allchains2))][,1:4]

alphaN <- as.mcmc(alphaN)
alphaI <- as.mcmc(alphaI)


lam <- matrix(NA, nrow = 8, ncol = 5)

colnames(lam) <- c("Population", "Mean", "ylo", "yhi", "Species")

lam[1:4, 2] <- colMeans(alphaN)
lam[1:4, 3] <- HPDinterval(as.mcmc(alphaN))[,1]
lam[1:4, 4] <- HPDinterval(as.mcmc(alphaN))[,2]

lam[5:8, 2] <- colMeans(alphaI)
lam[5:8, 3] <- HPDinterval(as.mcmc(alphaI))[,1]
lam[5:8, 4] <- HPDinterval(as.mcmc(alphaI))[,2]

lam[,1] <- rep(c("Uninfected", "Infected"), each = 4)
lam[,5] <- rep(c("E. prosoblepon", "P. cerasinus", "P. cruentus", "S. albomaculata"), times  = 2)

lam <- as.data.frame(lam)

for (i in 2:4){
  lam[,i] <- as.numeric(as.character(lam[,i]))
  lam[,i] <- plogis(lam[,i])
}

lam$Population <- factor(lam$Population, levels = c("Uninfected", "Infected"))

lam$Species <- factor(lam$Species, levels = c( "P. cerasinus", "P. cruentus", "E. prosoblepon", "S. albomaculata"))

library(ggplot2)

surv <- ggplot(data = lam, aes(x = Population, y = Mean, col = Species)) + 
  geom_point(size = 3, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = ylo, ymax = yhi), width = 0.2, position=position_dodge(width=0.5))+
  scale_color_manual(values = c("gold", "skyblue", "black", "orange")) +
  xlab('Disease Class') +
  ylab(expression(paste("Apparent monthly survival probability(", phi,")", sep = "")))+  
  theme_bw() + 
  ylim(c(0, 1))+
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
setwd("/Users/Cici/GitHub/4speciesmodel/Figures/")
ggsave("CB_Fig1.pdf", width = 8, height = 5)
 
