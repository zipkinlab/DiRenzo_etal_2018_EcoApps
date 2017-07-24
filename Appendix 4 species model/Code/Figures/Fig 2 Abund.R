library(coda)



load(file = "/Users/Cici/GitHub/4speciesmodel/Model_output/years2010to2014_aggregated_p_q_alpha_NI_beta_SR.rda")


allchains2 <- rbind(as.matrix(out3$samples[[1]]), 
                    as.matrix(out3$samples[[2]]), 
                    as.matrix(out3$samples[[3]]))


#------ Infected

NN <- allchains2[ , grep("NNT", colnames(allchains2))]
NI <- allchains2[ , grep("NIT", colnames(allchains2))]
Ntot <- allchains2[ , grep("NT", colnames(allchains2))][, 1:24]

NN <- as.mcmc(NN)
NI <- as.mcmc(NI)
NT <- as.mcmc(Ntot)

lam <- matrix(NA, nrow = 6*3*4, ncol = 6)

colnames(lam) <- c("Class", "Year", "Mean", "ylo", "yhi", "Species")

lam[1:24, 3] <- colMeans(NN)
lam[1:24, 4] <- HPDinterval(as.mcmc(NN))[,1]
lam[1:24, 5] <- HPDinterval(as.mcmc(NN))[,2]

lam[25:48, 3] <- colMeans(NI)
lam[25:48, 4] <- HPDinterval(as.mcmc(NI))[,1]
lam[25:48, 5] <- HPDinterval(as.mcmc(NI))[,2]

lam[49:72, 3] <- colMeans(NT)
lam[49:72, 4] <- HPDinterval(as.mcmc(NT))[,1]
lam[49:72, 5] <- HPDinterval(as.mcmc(NT))[,2]

lam[,1] <- rep(c("Uninfected", "Infected", "Total"), each = 24)

lam[,6] <- rep(rep(c("E. prosoblepon", "P. cerasinus", "P. cruentus", "S. albomaculata"), each  = 6), times = 3)

lam[,2] <- rep(c("2010W", "2011W", "2012W", "2013D", "2013W", "2014D"), times = 12)

lam <- as.data.frame(lam)



str(lam)

lam$Year <- factor(lam$Year, levels =c("2010W", "2011W", "2012W", "2013D", "2013W", "2014D")) 

lam$Class <- factor(lam$Class, levels = c("Total", "Uninfected", "Infected"))

for (i in 3:5){
  lam[,i] <- as.numeric(as.character(lam[,i]))
}
library(ggplot2)

abund <- ggplot(data = lam, aes(x = Year, y = Mean, ymin = ylo, ymax = yhi, col = Species, shape = Class)) + 
  geom_line(aes(x = Year, y = Mean))+
  geom_pointrange(position = position_dodge(width=0.5), size = 0.5)+
  xlab('Season-year') +
  ylab('Amphibian abundance')+  
  scale_color_manual(values = c("gold", "skyblue", "black", "orange")) +
 # ylim(c(0, 500))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        legend.title = element_text(size=9),
        legend.text = element_text(size=9),
        legend.position = c(0.7, 0.8),
        legend.direction = "horizontal")+
  annotate("text", x = 0.75, y = 350, label = "A", size = 8)

#----- Prevalence figure



lam <- matrix(NA, nrow = 6 * 4, ncol = 5)

colnames(lam) <- c( "Year", "Mean", "ylo", "yhi", "Species")

lam[1:24, 2] <- colMeans(NI/(NT+0.001))
lam[1:24, 3] <- HPDinterval(as.mcmc(NI/(NT+0.001)))[,1]
lam[1:24, 4] <- HPDinterval(as.mcmc(NI/(NT+0.001)))[,2]

lam[,1] <- rep(1:6, times = 4)

lam <- as.data.frame(lam)

for (i in 2:4){
  lam[,i] <- as.numeric(as.character(lam[,i]))
}

lam[,5] <- rep(c("E. prosoblepon", "P. cerasinus", "P. cruentus", "S. albomaculata"), each  = 6)

library(ggplot2)

prev <- ggplot(data = lam, aes(x = Year, y = Mean, ymin = ylo, ymax = yhi, col = Species)) + 
  geom_pointrange(position = position_dodge(width=0.5), size = 0.5)+
  scale_x_continuous("", breaks=c(1:6))+
  scale_color_manual(values = c("gold", "skyblue", "black", "orange")) +
  xlab('') +
  ylab('Pathogen prevalence')+  
  ylim(c(0, 1))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")+
  annotate("text", x = 0.5, y = 0.95, label = "B", size = 8)

#----- Infection intensity

#------ Read in the data

cope <- read.csv(file = "/Users/Cici/GitHub/2000to2014ElCope/Data_Raw files/Cope00_14.csv")

cope <- cope[cope$Species != "sp.",]
Cop <-cope[cope$yr == 2010| cope$yr == 2011 |
             cope$yr == 2012| cope$yr == 2013 |
             cope$yr == 2014,]

Cop <- droplevels(Cop)

Cop$Genus_species <- paste(Cop$Genus, Cop$Species, sep = "_")

Cop$season_yr <- paste(Cop$seasons, Cop$yr, sep = "_")

Cop$season_yr <- factor(Cop$season_yr, level = c("wet_2010", "wet_2011",
                                                 "wet_2012", "dry_2013",
                                                 "wet_2013", "dry_2014"))
#---- 

Cop <- Cop[is.na(Cop$LargestZooValue) == FALSE,]

Cop <- Cop[Cop$Species == "albomaculata" | Cop$Species == "cruentus"| Cop$Species == "cerasinus"| Cop$Species == "prosblepon",]

initial <- gsub("Sachatamia", "S", Cop$Genus)
initial <- gsub("Eleutherodactylus", "E", initial)
initial <- gsub("Eleutherodactylus", "E", initial)


Cop$G_S <- paste( initial, Cop$Species, sep = ". ")
  
  
Infection_intensity <- ggplot(data = Cop, aes(x = season_yr, y = (LargestZooValue+0.5), col = G_S)) + 
  geom_jitter(width = 0.4)+
  scale_x_discrete("Season-year",
                   labels=c("Wet10", "Wet11",
                            "Wet12", "Dry13", "Wet13", "Dry14"))+
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000), limits = c(0.4, 30500))+
  ylab(expression(paste("Mean ", italic(Bd), " infection intensity")))+  
  scale_color_manual(values = c("gold", "skyblue", "black", "orange")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none")+
  stat_summary(fun.data = "mean_cl_boot", size = 1, mapping = aes(group = G_S), position = position_dodge(width=0.5))+
  annotate("text", x = 0.55, y = 25000, label = "C", size = 8)+  geom_hline(aes(yintercept=10000), col = "red", lty = 2, size = 1.5)

#---- Combine
require(gridExtra)
grid.arrange(abund, prev, Infection_intensity, ncol=1, nrow = 3)
setwd("/Users/Cici/GitHub/4speciesmodel/Figures/")
#ggsave("CB_Fig2.pdf", width = 8, height = 5)
