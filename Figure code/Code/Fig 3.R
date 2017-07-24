# Citation:
# DiRenzo, G. V., E. Zipkin, E. C. Grant, J. A. Royle, A. V. Longo, K. Z. Zamudio, and K. R. Lips. In review. Eco-evolutionary rescue promotes host-pathogen coexistence. Ecology.

# This file creates Figure 3 in the paper.

# Load library
library(coda)
library(ggplot2)
library(plyr)
library(gridExtra)
library(cowplot)

# Load the data
load(file = "/Users/Cici/GitHub/2010to2014ElCope/Model_output_w_covariates/years2010to2014_Ntot_prev_out_thanksgiving.rda")

# Stack the chains
allchains2 <- rbind(as.matrix(out$samples[[1]]), 
                    as.matrix(out$samples[[2]]), 
                    as.matrix(out$samples[[3]]))

#------#------#------#------#
#------ Abundance ----------#
#------#------#------#------#

# Isoalte the columns of interest
NN <- allchains2[ , grep("NNT", colnames(allchains2))]
NI <- allchains2[ , grep("NIT", colnames(allchains2))]
Ntot <- allchains2[ , grep("NT", colnames(allchains2))[1:6]]

# Convert columns to MCMC objects
NN <- as.mcmc(NN)
NI <- as.mcmc(NI)
NT <- as.mcmc(Ntot)

# Create an empty column
lam <- matrix(NA, nrow = 18, ncol = 5)

# Add column names
colnames(lam) <- c("Population", "Year", "Mean", "ylo", "yhi")

# Fill in the columns with the mean and 95% CI
lam[1:6, 3] <- colMeans(NN)
lam[1:6, 4] <- HPDinterval(as.mcmc(NN))[,1]
lam[1:6, 5] <- HPDinterval(as.mcmc(NN))[,2]

lam[7:12, 3] <- colMeans(NI)
lam[7:12, 4] <- HPDinterval(as.mcmc(NI))[,1]
lam[7:12, 5] <- HPDinterval(as.mcmc(NI))[,2]

lam[13:18, 3] <- colMeans(NT)
lam[13:18, 4] <- HPDinterval(as.mcmc(NT))[,1]
lam[13:18, 5] <- HPDinterval(as.mcmc(NT))[,2]

lam[,1] <- rep(c("Uninfected", "Infected", "Total"), each = 6) 

# These are place holders
lam[,2] <- c(0.9, 1.9, 2.9, 3.9, 4.9, 5.9, 
             1, 2, 3, 4, 5, 6,
             1.1, 2.1, 3.1, 4.1, 5.1, 6.1
)

# Convert to data frame
lam <- as.data.frame(lam)

for (i in 2:5){
  lam[,i] <- as.numeric(as.character(lam[,i]))
}

lam$Population <- factor(lam$Population, levels = c("Total", "Uninfected", "Infected"))

str(lam)
lam$Class <- factor(lam$Population, levels = c("Total", "Uninfected", "Infected"))

abund <- ggplot(data = lam, aes(x = Year, y = Mean, col = Class, ymin = ylo, ymax = yhi)) + 
  geom_point()+
  geom_line(aes(x = Year, y = Mean))+
  scale_colour_manual(values = c("black", "gray40", "gray60"))+
  geom_pointrange(position = position_dodge(width=0.5), size = 0.5)+
  scale_x_continuous("", breaks=c(1:6))+
  xlab('') +
  ylab('Amphibian abundance')+  
  theme_bw() + 
  ylim(c(0, 425))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        legend.title = element_text(size=9),
        legend.text = element_text(size=9),
        legend.position = c(0.8, 0.1),
        legend.direction = "horizontal")+
  annotate("text", x = 0.5, y = 400, label = "A", size = 8)
  #geom_ribbon(data=lam,aes(ymin=ylo,ymax=yhi),alpha=0.3)


#------#------#------#------#
#------ Prevalence ----------#
#------#------#------#------#


lam <- matrix(NA, nrow = 6, ncol = 4)

colnames(lam) <- c( "Year", "Mean", "ylo", "yhi")

lam[1:6, 2] <- colMeans(NI/NT)
lam[1:6, 3] <- HPDinterval(as.mcmc(NI/NT))[,1]
lam[1:6, 4] <- HPDinterval(as.mcmc(NI/NT))[,2]

lam[,1] <- 1:6

lam <- as.data.frame(lam)

for (i in 2:4){
  lam[,i] <- as.numeric(as.character(lam[,i]))
}

library(ggplot2)

prev <- ggplot(data = lam, aes(x = Year, y = Mean, ymin = ylo, ymax = yhi)) + 
  geom_pointrange(position = position_dodge(width=0.5), size = 0.5)+
  geom_point()+
  scale_x_continuous("", breaks=c(1:6))+
  geom_line(aes(x = Year, y = Mean))+
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



#------#------#------#------#
#------ Infection intensity ----------#
#------#------#------#------#

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

samp <- ddply(.data = Cop, .variables = c("season_yr"), .fun = summarize,
              sample_size = length((LargestZooValue)))

Infection_intensity <- ggplot(data = Cop, aes(x = season_yr, y = (LargestZooValue+0.8))) + 
  geom_jitter(width = 0.4)+
  scale_x_discrete("Season-year",
                   labels=c("Wet10", "Wet11",
                            "Wet12", "Dry13", "Wet13", "Dry14"))+
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000), limits = c(0.4, 30500))+
  ylab(expression(paste("Mean ", italic(Bd), " infection intensity")))+  
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "none")+
  annotate("text", x = 0.7, y = 25000, label = "C", size = 8)+
  annotate("text", x = 1, y = 0.5, label = paste("n=", samp$sample_size[1], sep = ""), size = 4)+
  annotate("text", x = 2, y = 0.5, label = paste("n=", samp$sample_size[2], sep = ""), size = 4)+
  annotate("text", x = 3, y = 0.5, label = paste("n=", samp$sample_size[3], sep = ""), size = 4)+
  annotate("text", x = 4, y = 0.5, label = paste("n=", samp$sample_size[4], sep = ""), size = 4)+
  annotate("text", x = 5, y = 0.5, label = paste("n=", samp$sample_size[5], sep = ""), size = 4)+
  annotate("text", x = 6, y = 0.5, label = paste("n=", samp$sample_size[6], sep = ""), size = 4)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.5, col = "red")


#---- Combine figures
plot_grid(abund, prev, Infection_intensity, align = "v", nrow = 3)
