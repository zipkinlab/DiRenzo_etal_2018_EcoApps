load("/Users/Cici/GitHub/2010to2014ElCope/FINAL D-M model/Model_output/years2010to2014_prior001_001.rda")

load("/Users/Cici/GitHub/2010to2014ElCope/FINAL D-M model/Model_output/years2010to2014_prior001_01.rda")

load("/Users/Cici/GitHub/2010to2014ElCope/FINAL D-M model/Model_output/years2010to2014_prior01.rda")

# Load library
library(coda)

# Stack chains
chains2 <- rbind(as.matrix(out2$samples[[1]]), 
                         as.matrix(out2$samples[[2]]), 
                         as.matrix(out2$samples[[3]]))

chains3 <- rbind(as.matrix(out3$samples[[1]]), 
                        as.matrix(out3$samples[[2]]), 
                        as.matrix(out3$samples[[3]]))

chains4 <- rbind(as.matrix(out4$samples[[1]]), 
                         as.matrix(out4$samples[[2]]), 
                         as.matrix(out4$samples[[3]]))

chains2 <- as.mcmc(chains2)
chains3 <- as.mcmc(chains3)
chains4 <- as.mcmc(chains4)

#-----------
O2 <- cbind(colMeans(chains2), HPDinterval(chains2, prob = 0.95))
O3 <- cbind(colMeans(chains3), HPDinterval(chains3, prob = 0.95))
O4 <- cbind(colMeans(chains4), HPDinterval(chains4, prob = 0.95))
O2 <- as.data.frame(O2)
O3 <- as.data.frame(O3)
O4 <- as.data.frame(O4)

#-----------
O2$prior_gamma <- rep("0.01", times = nrow(O2))
O3$prior_gamma <- rep("0.001", times = nrow(O3))
O4$prior_gamma <- rep("0.001", times = nrow(O4))

O2$prior_pop <- rep("0.01", times = nrow(O2))
O3$prior_pop <- rep("0.01", times = nrow(O3))
O4$prior_pop <- rep("0.001", times = nrow(O4))

O2$prior <- rep("0.01, 0.01", times = nrow(O2))
O3$prior <- rep("0.001, 0.01", times = nrow(O3))
O4$prior <- rep("0.001, 0.001", times = nrow(O4))

tog <- rbind(O2, O3, O4)

tog$parameters <- rep(rownames(O2), times = 3)
colnames(tog)[1] <- "Mean"

str(tog)

tog2 <- tog[c(1:17, 61:77, 121:137),]
tog2$params <- as.factor(tog2$parameters)

levels(tog2$params) <- c(
  expression(phi[2]),
  expression(italic("r")),
  expression(phi[1]),
  expression(paste(alpha, "0", sep ="")),
  expression(lambda[2]),
  expression(lambda[1]),
  expression(paste(theta[2], "0", sep ="")),
  expression(paste(theta[2], "1", sep ="")),
  expression(paste(theta[1], "0", sep ="")),
  expression(paste(theta[1], "1", sep ="")),
  expression(paste(alpha, "1", sep ="")),
  expression(paste(beta, "1", sep ="")),
  expression(paste(theta, "2", sep ="")),
  expression(paste(theta, "3", sep ="")), 
    expression(paste(beta[1], "0", sep ="")),
    expression(paste(beta[2], "0", sep ="")),
    expression(italic("q"))
    )

#-----------
library(ggplot2)
ggplot(tog2, aes(x= as.factor(params), y=Mean, ymin=lower, ymax= upper, col = prior))+ 
  geom_linerange(size = 1,  position=position_dodge(width=c(0.7))) +
  geom_point(size = 3, aes(x = as.factor(params), y = Mean),  position=position_dodge(width=c(0.7))) +
  coord_flip() + ylab('Parameter estimates') +
  xlab("Parameter names") +
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 17, color = "black"), 
        axis.text.y = element_text(size = 17, color = "black"), 
        axis.title.y = element_text(size = 17, color = "black"), 
        axis.title.x =element_text(size = 17, color = "black"),
        legend.title =element_text(size = 17, color = "black"),
        legend.text =element_text(size = 17, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
