W10I <- read.csv(file = "/Users/Cici/Dropbox/MSU Post Doc/Dail-Madsen/Chpt 2/Eco Apps/Data/IN_WET_2010.csv")[,-1]
W10 <- read.csv(file = "/Users/Cici/Dropbox/MSU Post Doc/Dail-Madsen/Chpt 2/Eco Apps/Data/NOT_WET_2010.csv")[,-1]
W10U <- read.csv(file = "/Users/Cici/Dropbox/MSU Post Doc/Dail-Madsen/Chpt 2/Eco Apps/Data/NAN_WET_2010.csv")[,-1]
# ------ Bundle data
# Initial host values
R <- nrow(W10)
T <- 6
K <- 3

#------------ NA hosts

dim(W10I)
dim(W10)
dim(W10U)

host <- array(NA, dim = c(R, T, K))

host[,,1] <- as.matrix(W10)
host[,,2] <- as.matrix(W10I)
host[,,3] <- as.matrix(W10U)

host1 <- apply(host, c(1,3), max, na.rm = TRUE)
host1[host1 == "-Inf"] <- NA

host2 <- unlist(c(host1))

hist(host2)

mean.host <- mean(host2, na.rm = TRUE)
var.host <- var(host2, na.rm = TRUE)

mean(host2, na.rm = TRUE)/var(host2, na.rm = TRUE)


hist(host2, las = 1, main = "", xlab = "Number of amphibians per 20 m site (Season 1)")
text(x = 2.75, y = 200, labels = paste("Mean = ", round(mean.host, dig = 2), sep = ""))
text(x = 2.75, y = 185, labels = paste("Variance = ", round(var.host, dig = 2), sep = ""))
text(x = 2.70, y = 160, labels = paste("Mean/Variance = ", round(round(mean.host, dig = 2)/round(var.host, dig = 2), dig = 2), sep = ""))

