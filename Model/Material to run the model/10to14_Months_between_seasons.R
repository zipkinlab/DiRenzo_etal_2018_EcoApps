#---- Import data

days <- read.csv(file = "/Users/Cici/Dropbox/PhD_cope/Chpt2/Data/Formated_sites/Dail_Madsen_Model_data/ALL_FROGS_days.csv")[,-1]


#---------- 

days2 <- matrix(NA, nrow = 99, ncol = 3)

#---------
habitat <- as.factor(c(rep("stream", times = 10), 
                       rep("stream", times = 10),
                       rep("stream", times = 10),
                       rep("trail", times = 19),
                       rep("trail", times = 20),
                       rep("stream", times = 10),
                       rep("trail", times = 20)))

stream <- ifelse(habitat == "stream", 1, 0)
trail <- ifelse(habitat == "trail", 1, 0)

habitat2 <- ifelse(habitat == "stream", 1, 2)

tran <- as.numeric(c(rep(1, times = 10), 
                       rep(2, times = 10),
                       rep(3, times = 10),
                       rep(4, times = 19),
                       rep(5, times = 20),
                       rep(6, times = 10),
                       rep(7, times = 20)))


#--------- Cascada
days2[1:10, 1] <- days[1:10, 8]
days2[1:10, 2] <- days[1:10, 15]
days2[1:10, 3] <- days[1:10, 22]
  # 10

#------- Guabal
days2[11:20, 1] <- days[21:30, 8]
days2[11:20, 2] <- days[21:30, 15]
days2[11:20, 3] <- days[21:30, 22]
  # 10

#------- Loop Stream
days2[21:30, 1] <- days[41:50, 7]
days2[21:30, 2] <- days[41:50, 13]
days2[21:30, 3] <- days[41:50, 20]
  # 10

#------- Loop Trail
days2[31:49, 1] <- days[61:79, 7]
days2[31:49, 2] <- days[61:79, 14]
days2[31:49, 3] <- days[61:79, 20]
  # 19

#------- Main Trail
days2[50:69, 1] <- days[100:119, 7]
days2[50:69, 2] <- days[100:119, 14]
days2[50:69, 3] <- days[100:119, 20]
  # 20

#------- Silenciosa
days2[70:79, 1] <- days[140:149, 6]
days2[70:79, 2] <- days[140:149, 13]
days2[70:79, 3] <- days[140:149, 20]
  # 10

#------- Verrugosa
days2[80:99, 1] <- days[160:179, 7]
days2[80:99, 2] <- days[160:179, 14]
days2[80:99, 3] <- days[160:179, 19]
  # 20

days3 <- round(days2/30)

days3 <- cbind(
               rep(12, times = nrow(days3)), 
               rep(11, times = nrow(days3)),
               days3)
