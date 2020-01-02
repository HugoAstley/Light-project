#Dropping superfluous columns (not related)
drops <- c("X","X.1")
data2 <- data2[ , !(names(data2) %in% drops)]

##How many diagnoses per person
#Replaces columns with TRUE/ FALSE 1/0
data2.binary <- data2

for(i in 1:7){
  data2.binary[,i+3] <- data2[,i+3] !="NA"
  }

#Find rowwise count values of the number of true and false values
data2$DIAGNOSn <- rowSums(data2.binary[,4:10], na.rm = T)





##How many diagnoses per person
#Replaces columns with TRUE/ FALSE 1/0
data2.binary <- data2

for(i in 1:4){
  data2.binary[,i+24] <- data2[,i+24] !=""
}

#Find rowwise count values of the number of true and false values
data2$PROCEDUn <- rowSums(data2.binary[,25:28], na.rm = T)