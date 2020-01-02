###How many diagnoses

#data1
diagnoses.num.data1 <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of diagnoses

for(i in 1:7){
  diagnoses.num.data1[i,1] <- i
  diagnoses.num.data1[i,2] <- sum((data1[,i+17] !=""))
}

diagnoses.num.data1 <- as.data.frame(diagnoses.num.data1)

#data2
diagnoses.num.data2 <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of diagnoses


for(i in 1:7){
  diagnoses.num.data2[i,1] <- i
  diagnoses.num.data2[i,2] <- sum((data2[,i+5] !="NA"))
}

diagnoses.num.data2 <- as.data.frame(diagnoses.num.data2)



#psychiatric diagnoses
diagnoses.num.psychiatric_diagnoses <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of diagnoses

for(i in 1:7){
  diagnoses.num.psychiatric_diagnoses[i,1] <- i
  diagnoses.num.psychiatric_diagnoses[i,2] <- sum((psychiatric_diagnoses[,i+5] !=""))
}

diagnoses.num.psychiatric_diagnoses <- as.data.frame(diagnoses.num.psychiatric_diagnoses)

#without psychiatric diagnoses
diagnoses.num.data2.minus.psychiatric_diagnoses <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of diagnoses

for(i in 1:7){
  diagnoses.num.data2.minus.psychiatric_diagnoses[i,1] <- i
  diagnoses.num.data2.minus.psychiatric_diagnoses[i,2] <- sum((data2.minus.psychiatric_diagnoses[,i+5] !=""))
}


diagnoses.num.data2.minus.psychiatric_diagnoses <- as.data.frame(diagnoses.num.data2.minus.psychiatric_diagnoses)





##Graph that

# Plot
plot.diagnoses.num.data1 <- ggplot(diagnoses.num.data1, aes(x=V1, y=((V2/sum(V2)*100)))) +
  geom_line(color="skyblue", alpha=0.9)+
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  coord_cartesian(ylim = c(0, 25))+ 
  theme(
    axis.line = element_line(color = "black", 
                             size = 0.7, linetype = "solid"),
    panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid',
                                                                        colour = "Grey"))+
  xlab("Number of diagnoses") +
  ylab("Frequency (%)")
  



# Plot
plot.diagnoses.num.data2 <- ggplot(diagnoses.num.data2, aes(x=V1, y=((V2/sum(V2)*100)))) +
  geom_line(color="skyblue", alpha=0.9)+
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  coord_cartesian(ylim = c(0, 25))+ 
  theme(
    axis.line = element_line(color = "black", 
                             size = 0.7, linetype = "solid"),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Number of diagnoses") +
  ylab("Frequency (%)")



# Plot
plot.diagnoses.num.psychiatric_diagnoses <- ggplot(diagnoses.num.psychiatric_diagnoses, aes(x=V1, y=((V2/sum(V2)*100)))) +
  geom_line(color="skyblue", alpha=0.9)+
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  coord_cartesian(ylim = c(0, 25))+ 
  theme(
    axis.line = element_line(color = "black", 
                             size = 0.7, linetype = "solid"),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Number of diagnoses") +
  ylab("Frequency (%)")




# Plot
plot.diagnoses.num.data2.minus.psychiatric_diagnoses <- ggplot(diagnoses.num.data2.minus.psychiatric_diagnoses, aes(x=V1, y=((V2/sum(V2)*100)))) +
  geom_line(color="skyblue", alpha=0.9)+
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  coord_cartesian(ylim = c(0, 25))+ 
  theme(
    axis.line = element_line(color = "black", 
                             size = 0.7, linetype = "solid"),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Number of diagnoses") +
  ylab("Frequency (%)")








grid.arrange(plot.diagnoses.num.data1, plot.diagnoses.num.data2, plot.diagnoses.num.psychiatric_diagnoses, plot.diagnoses.num.data2.minus.psychiatric_diagnoses,
             plot.procedures.num.data1, plot.procedures.num.data2, plot.procedures.num.psychiatric_diagnoses, plot.procedures.num.data2.minus.psychiatric_diagnoses,
             ncol=4)

























##How many procedures
#data1
procedures.num.data1 <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of procedures

for(i in 1:7){
  procedures.num.data1[i,1] <- i
  procedures.num.data1[i,2] <- sum((data1[,i+24] !=""))
}

procedures.num.data1 <- as.data.frame(procedures.num.data1)


#data2
procedures.num.data2 <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of procedures


for(i in 1:7){
  procedures.num.data2[i,1] <- i
  procedures.num.data2[i,2] <- sum((data2[,i+24] !=""))
}



procedures.num.data2 <- as.data.frame(procedures.num.data2)


#psychiatric procedures
procedures.num.psychiatric_diagnoses <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of procedures

for(i in 1:7){
  procedures.num.psychiatric_diagnoses[i,1] <- i
  procedures.num.psychiatric_diagnoses[i,2] <- sum((psychiatric_diagnoses[,i+24] !=""))
}


procedures.num.psychiatric_diagnoses <- as.data.frame(procedures.num.psychiatric_diagnoses)

#without psychiatric procedures
procedures.num.data2.minus.psychiatric_diagnoses <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of procedures

for(i in 1:7){
  procedures.num.data2.minus.psychiatric_diagnoses[i,1] <- i
  procedures.num.data2.minus.psychiatric_diagnoses[i,2] <- sum((data2.minus.psychiatric_diagnoses[,i+24] !=""))
}


procedures.num.data2.minus.psychiatric_diagnoses <- as.data.frame(procedures.num.data2.minus.psychiatric_diagnoses)





##Graph that

# Plot
plot.procedures.num.data1 <- ggplot(procedures.num.data1, aes(x=V1, y=((V2/sum(V2)*100)))) +
  geom_line(color="skyblue", alpha=0.9)+
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  coord_cartesian(ylim = c(0, 25))+ 
  theme(
    axis.line = element_line(color = "black", 
                             size = 0.7, linetype = "solid"),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Number of procedures") +
  ylab("Frequency (%)")




# Plot
plot.procedures.num.data2 <- ggplot(procedures.num.data2, aes(x=V1, y=((V2/sum(V2)*100)))) +
  geom_line(color="skyblue", alpha=0.9)+
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  coord_cartesian(ylim = c(0, 25))+ 
  theme(
    axis.line = element_line(color = "black", 
                             size = 0.7, linetype = "solid"),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Number of procedures") +
  ylab("Frequency (%)")



# Plot
plot.procedures.num.psychiatric_diagnoses <- ggplot(procedures.num.psychiatric_diagnoses, aes(x=V1, y=((V2/sum(V2)*100)))) +
  geom_line(color="skyblue", alpha=0.9)+
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  coord_cartesian(ylim = c(0, 25))+ 
  theme(
    axis.line = element_line(color = "black", 
                             size = 0.7, linetype = "solid"),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Number of procedures") +
  ylab("Frequency (%)")




# Plot
plot.procedures.num.data2.minus.psychiatric_diagnoses <- ggplot(procedures.num.data2.minus.psychiatric_diagnoses, aes(x=V1, y=((V2/sum(V2)*100)))) +
  geom_line(color="skyblue", alpha=0.9)+
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  coord_cartesian(ylim = c(0, 25))+ 
  theme(
    axis.line = element_line(color = "black", 
                             size = 0.7, linetype = "solid"),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Number of procedures") +
  ylab("Frequency (%)")













































##############################

##How many psychiatric diagnoses!




n <-data2
#make a table of ones and zeros
n$DIAGNOS1 <- ifelse(n$DIAGNOS1 %in% c(290:319), 1, 0)
n$DIAGNOS2 <- ifelse(n$DIAGNOS2 %in% c(290:319), 1, 0)
n$DIAGNOS3 <- ifelse(n$DIAGNOS3 %in% c(290:319), 1, 0)
n$DIAGNOS4 <- ifelse(n$DIAGNOS4 %in% c(290:319), 1, 0)
n$DIAGNOS5 <- ifelse(n$DIAGNOS5 %in% c(290:319), 1, 0)
n$DIAGNOS6 <- ifelse(n$DIAGNOS6 %in% c(290:319), 1, 0)
n$DIAGNOS7 <- ifelse(n$DIAGNOS7 %in% c(290:319), 1, 0)


n



#count the 1's in each row


n %>%
  mutate(sum = rowSums(.[6:12]))

p <- rowSums(n[,6:12], na.rm = TRUE)
p

length(which(p>0))



n <- rowSums((DIAGNOS1 > 289 & DIAGNOS1 <320)| 
           (DIAGNOS2 > 289 & DIAGNOS2 <320)| 
           (DIAGNOS3 > 289 & DIAGNOS3 <320)|
           (DIAGNOS4 > 289 & DIAGNOS4 <320)|
           (DIAGNOS5 > 289 & DIAGNOS5 <320)|
           (DIAGNOS6 > 289 & DIAGNOS6 <320)|
           (DIAGNOS7 > 289 & DIAGNOS7 <320))



data2$DIAGNOS1 <- as.numeric(data2$DIAGNOS1)
data2$DIAGNOS2 <- as.numeric(data2$DIAGNOS2)
data2$DIAGNOS3 <- as.numeric(data2$DIAGNOS3)
data2$DIAGNOS4 <- as.numeric(data2$DIAGNOS4)
data2$DIAGNOS5 <- as.numeric(data2$DIAGNOS5)
data2$DIAGNOS6 <- as.numeric(data2$DIAGNOS6)
data2$DIAGNOS7 <- as.numeric(data2$DIAGNOS7)





#data1
diagnoses.num.data1 <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of diagnoses

for(i in 1:7){
  diagnoses.num.data1[i,1] <- i
  diagnoses.num.data1[i,2] <- sum((data1[,i+17] !=""))
}

#data2
diagnoses.num.data2 <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of diagnoses


for(i in 1:7){
  diagnoses.num.data2[i,1] <- i
  diagnoses.num.data2[i,2] <- sum((data2[,i+17] !=""))
}

#psychiatric diagnoses
diagnoses.num.psychiatric_diagnoses <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of diagnoses

for(i in 1:7){
  diagnoses.num.psychiatric_diagnoses[i,1] <- i
  diagnoses.num.psychiatric_diagnoses[i,2] <- sum((psychiatric_diagnoses[,i+17] !=""))
}

#without psychiatric diagnoses
diagnoses.num.data2.minus.psychiatric_diagnoses <- matrix(ncol=2, nrow=7) #make a place to store the number of people with each number of diagnoses

for(i in 1:7){
  diagnoses.num.data2.minus.psychiatric_diagnoses[i,1] <- i
  diagnoses.num.data2.minus.psychiatric_diagnoses[i,2] <- sum((data2.minus.psychiatric_diagnoses[,i+17] !=""))
}



#####nope#####

n <- rowSums(data1[,19] !="")

n <- rowSums(data2[,c(6:12)] >289 & data2[,c(6:12)] <320)

n <- rowSums(data2[,c(6:12)] == 290:319)



length(which(n >0))
n




n$DIAGNOS1 <- as.numeric(n$DIAGNOS1)

n$DIAGNOS1 <- n$DIAGNOS1 %>%
  replace(n[n$DIAGNOS1 >289 & n$DIAGNOS1<320,], 1) 

n[n$DIAGNOS1 >289 & n$DIAGNOS1<320,]


n[n$DIAGNOS1 >289 & n$DIAGNOS1<320,]