#Population level estimates


#Create a plot of total incidence (y) against the admission month (x)



aggregate(data2,
          by = data2$ADM_MON,
          FUN = sum(data2$WEIGHT))





ddply(data2, .(ADM_MON), # invoke the following function by admission month (where x is the [admission month] inside [data2])
                                                    function(x) sum(x$WEIGHT))
                                                    
                                                    
#Create a plot of the total number of days of care (y) against the admission month (x)


total_DOC_weighted_permonth <- ddply(data2, .(SVYEAR, ADM_MON), # invoke the following function by admission month
           function(x) sum(x$WEIGHT*x$DOC))

mean_DOC_weighted_permonth <- ddply(total_DOC_weighted_permonth, .(ADM_MON), # invoke the following function by admission month
           function(x) mean(x$V1))



ggplot(total_DOC_weighted_permonth, aes(x= factor(ADM_MON), y=as.numeric(V1)))+
  scale_y_continuous(name= "Length of stay (Millions of Days)",
                     breaks = c(7000000,8000000, 9000000, 10000000, 11000000,12000000,13000000,14000000,15000000),
                     labels = c("7", "8", "9", "10", "11", "12", "13","14", "15"))+
  geom_bar(data=mean_DOC_weighted_permonth, aes(y= mean_DOC_weighted_permonth$V1, x=mean_DOC_weighted_permonth$ADM_MON), stat= "identity", fill = "skyblue")+
  geom_point(data=total_DOC_weighted_permonth, aes(x=factor(ADM_MON), y=as.numeric(V1)))+
  geom_smooth(aes(y= total_DOC_weighted_permonth$V1,x= ADM_MON))+
  coord_cartesian(ylim= c(7000000,15000000))+
  scale_x_discrete(name= "Admission Month",
                   labels= c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "grey", size = 0.5))
  


   
, shape=2, colour= "skyblue"
breaks= c("1","2", "3", "4","5", "6", "7","8","9","10","11","12")
, colour= factor(SVYEAR)