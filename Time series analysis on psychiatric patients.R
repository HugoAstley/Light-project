library(plotrix)
library(SDMTools)
library(TTR)


psychiatric_diagnoses_ts <- psychiatric_diagnoses

psychiatric_diagnoses_ts <- psychiatric_diagnoses_ts %>%
  dplyr::select(DOC, Date, WEIGHT)



#Make it date again
psychiatric_diagnoses_ts$Date <- as.Date(psychiatric_diagnoses_ts$Date)

#Cut down to month and year because they are the only columns changing
psychiatric_diagnoses_ts$Date <- substr((psychiatric_diagnoses_ts$Date), 1,7)

#Make it a tibble
psychiatric_diagnoses_ts <- as.data.frame(psychiatric_diagnoses_ts)





#Aggregate into weighted means for each month

psychiatric_diagnoses_ts_means <- ddply(psychiatric_diagnoses_ts, .(Date), # invoke following function by date
                                  function(x) data.frame(DOC=weighted.mean(x$DOC, x$WEIGHT)))


#Also find the weighted standard deviations of these means


psychiatric_diagnoses_ts_std.error.weighted <- ddply(psychiatric_diagnoses_ts, .(Date), # invoke following function by date
                                                     function(x) data.frame(DOC=wt.sd(x$DOC, x$WEIGHT))/sqrt(nrow(x)))


#Find a moving average of the mean
psychiatric_diagnosesSMA <- SMA(psychiatric_diagnoses_ts_means$DOC, n=12)
psychiatric_diagnoses_ts_means <-cbind(psychiatric_diagnoses_ts_means, psychiatric_diagnosesSMA) #column combine it with the means
#if adjusting do this instead
psychiatric_diagnoses_ts_means$psychiatric_diagnosesSMA <- psychiatric_diagnosesSMA

####Time series analysis############Anomaly detection pubr##########


#Specify the different datasets
#Points from the psychiatric one
#Line from the means
#Ribbon from the standard deviation
plot_time_series_psychiatric_and_nonpsychiatric <-ggplot(psychiatric_diagnoses_ts_means, aes(x=Date, y=DOC))+
  scale_y_continuous(name= "Mean length of stay (Days)", breaks= c(0, 2,4, 6, 8, 10, 12))+
  coord_cartesian(ylim= c(3,11))+
  scale_x_discrete(name= "Date",
                   breaks=c("1990-01","1994-01", "1998-01", "2002-01","2006-01"),
                   labels=c("1990", "1994", "1998","2002", "2006"))+
  geom_line(data=psychiatric_diagnoses_ts_means, group=1)+
  geom_line(data=psychiatric_diagnoses_ts_means, aes(x=Date, y= psychiatric_diagnosesSMA), group=3)+
  geom_ribbon(data= psychiatric_diagnoses_ts_std.error.weighted, aes(x=1:length(Date),ymin= (psychiatric_diagnoses_ts_means$DOC-psychiatric_diagnoses_ts_std.error.weighted$DOC),
                                                                     ymax= (psychiatric_diagnoses_ts_means$DOC+psychiatric_diagnoses_ts_std.error.weighted$DOC)), alpha=0.3)+
  geom_line(data=data2.minus.psychiatric_diagnoses_ts_means, group=2)+
  geom_line(data=data2.minus.psychiatric_diagnoses_ts_means, aes(x=Date, y= data2.minus.psychiatric_diagnosesSMA), group=4)+
  geom_ribbon(data= data2.minus.psychiatric_diagnoses_ts_std.error.weighted, aes(x=1:length(Date),ymin= (data2.minus.psychiatric_diagnoses_ts_means$DOC-data2.minus.psychiatric_diagnoses_ts_std.error.weighted$DOC),
                                                                     ymax= (data2.minus.psychiatric_diagnoses_ts_means$DOC+data2.minus.psychiatric_diagnoses_ts_std.error.weighted$DOC)), alpha=0.3)+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "grey", size = 0.5))+
  geom_point(data= psychiatric_diagnoses[sample(nrow(psychiatric_diagnoses), 10000), ], aes(x=substr(Date, start = 1, stop = 7), y=DOC),colour= "skyblue", alpha=0.2)


plot_time_series_psychiatric_and_nonpsychiatric


#Save the plot
ggsave(plot_time_series_psychiatric_and_nonpsychiatric, filename="time series psychiatric vs nonpsychiatric.pdf")


#In case you want to plot the nonpsychiatric points too
  geom_point(data= data2.minus.psychiatric_diagnoses[sample(nrow(data2.minus.psychiatric_diagnoses), 10000), ], aes(x=substr(Date, start = 1, stop = 7), y=DOC), colour= "blue", shape=2, alpha=0.3)


psychiatric_diagnoses$Date
psychiatric_diagnoses_ts_means$Date






####take away the trend####
##Additive
detrend_psychiatric_diagnoses_ts_additive <- psychiatric_diagnoses_ts_means
detrend_psychiatric_diagnoses_ts_additive$DOC <- psychiatric_diagnoses_ts_means$DOC-psychiatric_diagnosesSMA


##Multiplicative
detrend_psychiatric_diagnoses_ts_multiplicative <- psychiatric_diagnoses_ts_means
detrend_psychiatric_diagnoses_ts_multiplicative$DOC <- psychiatric_diagnoses_ts_means$DOC/psychiatric_diagnosesSMA



####Finding a mean for each month####
##Additive
detrend_psychiatric_diagnoses_ts_additive_m = t(matrix(data =detrend_psychiatric_diagnoses_ts_additive$DOC, nrow = 12))#transpose into rows of 12
detrend_psychiatric_diagnoses_ts_additive_means = colMeans(detrend_psychiatric_diagnoses_ts_additive_m, na.rm = T) #calculate means down the columns

plot(as.ts(rep(detrend_psychiatric_diagnoses_ts_additive_means,1)))

##Multiplicative
detrend_psychiatric_diagnoses_ts_multiplicative_m = t(matrix(data =detrend_psychiatric_diagnoses_ts_multiplicative$DOC, nrow = 12))
detrend_psychiatric_diagnoses_ts_multiplicative_means = colMeans(detrend_psychiatric_diagnoses_ts_multiplicative_m, na.rm = T)

plot(as.ts(rep(detrend_psychiatric_diagnoses_ts_multiplicative_means,1)))


##Plot
#Create the matrix so you have two dimentions
detrend_plot_psychiatric <- cbind(matrix(1:12, nrow = 12),detrend_psychiatric_diagnoses_ts_multiplicative_means)
detrend_plot_psychiatric <- as.data.frame(detrend_plot_psychiatric)

ggplot(detrend_plot_psychiatric, aes(x= detrend_plot_psychiatric$V1, y= detrend_plot_psychiatric$detrend_psychiatric_diagnoses_ts_multiplicative_means))+
  scale_x_continuous(name= "Month", breaks= seq(0,12,1))+
  scale_y_continuous(name= "Coefficient")+
  geom_line()+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "grey", size = 0.5))



####Residuals (all approximate)####
##Additive
random_psychiatric_diagnoses_ts_additive = detrend_psychiatric_diagnoses_ts_additive$DOC - detrend_psychiatric_diagnoses_ts_additive_means
plot(as.ts(random_psychiatric_diagnoses_ts_additive))

##Multiplicative
random_psychiatric_diagnoses_ts_multiplicative = detrend_psychiatric_diagnoses_ts_multiplicative$DOC / detrend_psychiatric_diagnoses_ts_multiplicative_means




















###########In case####
psychiatric_diagnoses_ts <- aggregate(psychiatric_diagnoses_ts, by = list(psychiatric_diagnoses_ts$Date),
FUN = weighted.mean)


#Cut out the column that gets corrupted
psychiatric_diagnoses_ts <- psychiatric_diagnoses_ts[,(1:2)]
#Remane the group 1 column
colnames(psychiatric_diagnoses_ts)[1] <- "Date"


Time_series_plot_psychiatric_diagnoses <- ggplot(psychiatric_diagnoses_ts, aes(x=(ADM_MON), y= (Compare_means_allregions$DOC_mean_psych)))+
  scale_x_continuous("Month", breaks= seq(0,12,1))+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  geom_point(alpha=1)+
  geom_smooth(aes(colour= "Psychiatric"))+
  geom_point(aes(y= DOC_mean_all_minus_psychiatric))+
  geom_smooth(aes(y= DOC_mean_all_minus_psychiatric, colour= "All-Psychiatric"))+
  geom_point(aes(y= DOC_mean_all))+
  geom_smooth(aes(y= DOC_mean_all, colour= "All"))+
  xlab("Month")+
  ylab("Average length of stay (in days)")+
  scale_colour_manual(values= c("black", "grey70","royalblue1" ))+
  coord_cartesian(ylim= 0:7)

#plot the ts
Time_series_plot_psychiatric_diagnoses <- ggplot(psychiatric_diagnoses_ts, aes(x=(Date), y= (DOC), group=1))+
  geom_line()

Time_series_plot_psychiatric_diagnoses

Time_series_plot_psychiatric_diagnoses <- ggplot(psychiatric_diagnoses_ts, aes(x=(Date), y= (psychiatric_diagnoses_ts$DOC)))+
  scale_x_continuous("Month", breaks= seq(0,12,1))+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  geom_point(alpha=1)+
  geom_smooth(aes(colour= "Psychiatric"))+
  geom_point(aes(y= DOC_mean_all_minus_psychiatric))+
  geom_smooth(aes(y= DOC_mean_all_minus_psychiatric, colour= "All-Psychiatric"))+
  geom_point(aes(y= DOC_mean_all))+
  geom_smooth(aes(y= DOC_mean_all, colour= "All"))+
  xlab("Month")+
  ylab("Average length of stay (in days)")+
  scale_colour_manual(values= c("black", "grey70","royalblue1" ))+
  coord_cartesian(ylim= 0:7)


psychiatric_diagnoses_ts_std.error.weighted <- ddply(psychiatric_diagnoses_ts, .(Date), # invoke following function by date
                                                     function(x) (data.frame(DOC=wt.sd(x$DOC, x$WEIGHT))/sqrt(length(x))))
