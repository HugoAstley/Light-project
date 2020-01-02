library(SDMTools)


##Give date to nonpsychiatric group (don't know why they don't have it already)
#Make month have at two digits throughout
data2.minus.psychiatric_diagnoses$ADM_MON <- sprintf("%02d", as.numeric(data2.minus.psychiatric_diagnoses$ADM_MON))

#Put them in one dataframe
data2.minus.psychiatric_diagnoses$Date <- paste0("01", data2.minus.psychiatric_diagnoses$ADM_MON, data2.minus.psychiatric_diagnoses$SVYEAR)

#Put it in a date format
data2.minus.psychiatric_diagnoses$Date <- dmy(data2.minus.psychiatric_diagnoses$Date)




data2.minus.psychiatric_diagnoses_ts <- data2.minus.psychiatric_diagnoses

data2.minus.psychiatric_diagnoses_ts <- data2.minus.psychiatric_diagnoses_ts %>%
  dplyr::select(DOC, Date, WEIGHT)



#Make it date again
data2.minus.psychiatric_diagnoses_ts$Date <- as.Date(data2.minus.psychiatric_diagnoses_ts$Date)

#Cut down to month and year because they are the only columns changing
data2.minus.psychiatric_diagnoses_ts$Date <- substr((data2.minus.psychiatric_diagnoses_ts$Date), 1,7)

#Make it a dataframe
data2.minus.psychiatric_diagnoses_ts <- as.data.frame(data2.minus.psychiatric_diagnoses_ts)





#Aggregate into weighted means for each month
data2.minus.psychiatric_diagnoses_ts_means <- ddply(data2.minus.psychiatric_diagnoses_ts, .(Date), # invoke following function by date
                                  function(x) data.frame(DOC=weighted.mean(x$DOC, x$WEIGHT)))

#Also find the weighted standard deviations of these means
data2.minus.psychiatric_diagnoses_ts_std.error.weighted <- ddply(data2.minus.psychiatric_diagnoses_ts, .(Date), # invoke following function by date
                                                     function(x) data.frame(DOC=wt.sd(x$DOC, x$WEIGHT))/sqrt(nrow(x)))

#Find a moving average of the mean
data2.minus.psychiatric_diagnosesSMA <- SMA(data2.minus.psychiatric_diagnoses_ts_means$DOC, n=12)
data2.minus.psychiatric_diagnoses_ts_means <-cbind(data2.minus.psychiatric_diagnoses_ts_means, data2.minus.psychiatric_diagnosesSMA) #column combine it with the means
#if adjusting do this instead
data2.minus.psychiatric_diagnoses_ts_means$psychiatric_diagnosesSMA <- data2.minus.psychiatric_diagnosesSMA
















####take away the trend####
##Additive
detrend_data2.minus.psychiatric_diagnoses_ts_additive <- data2.minus.psychiatric_diagnoses_ts_means
detrend_data2.minus.psychiatric_diagnoses_ts_additive$DOC <- data2.minus.psychiatric_diagnoses_ts_means$DOC-data2.minus.psychiatric_diagnosesSMA


##Multiplicative
detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative <- data2.minus.psychiatric_diagnoses_ts_means
detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative$DOC <- data2.minus.psychiatric_diagnoses_ts_means$DOC/data2.minus.psychiatric_diagnosesSMA



####Finding a mean for each month####
##Additive
detrend_data2.minus.psychiatric_diagnoses_ts_additive_m = t(matrix(data =detrend_data2.minus.psychiatric_diagnoses_ts_additive$DOC, nrow = 12))#transpose into rows of 12
detrend_data2.minus.psychiatric_diagnoses_ts_additive_means = colMeans(detrend_data2.minus.psychiatric_diagnoses_ts_additive_m, na.rm = T) #calculate means down the columns

plot(as.ts(rep(detrend_data2.minus.psychiatric_diagnoses_ts_additive_means,1)))

##Multiplicative
detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative_m = t(matrix(data =detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative$DOC, nrow = 12))
detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative_means = colMeans(detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative_m, na.rm = T)











#Create the matrix so you have two dimentions
detrend_plot_non_psychiatric <- cbind(matrix(1:12, nrow = 12),detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative_means)
detrend_plot_non_psychiatric <- as.data.frame(detrend_plot_non_psychiatric)

ggplot(detrend_plot_non_psychiatric, aes(x= detrend_plot_non_psychiatric$V1, y= detrend_plot_non_psychiatric$detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative_means))+
  scale_x_continuous(name= "Month", breaks= seq(0,12,1))+
  scale_y_continuous(name= "Coefficient")+
  geom_line()+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "grey", size = 0.5))











####Residuals (all approximate)####
##Additive
random_data2.minus.psychiatric_diagnoses_ts_additive = detrend_data2.minus.psychiatric_diagnoses_ts_additive$DOC - detrend_data2.minus.psychiatric_diagnoses_ts_additive_means
plot(as.ts(random_data2.minus.psychiatric_diagnoses_ts_additive))

##Multiplicative
random_data2.minus.psychiatric_diagnoses_ts_multiplicative = detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative$DOC / detrend_data2.minus.psychiatric_diagnoses_ts_multiplicative_means



###Plot