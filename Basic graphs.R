##########################Make the data into a time series########################################
library(digest)
library(astsa, quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2)
library(knitr)
library(printr)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(TTR)

data2_ts <- data2

data2_ts <- data2_ts %>%
  dplyr::select(DOC, Date)

#Make it date again
data2_ts$Date <- as.Date(data2_ts$Date)

#Cut down to month and year because they are the only columns changing
data2_ts$Date <- substr((data2_ts$Date), 1,7)

#Make it a tibble
data2_ts <- as_tibble(data2_ts)


#Aggregate because you aren't allowed duplicates
data2_ts <- aggregate(data2_ts, by = list(data2_ts$Date),
                                      FUN = mean)

#Cut out the column that gets corrupted
data2_ts <- data2_ts[,(1:2)]

#Remane the group 1 column
colnames(data2_ts)[1] <- "Date"



####Time series analysis############Anomaly detection pubr##########


#plot the ts
Time_series_plot_data2 <- ggplot(data2_ts, aes(x=(ADM_MON), y= (Compare_means_allregions$DOC_mean_psych)))+
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





data2SMA3 <- SMA(data2_ts$DOC, n=3)
plot.ts(data2SMA3)


data2SMA8 <- SMA(data2_ts$DOC, n=8)
plot.ts(data2SMA8)

data2SMA10 <- SMA(data2_ts$DOC, n=10)
plot.ts(data2SMA10)

data2SMA12 <- SMA(data2_ts$DOC, n=12)
plot.ts(data2SMA12)

data2SMA16 <- SMA(data2_ts$DOC, n=16)
plot.ts(data2SMA16)



####take away the trend####
##Additive
detrend_data2_ts_additive <- data2_ts

detrend_data2_ts_additive$DOC <- data2_ts$DOC-data2SMA12
plot(as.ts(detrend_data2_ts_additive))

##Multiplicative
detrend_data2_ts_multiplicative <- data2_ts

detrend_data2_ts$DOC <- data2_ts$DOC/data2SMA12
plot(as.ts(detrend_data2_ts_multiplicative))


####Finding a mean for each month####
##Additive
detrend_data2_ts_additive_m = t(matrix(data =detrend_data2_ts_additive$DOC, nrow = 12))
detrend_data2_ts_additive_means = colMeans(detrend_data2_ts_additive_m, na.rm = T)

plot(as.ts(rep(detrend_data2_ts_additive_means,1)))

##Multiplicative
detrend_data2_ts_multiplicative_m = t(matrix(data =detrend_data2_ts_multiplicative$DOC, nrow = 12))
detrend_data2_ts_multiplicative_means = colMeans(detrend_data2_ts_multiplicative_m, na.rm = T)

plot(as.ts(rep(seasonal_data2_ts,1)))



####Residuals (all approximate)####
##Additive
random_data2_ts_additive = detrend_data2_ts_additive$DOC - detrend_data2_ts_additive_means
plot(as.ts(random_data2_ts_additive))

##Multiplicative
random_data2_ts_multiplicative = detrend_data2_ts_multiplicative$DOC / detrend_data2_ts_multiplicative_means



####Which one is better: multiplicative or additive?####
ssacf<- function(x) sum(acf(x, na.action = na.omit)$acf^2)
compare_ssacf<-function(add,mult) ifelse(ssacf(add)< ssacf(mult), "Additive", "Multiplicative") 
sample_ts[,.(ts_type = compare_ssacf(residual_a, residual_m ))]







######Differencing to make it stationary#####
library(forecast)

ndiffs(detrend_data2_ts[,2])  # number of differences need to make it stationary

stationaryTS <- diff(detrend_data2_ts[13:204,2], differences= 1)
plot(stationaryTS, type="l", main="Differenced and Stationary")  # appears to be stationary

###Adjusting for seasonality, and getting random noise, again but now will the fuly stationary data
#Adjusting for seasons
m_data2_ts = t(matrix(data =detrend_data2_ts$DOC, nrow = 12))
seasonal_data2_ts = colMeans(m_data2_ts, na.rm = T)
plot(as.ts(rep(seasonal_data2_ts,1)))


#Time series minus trend and seasonality = random noise (all approximate)
random_data2_ts = detrend_data2_ts$DOC - seasonal_data2_ts
plot(as.ts(random_data2_ts))


#######Check if the data is stationary###########
library(tseries)

stationary <- adf.test(detrend_data2_ts[12:204,2], alternative = 'stationary', k= 0)
stationary


###########Anomaly detection##############
install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

#copied and pasted from the website (doesnt work)
data(raw_data) 
res<-AnomalyDetectionTs(data2_ts, max_anoms=0.02, direction='both', plot=TRUE)
res$plot

#on the unmodified time series data
AnomalyDetectionVec(data2_ts[,2], max_anoms=0.02, period = 12, direction='both', only_last=FALSE, plot=TRUE)

#on the time series data without the trend
AnomalyDetectionVec(detrend_data2_ts[,2], max_anoms=0.02, period = 12, direction='both', only_last=FALSE, plot=TRUE)

#MAIN ONE: on the seasons. #Can't because it is only one period
AnomalyDetectionVec(seasonal_data2_ts, max_anoms=0.02, period = 12, direction='both', only_last=FALSE, plot=TRUE)

#on the remainding variance time series
AnomalyDetectionVec(random_data2_ts, max_anoms=0.02, period = 12, direction='both', only_last=FALSE, plot=TRUE)
###########



##Hopefully not needed
library(tibbletime)
data2_ts <- as_tbl_time(data2_ts, index = Date)

data2_ts %>% 
  as_period("daily")
##Hopefully not needed














############All diagnoses across all regions############

#ADM_MON vs DOC

#Group the data according to their admission month and find a mean length of stay for that month
#for all
DOC_by_mon <- group_by(data2, ADM_MON)
by_mon_summary <- summarise(DOC_by_mon, DOC_mean_all= mean(DOC), DOC_sd_all= sd(DOC))

#for all-psychiatric diagnoses
DOC_by_mon_minus_psychiatric <- group_by(data2.minus.psychiatric_diagnoses, ADM_MON)
by_mon_summary_minus_psychiatric <- summarise(DOC_by_mon, DOC_mean_all_minus_psychiatric= mean(DOC), DOC_sd_all_minus_psychiatric= sd(DOC))

#for psych
DOC_by_mon_psych <- group_by(psychiatric_diagnoses, ADM_MON)
by_mon_summary_psych <- summarise(DOC_by_mon_psych, DOC_mean_psych= mean(DOC), DOC_sd_psych= sd(DOC))

#merge these into a table
Compare_means_allregions <- merge(by_mon_summary, by_mon_summary_psych, by= "ADM_MON")
Compare_means_allregions <- merge(Compare_means_allregions, by_mon_summary_minus_psychiatric, by= "ADM_MON")






##Graphs: plotting average stay against TIME##
#graph the amount of light vs the average DOC for all diagnoses
TIME.V.DOC_all_three <- ggplot(Compare_means_allregions, aes(x=(ADM_MON), y= (Compare_means_allregions$DOC_mean_psych)))+
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





ggarrange(TIME.V.DOC_all_three, TIME.V.DOC_all_minus, TIME.V.DOC_psychiatric, labels = c("A", "B", "C"), ncol = 1, nrow = 3, common.legend= T, legend= "top")