##Intercorrelation##


#Get a dataframe containing the variables you are going to cross correlate
# select variables 
myvars <- c(data2$REGION, data2$ADM_MON, data2$AGE, data2$DISC_MON, data2$Mean_of_Sunlight_Duration_minutes, data2$ESOP1, data2$ADM_TYPE, data2$ASOURCE)
newdata <- data2[myvars]

cross_correlate_data2 <- dataframe()
cross_correlate_data2 <- cbind(data2$REGION, data2$ADM_MON, data2$AGE, data2$DISC_MON, data2$Mean_of_Sunlight_Duration_minutes, data2$ESOP1, data2$ADM_TYPE, data2$ASOURCE)

cross_correlate_data2 <- sapply(cross_correlate_data2, as.numeric )




corrplot(cor(cross_correlate_data2), method="shade",shade.col=NA, tl.col="black", tl.srt=45)

cor(cross_correlate_data2)
