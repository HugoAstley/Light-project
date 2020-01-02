######Light data: Integrating the light data with patient data#####
light.durations$Sunlight.Duration..minutes.

#Mean and standard deviation of light level for each month
mean.light.per.month.year.REGION <- aggregate(Sunlight.Duration..minutes. ~ ADM_MON + SVYEAR + REGION, data = light.durations, FUN = mean, trim= 0, na.rm= F) #mean

sd.light.per.month.year.REGION <- aggregate(Sunlight.Duration..minutes. ~ ADM_MON + SVYEAR + REGION, data = light.durations, FUN = sd, na.rm= F) #standard deviation

#Rename the column
names(mean.light.per.month.year.REGION)[4] <- "Mean_of_Sunlight_Duration_minutes"
names(sd.light.per.month.year.REGION)[4] <- "Sd_of_Sunlight_Duration_minutes"

##Merge the light and patient data
#to merge they have to be the same type
data2$ADM_MON <- as.numeric(data2$ADM_MON)
data2$SVYEAR <- as.numeric(data2$SVYEAR)

mean.light.per.month.year.REGION$ADM_MON <- as.numeric(mean.light.per.month.year.REGION$ADM_MON)
mean.light.per.month.year.REGION$SVYEAR <- as.numeric(mean.light.per.month.year.REGION$SVYEAR)

sd.light.per.month.year.REGION$ADM_MON <- as.numeric(sd.light.per.month.year.REGION$ADM_MON)
sd.light.per.month.year.REGION$SVYEAR <- as.numeric(sd.light.per.month.year.REGION$SVYEAR)


#merge
data2 <- merge(x= data2, y= mean.light.per.month.year.REGION, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x= TRUE)
data2 <- merge(x= data2, y= sd.light.per.month.year.REGION, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x= TRUE)

write.csv(data2, "data2")