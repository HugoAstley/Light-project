####Solar noon; Sunrise time; Sunset time: for accuracy have to adjust for DST so only REGION 1 valid

#combine the two datasets
REGION_1_daily_1990_2000 <- rbind(REGION_1_daily_1990, REGION_1_daily_2000)

#row for month
REGION_1_daily_1990_2000$ADM_MON <- substr(REGION_1_daily_1990_2000$Date,6,7)
REGION_1_daily_1990_2000$ADM_MON <- as.numeric(REGION_1_daily_1990_2000$ADM_MON)

#row for year
REGION_1_daily_1990_2000$SVYEAR <- substr(REGION_1_daily_1990_2000$Date,1,4)
REGION_1_daily_1990_2000$SVYEAR <- as.numeric(REGION_1_daily_1990_2000$SVYEAR)

#accounting for DST
REGION_1_daily_1990_2000$`Solar Noon (LST)`[REGION_1_daily_1990_2000$ADM_MON>2 & REGION_1_daily_1990_2000$ADM_MON<11] <-
  REGION_1_daily_1990_2000$`Solar Noon (LST)`[REGION_1_daily_1990_2000$ADM_MON>2 & REGION_1_daily_1990_2000$ADM_MON<11] + (1/24)

REGION_1_daily_1990_2000$`Sunrise Time (LST)`[REGION_1_daily_1990_2000$ADM_MON>2 & REGION_1_daily_1990_2000$ADM_MON<11] <-
  REGION_1_daily_1990_2000$`Sunrise Time (LST)`[REGION_1_daily_1990_2000$ADM_MON>2 & REGION_1_daily_1990_2000$ADM_MON<11] + (1/24)

REGION_1_daily_1990_2000$`Sunset Time (LST)`[REGION_1_daily_1990_2000$ADM_MON>2 & REGION_1_daily_1990_2000$ADM_MON<11] <-
  REGION_1_daily_1990_2000$`Sunset Time (LST)`[REGION_1_daily_1990_2000$ADM_MON>2 & REGION_1_daily_1990_2000$ADM_MON<11] + (1/24)


##calculate means and standard deviations for each month
#means
mean.solar.noon <- aggregate(Solar.Noon..LST. ~ ADM_MON + SVYEAR + REGION, data = REGION_1_daily_1990_2000, FUN = mean, trim= 0, na.rm= F)
mean.sunrise.time <- aggregate(Sunrise.Time..LST. ~ ADM_MON + SVYEAR + REGION, data = REGION_1_daily_1990_2000, FUN = mean, trim= 0, na.rm= F)
mean.sunset.time <- aggregate(Sunset.Time..LST. ~ ADM_MON + SVYEAR + REGION, data = REGION_1_daily_1990_2000, FUN = mean, trim= 0, na.rm= F)

#standard deviations
sd.solar.noon <- aggregate(Solar.Noon..LST. ~ ADM_MON + SVYEAR + REGION, data = REGION_1_daily_1990_2000, FUN = sd, na.rm= F)
sd.sunrise.time <- aggregate(Sunrise.Time..LST. ~ ADM_MON + SVYEAR + REGION, data = REGION_1_daily_1990_2000, FUN = sd, na.rm= F)
sd.sunset.time <- aggregate(Sunset.Time..LST. ~ ADM_MON + SVYEAR + REGION, data = REGION_1_daily_1990_2000, FUN = sd, na.rm= F)

#change the names of mean solar noon; mean sunset time; and mean sunrise time etc. so that they can be more easily included in the models
names(mean.solar.noon)[4] <- "mean_solar_noon"
names(mean.sunrise.time)[4] <- "mean_sunrise_time"
names(mean.sunset.time)[4] <- "mean_sunset_time"

names(sd.solar.noon)[4] <- "sd_solar_noon"
names(sd.sunrise.time)[4] <- "sd_sunrise_time"
names(sd.sunset.time)[4] <- "sd_sunset_time"

#merge
data2 <- merge(x= data2, y= mean.solar.noon, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x= TRUE)
data2 <- merge(x= data2, y= sd.solar.noon, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x= TRUE)
data2 <- merge(x= data2, y= mean.sunrise.time, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x= TRUE)
data2 <- merge(x= data2, y= sd.sunrise.time, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x= TRUE)
data2 <- merge(x= data2, y= mean.sunset.time, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x= TRUE)
data2 <- merge(x= data2, y= sd.sunset.time, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x= TRUE)

