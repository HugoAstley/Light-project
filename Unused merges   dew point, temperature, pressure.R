










#####Temperature#####
Temperature_Region_1 <- Temperature.merged%>%
  filter(Temperature.merged$State.Code %in% c("6", "17", "19", "27", "28", "30", "36", "37"))

Temperature_Region_2 <- Temperature.merged%>%
  filter(Temperature.merged$State.Code %in% c("11", "12", "13", "20", "21", "23", "25", "32", "33", "39", "47"))

Temperature_Region_3 <- Temperature.merged%>%
  filter(Temperature.merged$State.Code %in% c("1", "8", "9", "16", "22", "34", "40", "41", "44", "46"))

Temperature_Region_4 <- Temperature.merged%>%
  filter(Temperature.merged$State.Code %in% c("10", "24", "26", "29", "35", "42", "45", "48"))


#Put the pollution data for each of the regions into a list
Temperature.group <- list(Temperature_Region_1, Temperature_Region_2, Temperature_Region_3, Temperature_Region_4)

#Add ADM_MON and SVYEAR columns
for(i in 1:4){Temperature.group[[i]]$ADM_MON <- substr(Temperature.group[[i]]$`Date Local`,6,7)
Temperature.group[[i]]$SVYEAR <- substr(Temperature.group[[i]]$`Date Local`,1,4)}

#Grouping by month, and applying the mean function to the temperature
mean.temperature <- list()
for(i in 1:4){mean.temperature[[i]] <- aggregate(Temperature.group[[i]]$`Arithmetic Mean`, list(Temperature.group[[i]]$ADM_MON), mean)
mean.temperature[[i]]$REGION <- rep(i,length(mean.temperature))}

##turn temperature into a list
mean.temperature <- bind_rows(mean.temperature)
names(mean.temperature)[2] <- "mean.temperature"
mean.temperature$Group.1 <- as.numeric(mean.temperature$Group.1)

#Join with the rest of the data
data2 <- merge(x= data2, y= mean.temperature, by.x = c("REGION", "ADM_MON"), by.y = c("REGION", "Group.1"), all.x = TRUE)







######Dew point####



Dew.merged <- do.call("rbind", list(daily_RH_DP_1980,
                                    daily_RH_DP_1981,
                                    daily_RH_DP_1982,
                                    daily_RH_DP_1983,
                                    daily_RH_DP_1984,
                                    daily_RH_DP_1985,
                                    daily_RH_DP_1986,
                                    daily_RH_DP_1987,
                                    daily_RH_DP_1988,
                                    daily_RH_DP_1989,
                                    daily_RH_DP_1990,
                                    daily_RH_DP_1991,
                                    daily_RH_DP_1992,
                                    daily_RH_DP_1993,
                                    daily_RH_DP_1994,
                                    daily_RH_DP_1995,
                                    daily_RH_DP_1996,
                                    daily_RH_DP_1997,
                                    daily_RH_DP_1998,
                                    daily_RH_DP_1999,
                                    daily_RH_DP_2000,
                                    daily_RH_DP_2001,
                                    daily_RH_DP_2002,
                                    daily_RH_DP_2003,
                                    daily_RH_DP_2004,
                                    daily_RH_DP_2005,
                                    daily_RH_DP_2006))

write.csv(Dew.merged, "Dew.merged")

Dew_Region_1 <- Dew.merged%>%
  filter(Dew.merged$`State Code` %in% c("6", "17", "19", "27", "28", "30", "36", "37"))

Dew_Region_2 <- Dew.merged%>%
  filter(Dew.merged$`State Code` %in% c("11", "12", "13", "20", "21", "23", "25", "32", "33", "39", "47"))

Dew_Region_3 <- Dew.merged%>%
  filter(Dew.merged$`State Code` %in% c("1", "8", "9", "16", "22", "34", "40", "41", "44", "46"))

Dew_Region_4 <- Dew.merged%>%
  filter(Dew.merged$`State Code` %in% c("10", "24", "26", "29", "35", "42", "45", "48"))


#Put the Dew data for each of the regions into a list
Dew.group <- list(Dew_Region_1, Dew_Region_2, Dew_Region_3, Dew_Region_4)

#Add ADM_MON and SVYEAR columns
for(i in 1:4){Dew.group[[i]]$ADM_MON <- substr(Dew.group[[i]]$Date,6,7)
Dew.group[[i]]$SVYEAR <- substr(Dew.group[[i]]$Date,1,4)}

#Grouping by month, and applying the mean function to the Dew
mean.Dew <- list()
for(i in 1:4){mean.Dew[[i]] <- aggregate(Dew.group[[i]]$Dew, list(Dew.group[[i]]$ADM_MON), mean)
mean.Dew[[i]]$REGION <- rep(i,length(mean.Dew))}

##turn Dew into a list
mean.Dew <- bind_rows(mean.Dew)
names(mean.Dew)[2] <- "mean.Dew"
mean.Dew$Group.1 <- as.numeric(mean.Dew$Group.1)

#Join with the rest of the data
data2 <- merge(x= data2, y= mean.Dew, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "Group.1", "SVYEAR"), all.x = TRUE)



######Pressure####



Pressure.merged <- do.call("rbind", list(daily_PRESS_1980,
                                         daily_PRESS_1981,
                                         daily_PRESS_1982,
                                         daily_PRESS_1983,
                                         daily_PRESS_1984,
                                         daily_PRESS_1985,
                                         daily_PRESS_1986,
                                         daily_PRESS_1987,
                                         daily_PRESS_1988,
                                         daily_PRESS_1989,
                                         daily_PRESS_1990,
                                         daily_PRESS_1991,
                                         daily_PRESS_1992,
                                         daily_PRESS_1993,
                                         daily_PRESS_1994,
                                         daily_PRESS_1995,
                                         daily_PRESS_1996,
                                         daily_PRESS_1997,
                                         daily_PRESS_1998,
                                         daily_PRESS_1999,
                                         daily_PRESS_2000,
                                         daily_PRESS_2001,
                                         daily_PRESS_2002,
                                         daily_PRESS_2003,
                                         daily_PRESS_2004,
                                         daily_PRESS_2005,
                                         daily_PRESS_2006))

write.csv(Pressure.merged, "Pressure.merged")

Pressure_Region_1 <- Pressure.merged%>%
  filter(Pressure.merged$`State Code` %in% c("6", "17", "19", "27", "28", "30", "36", "37"))

Pressure_Region_2 <- Pressure.merged%>%
  filter(Pressure.merged$`State Code` %in% c("11", "12", "13", "20", "21", "23", "25", "32", "33", "39", "47"))

Pressure_Region_3 <- Pressure.merged%>%
  filter(Pressure.merged$`State Code` %in% c("1", "8", "9", "16", "22", "34", "40", "41", "44", "46"))

Pressure_Region_4 <- Pressure.merged%>%
  filter(Pressure.merged$`State Code` %in% c("10", "24", "26", "29", "35", "42", "45", "48"))


#Put the Pressure data for each of the regions into a list
Pressure.group <- list(Pressure_Region_1, Pressure_Region_2, Pressure_Region_3, Pressure_Region_4)

#Add ADM_MON and SVYEAR columns
for(i in 1:4){Pressure.group[[i]]$ADM_MON <- substr(Pressure.group[[i]]$`Date Local`,6,7)
Pressure.group[[i]]$SVYEAR <- substr(Pressure.group[[i]]$`Date Local`,1,4)}

#Grouping by month, and applying the mean function to the Pressure
mean.Pressure <- list()
for(i in 1:4){mean.Pressure[[i]] <- aggregate(Pressure.group[[i]]$`Arithmetic Mean`, list(Pressure.group[[i]]$ADM_MON), mean)
mean.Pressure[[i]]$REGION <- rep(i,length(mean.Pressure))}

##turn Pressure into a list
mean.Pressure <- bind_rows(mean.Pressure)
names(mean.Pressure)[2] <- "mean.Pressure"
mean.Pressure$Group.1 <- as.numeric(mean.Pressure$Group.1)

#Join with the rest of the data
data2 <- merge(x= data2, y= mean.Pressure, by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "Group.1", "SVYEAR"), all.x = TRUE)



