####Create a linear variable for date####



#Make month have at two digits throughout
data2$ADM_MON <- sprintf("%02d", as.numeric(data2$ADM_MON))

#Put them in one dataframe
data2$Date <- paste0("01", data2$ADM_MON, data2$SVYEAR)

#Put it in a date format
data2$Date <- dmy(data2$Date)


###Let's make it linear

data2$Time <- difftime(data2$Date, as.Date("1990-01-01"), units = "days")




save(data2,file="data2.Rda")

#Original data saved under "data2.backup.Rda"