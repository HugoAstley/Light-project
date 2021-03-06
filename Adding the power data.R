#####Power data####
month.ABB <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

POWER_ALL_REGIONS_1990.2000 <- rbind(POWER_REGION1_1990, POWER_REGION2_1990, POWER_REGION3_1990, POWER_REGION4_1990,
                                     POWER_REGION1_2000, POWER_REGION2_2000, POWER_REGION3_2000, POWER_REGION4_2000)

POWER_ALL_REGIONS_1990.2000$ANN <- NULL
POWER_ALL_REGIONS_1990.2000.long <- POWER_ALL_REGIONS_1990.2000 %>% gather(ADM_MON, VALUE, JAN:DEC)
POWER_ALL_REGIONS_1990.2000.long$ADM_MON <- match(POWER_ALL_REGIONS_1990.2000.long$ADM_MON, month.ABB)
POWER_ALL_REGIONS_1990.2000.final <- POWER_ALL_REGIONS_1990.2000.long %>% spread(PARAMETER, VALUE)

##Remove the duplicate 2000's
bin1 <- POWER_ALL_REGIONS_1990.2000.final[205:216,]
bin2 <- POWER_ALL_REGIONS_1990.2000.final[421:432,]
bin3 <- POWER_ALL_REGIONS_1990.2000.final[553:564,]
bin4 <- POWER_ALL_REGIONS_1990.2000.final[769:780,]

POWER_ALL_REGIONS_1990.2000.final <- setdiff(POWER_ALL_REGIONS_1990.2000.final, bin1)
POWER_ALL_REGIONS_1990.2000.final <- setdiff(POWER_ALL_REGIONS_1990.2000.final, bin2)
POWER_ALL_REGIONS_1990.2000.final <- setdiff(POWER_ALL_REGIONS_1990.2000.final, bin3)
POWER_ALL_REGIONS_1990.2000.final <- setdiff(POWER_ALL_REGIONS_1990.2000.final, bin4)

#merge
data2 <- merge(x= data2, y= POWER_ALL_REGIONS_1990.2000.final[,3:17], by.x = c("REGION", "ADM_MON", "SVYEAR"), by.y = c("REGION", "ADM_MON", "SVYEAR"), all.x= TRUE)


write.csv(data2, "data2")
