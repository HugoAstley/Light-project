###Principal component analysis
principal.component.analysis <- prcomp(~ data2$AGE 
                                       + data2$DOC 
                                       + data2$Mean_of_Sunlight_Duration_minutes
                                       + data2$SVYEAR
                                       + data2$ADM_MON
                                       + data2$AQI
                                       , data= data2, scale= TRUE)
principal.component.analysis
summary(principal.component.analysis)
biplot(principal.component.analysis, scale=0)


















####Factor analysis of mixed data####
##Select factors
data2.FAMD.variables <- data2 %>%
  dplyr::select(AQI
                ,mean_solar_noon
                ,sd_solar_noon
                ,mean_sunrise_time
                ,sd_sunrise_time
                ,mean_sunset_time
                ,sd_sunset_time
                ,ALLSKY_SFC_LW_DWN
                ,ALLSKY_SFC_SW_DWN
                ,ALLSKY_TOA_SW_DWN
                ,KT
                ,PRECTOT
                ,PS
                ,QV2M
                ,RH2M
                ,T2M
                ,T2M_RANGE
                ,T2MWET
                ,WS2M
                ,Mean_of_Sunlight_Duration_minutes
                ,Sd_of_Sunlight_Duration_minutes)



#Make sure the variables are a dataframe...
data2.FAMD.variables <- as.data.frame(data2.FAMD.variables)

#... so that you can scale the variables
data2.FAMD.variables <- scale(data2.FAMD.variables)

#...turn it from a matrix back into a dataframe
data2.FAMD.variables <- as.data.frame(data2.FAMD.variables)




#Turn the variables into factors
i=0
while(i < ncol(data2.FAMD.variables)){
  i=i+1
  data2.FAMD.variables[,i] = as.factor(data2.FAMD.variables[,i])}

#Run factor analysis of mixed data
FAMD.result <- FAMD(data2.FAMD.variables[1:10000,], graph = T)




fviz_screeplot(FAMD.result)
fviz_contrib(FAMD.result, "var", axes = 1)

fviz_contrib(FAMD.result, "var", axes = 2)


fviz_pca_var(FAMD.result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)








####Redundant#####

##Select factors
data2.FAMD.variables <- data2 %>%
  dplyr::select(REGION,DOC,AGE,SEX,Mean_of_Sunlight_Duration_minutes)

#Make sure the variables are a dataframe...
data2.FAMD.variables <- as.data.frame(data2.FAMD.variables)

#... so that you can scale the variables
data2.FAMD.variables <- scale(data2.FAMD.variables)

#...turn it from a matrix back into a dataframe
data2.FAMD.variables <- as.data.frame(data2.FAMD.variables)




#Turn the variables into factors
i=0
while(i < ncol(data2.FAMD.variables)){
  i=i+1
  data2.FAMD.variables[,i] = as.factor(data2.FAMD.variables[,i])}

#Run factor analysis of mixed data
FAMD.result <- FAMD(data2.FAMD.variables[1:10000,], graph = T)




fviz_screeplot(FAMD.result)
fviz_contrib(FAMD.result, "var", axes = 1)

fviz_contrib(FAMD.result, "var", axes = 2)


fviz_pca_var(FAMD.result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

data2$AQI
data2$mean_solar_noon
data2$sd_solar_noon
data2$mean_sunrise_time
data2$sd_sunrise_time
data2$mean_sunset_time
data2$sd_sunset_time
data2$ALLSKY_SFC_LW_DWN
data2$ALLSKY_SFC_SW_DWN
data2$ALLSKY_TOA_SW_DWN
data2$KT
data2$PRECTOT
data2$PS
data2$QV2M
data2$RH2M
data2$T2M
data2$T2M_RANGE
data2$T2MWET
data2$WS2M
data2$Mean_of_Sunlight_Duration_minutes
data2$Sd_of_Sunlight_Duration_minutes

#Make sure time is numeric...
data2.FAMD.variables$Time <- as.numeric(data2.FAMD.variables$Time)


