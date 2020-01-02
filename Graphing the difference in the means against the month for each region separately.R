#Graphing the difference in the means against the month for each region separately
############REGION1###############
#Reduce data to the region of interest 
psychiatric_diagnoses.REGION1 <- psychiatric_diagnoses%>%
  filter(REGION==1)


data2.minus.psychiatric_diagnoses.REGION1 <- data2.minus.psychiatric_diagnoses%>%
  filter(REGION==1)


#Group the data according to their admission month and find a mean length of stay for that month
#for all
DOC_by_mon <- group_by(data2.minus.psychiatric_diagnoses.REGION1, ADM_MON)
by_mon_summary <- summarise(DOC_by_mon, DOC_mean_all= mean(DOC))

#for psych
DOC_by_mon_psych <- group_by(psychiatric_diagnoses.REGION1, ADM_MON)
by_mon_summary_psych <- summarise(DOC_by_mon_psych, DOC_mean_psych= mean(DOC))



#merge these into a table
Compare_means_region1 <- merge(by_mon_summary, by_mon_summary_psych, by= "ADM_MON")

#find the difference in the means and add this to the table
Compare_means_region1$difference_means <- by_mon_summary_psych$DOC_mean_psych - by_mon_summary$DOC_mean_all


#Merge the DOCs and light into a table by month 
Compare_means.light_region1 <-merge(Compare_means_region1, North_east_sunshine_hours, by= "ADM_MON")



#graph the differences in the means against the amount of light (by month)
Compare_means.light_region1 %>%
  filter(ADM_MON < 13) %>%
  ggplot(aes(x=(Average), y= (difference_means)))+
  geom_point(alpha=1)+
  geom_smooth(method= lm)+
  coord_cartesian(ylim= c(0, 3), xlim= c(100,350))+
  xlab("Average amount of sunlight in major cities (hours per month)")+
  ylab("Average length of stay (psychiatric patients - all patients) (in days)")+
  ggtitle("The relationship between amount of sunlight and the length of stay of psychiatric patients compared with all patients", "North-east")


##############REGION2##############
#Reduce data to the region of interest
psychiatric_diagnoses.REGION2 <- psychiatric_diagnoses%>%
  filter(REGION==2)

data2.minus.psychiatric_diagnoses.REGION2 <- data2.minus.psychiatric_diagnoses%>%
  filter(REGION==2)

#Group the data according to their admission month and find a mean length of stay for that month
#for all
DOC_by_mon <- group_by(data2.minus.psychiatric_diagnoses.REGION2, ADM_MON)
by_mon_summary <- summarise(DOC_by_mon, DOC_mean_all= mean(DOC))

#for psych
DOC_by_mon_psych <- group_by(psychiatric_diagnoses.REGION2, ADM_MON)
by_mon_summary_psych <- summarise(DOC_by_mon_psych, DOC_mean_psych= mean(DOC))




#merge these into a table
Compare_means_region2 <- merge(by_mon_summary, by_mon_summary_psych, by= "ADM_MON")

#find the difference in the means and add this to the table
Compare_means_region2$difference_means <- by_mon_summary_psych$DOC_mean_psych - by_mon_summary$DOC_mean_all


#Merge the DOCs and light into a table by month 
Compare_means.light_region2 <-merge(Compare_means_region2, Midwest_sunshine_hours, by= "ADM_MON")




#graph the differences in the means against the month
Compare_means.light_region2 %>%
  filter(ADM_MON < 13) %>%
  ggplot(aes(x=(Average), y= (difference_means)))+
  geom_point(alpha=1)+
  geom_smooth(method= lm)+
  coord_cartesian(ylim= c(0, 3), xlim= c(100,350))+
  xlab("Average amount of sunlight in major cities (hours per month)")+
  ylab("Average length of stay (psychiatric patients - all patients) (in days)")+
  ggtitle("The relationship between amount of sunlight and the length of stay of psychiatric patients compared with all patients", "Midwest")



##############REGION3##############
#Reduce data to the region of interest
psychiatric_diagnoses.REGION3 <- psychiatric_diagnoses%>%
  filter(REGION==3)

data2.minus.psychiatric_diagnoses.REGION3 <- data2.minus.psychiatric_diagnoses%>%
  filter(REGION==3)

#Group the data according to their admission month and find a mean length of stay for that month
#for all
DOC_by_mon <- group_by(data2.minus.psychiatric_diagnoses.REGION3, ADM_MON)
by_mon_summary <- summarise(DOC_by_mon, DOC_mean_all= mean(DOC))

#for psych
DOC_by_mon_psych <- group_by(psychiatric_diagnoses.REGION3, ADM_MON)
by_mon_summary_psych <- summarise(DOC_by_mon_psych, DOC_mean_psych= mean(DOC))


#merge these into a table
Compare_means_region3 <- merge(by_mon_summary, by_mon_summary_psych, by= "ADM_MON")

#find the difference in the means and add this to the table
Compare_means_region3$difference_means <- by_mon_summary_psych$DOC_mean_psych - by_mon_summary$DOC_mean_all


#Merge the DOCs and light into a table by month 
Compare_means.light_region3 <-merge(Compare_means_region3, South_sunshine_hours, by= "ADM_MON")


#graph the differences in the means against the month
Compare_means.light_region3 %>%
  filter(ADM_MON < 13) %>%
  ggplot(aes(x=(Average), y= (difference_means)))+
  geom_point(alpha=1)+
  geom_smooth(method= lm)+
  coord_cartesian(ylim= c(0, 3), xlim= c(100,350))+
  xlab("Average amount of sunlight in major cities (hours per month)")+
  ylab("Average length of stay (psychiatric patients - all patients) (in days)")+
  ggtitle("The relationship between amount of sunlight and the length of stay of psychiatric patients compared with all patients", "South")




##############REGION4##############
#Reduce data to the region of interest
psychiatric_diagnoses.REGION4 <- psychiatric_diagnoses%>%
  filter(REGION==4)

data2.minus.psychiatric_diagnoses.REGION4 <- data2.minus.psychiatric_diagnoses%>%
  filter(REGION==4)

#Group the data according to their admission month and find a mean length of stay for that month
#for all
DOC_by_mon <- group_by(data2.minus.psychiatric_diagnoses.REGION4, ADM_MON)
by_mon_summary <- summarise(DOC_by_mon, DOC_mean_all= mean(DOC))

#for psych
DOC_by_mon_psych <- group_by(psychiatric_diagnoses.REGION4, ADM_MON)
by_mon_summary_psych <- summarise(DOC_by_mon_psych, DOC_mean_psych= mean(DOC))


#merge these into a table
Compare_means_region4 <- merge(by_mon_summary, by_mon_summary_psych, by= "ADM_MON")

#find the difference in the means and add this to the table
Compare_means_region4$difference_means <- by_mon_summary_psych$DOC_mean_psych - by_mon_summary$DOC_mean_all


#Merge the DOCs and light into a table by month 
Compare_means.light_region4 <-merge(Compare_means_region4, West_sunshine_average, by= "ADM_MON")

#graph the differences in the means against the month
Compare_means.light_region4 %>%
  filter(ADM_MON < 13) %>%
  ggplot(aes(x=(Average), y= (difference_means)))+
  geom_point(alpha=1)+
  geom_smooth(method= lm)+
  coord_cartesian(ylim= c(0, 3), xlim= c(100,350))+
  xlab("Average amount of sunlight in major cities (hours per month)")+
  ylab("Average length of stay (psychiatric patients - all patients) (in days)")+
  ggtitle("The relationship between amount of sunlight and the length of stay of psychiatric patients compared with all patients", "West")



#Correlation coefficient between light and DOC for each region separately
