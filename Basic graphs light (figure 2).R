
#graph the amount of light vs the average DOC for all diagnoses
TIME.V.DOC_all_minus <- ggplot(Compare_means_allregions, aes(x=(ADM_MON), y= (Compare_means_allregions$DOC_mean_all_minus_psychiatric)))+
  scale_x_continuous("Month", breaks= seq(0,12,1))+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  geom_point(alpha=1)+
  geom_smooth(aes(colour= "All-Minus"), color= "grey70")+
  xlab("Month")+
  ylab("Average length of stay (in days)")


#graph the amount of light vs the average DOC for all diagnoses
TIME.V.DOC_psychiatric <- ggplot(Compare_means_allregions, aes(x=(ADM_MON), y= (Compare_means_allregions$DOC_mean_psych)))+
  scale_x_continuous("Month", breaks= seq(0,12,1))+
  geom_point(alpha=1)+
  geom_smooth(aes(colour= "Psychiatric"), color= "royalblue1")+
  xlab("Month")+
  ylab("Average length of stay (in days)")



#Merge the DOCs and light into a table by month 
Compare_means.light_allregions <-merge(Compare_means_allregions, Average_across_all_cities, by= "ADM_MON")

##Plotting average stay against LIGHT

#graph the amount of light vs the average DOC for all diagnoses
Compare_means.light_allregions %>%
  filter(ADM_MON < 13) %>%
  ggplot(aes(x=(Average), y= (Compare_means.light_allregions$DOC_mean_all)))+
  geom_point(alpha=1)+
  geom_smooth(method= lm)+
  coord_cartesian(ylim= c(5, 6), xlim= c(100,350))+
  xlab("Average amount of sunlight in major cities (hours per month)")+
  ylab("Average length of stay (in days)")+
  ggtitle("The relationship between amount of sunlight and the length of stay", "All patients")


####Correlations coefficients####
cor.test(data= data2, data2$DOC, data2$Average)













############Psychiatric diagnoses and All- psychiatric diagnoses; All regions############

##Plotting average stay against LIGHT

#graph the amount of light vs the average DOC for all diagnoses
Compare_means.light_allregions %>%
  filter(ADM_MON < 13) %>%
  ggplot(aes(x=(Average), y= (Compare_means.light_allregions$DOC_mean_all)))+
  geom_point(alpha=1)+
  geom_smooth(method= lm)+
  coord_cartesian(ylim= c(5, 6), xlim= c(100,350))+
  xlab("Average amount of sunlight in major cities (hours per month)")+
  ylab("Average length of stay (in days)")+
  ggtitle("The relationship between amount of sunlight and the length of stay", "All patients")

#graph the amount of light vs the average DOC for psychiatric diagnoses
Compare_means.light_allregions %>%
  filter(ADM_MON < 13) %>%
  ggplot(aes(x=(Average), y= (Compare_means.light_allregions$DOC_mean_psych)))+
  geom_point(alpha=1)+
  geom_smooth(method= lm)+
  coord_cartesian(ylim= c(6, 7), xlim= c(100,350))+
  xlab("Average amount of sunlight in major cities (hours per month)")+
  ylab("Average length of stay (in days)")+
  ggtitle("The relationship between amount of sunlight and the length of stay", "Psychiatric patients")



####Correlations coefficients####
cor.test(data= data2.minus.psychiatric_diagnoses, data2.minus.psychiatric_diagnoses$DOC, data2.minus.psychiatric_diagnoses$Average)
cor.test(data= psychiatric_diagnoses, psychiatric_diagnoses$Average, psychiatric_diagnoses$DOC)

cor.test(data= Compare_means.light_allregions, Compare_means.light_allregions$Average, Compare_means.light_allregions$difference_means)