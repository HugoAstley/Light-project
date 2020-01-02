######Hospital ownership#####

#Data1
demographicplot_OWNER_data1 <- data1 %>%
  ggplot(aes(x= as.factor(data1$OWNER), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Proprietary", "Government", "Nonprofit"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Hospital ownership") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2
demographicplot_OWNER_data2 <- data2 %>%
  ggplot(aes(x= as.factor(data2$OWNER), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Proprietary", "Government", "Nonprofit"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Hospital ownership") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 



#Psychiatric diagnoses
demographicplot_OWNER_psychiatric <- psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(psychiatric_diagnoses$OWNER), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Proprietary", "Government", "Nonprofit"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Hospital ownership") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2 without psychiatric diagnoses
demographicplot_OWNER_minuspsychiatric <- data2.minus.psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(data2.minus.psychiatric_diagnoses$OWNER), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Proprietary", "Government", "Nonprofit"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Hospital ownership") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 





####Number of beds#####
#Data1
demographicplot_BEDSIZE_data1 <- data1 %>%
  ggplot(aes(x= as.factor(data1$BEDSIZE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("6-99", "100-199", "200-299", "300-499", "500+"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Hospital size") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2
demographicplot_BEDSIZE_data2 <- data2 %>%
  ggplot(aes(x= as.factor(data2$BEDSIZE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Proprietary", "Government", "Nonprofit"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Hospital size") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Psychiatric diagnoses
demographicplot_BEDSIZE_psychiatric <- psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(psychiatric_diagnoses$BEDSIZE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Proprietary", "Government", "Nonprofit"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Hospital size") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2 without psychiatric diagnoses
demographicplot_BEDSIZE_minuspsychiatric <- data2.minus.psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(data2.minus.psychiatric_diagnoses$BEDSIZE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Proprietary", "Government", "Nonprofit"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Hospital size") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60))


###Plot in one page
grid.arrange(demographicplot_OWNER_data1, demographicplot_OWNER_data2, demographicplot_OWNER_psychiatric, demographicplot_OWNER_minuspsychiatric,
             demographicplot_BEDSIZE_data1, demographicplot_BEDSIZE_data2, demographicplot_BEDSIZE_psychiatric, demographicplot_BEDSIZE_minuspsychiatric,
             ncol=4)
