# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(gridExtra)



#median age
median(data2$AGE)



#### plot ####

##Age
#Data1
demographicplot_age_data1 <- data1 %>%
  ggplot(aes(x= data1$AGE, y = (((..count..)/sum(..count..)))*100))+
  geom_histogram(alpha=0.7, binwidth = 1, color= "black", fill= "skyblue") +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.line = element_line(linetype= "dotted"),
    panel.background = element_rect(fill= "white")
  ) +
  xlab("Age (Years)") +
  ylab("Frequency (%)")+
  scale_x_continuous(breaks = round(seq(min(0), max(100), by = 10),1))+
  coord_cartesian(ylim = c(0, 2.5))+
  ggtitle("Raw dataset")

#Data2
demographicplot_age_data2 <- ggplot(data2, aes(x= data2$AGE, y = (((..count..)/sum(..count..)))*100))+
  geom_histogram(alpha=0.7, binwidth = 1, color= "black", fill= "skyblue") +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.line = element_line(linetype= "dotted"),
    panel.background = element_rect(fill= "white")
  ) +
  xlab("Age (Years)") +
  ylab("Frequency (%)")+
  scale_x_continuous(breaks = round(seq(min(0), max(100), by = 10),1))+
  coord_cartesian(ylim = c(0, 2.5))+
  ggtitle("After exclusion")


#Psychiatric diagnoses
demographicplot_age_psychiatric <- ggplot(psychiatric_diagnoses, aes(x= psychiatric_diagnoses$AGE, y = (((..count..)/sum(..count..)))*100))+
  geom_histogram(alpha=0.7, binwidth = 1, color= "black", fill= "skyblue") +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.line = element_line(linetype= "dotted"),
    panel.background = element_rect(fill= "white")
  ) +
  xlab("Age (Years)") +
  ylab("Frequency (%)")+
  scale_x_continuous(breaks = round(seq(min(0), max(100), by = 10),1))+
  coord_cartesian(ylim = c(0, 2.5))+
  ggtitle("Psychiatric patients")


#Data2 without psychiatric diagnoses
demographicplot_age_minuspsychiatric <- ggplot(data2.minus.psychiatric_diagnoses, aes(x= data2.minus.psychiatric_diagnoses$AGE, y = (((..count..)/sum(..count..)))*100))+
  geom_histogram(alpha=0.7, binwidth = 1, color= "black", fill= "skyblue") +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.line = element_line(linetype= "dotted"),
    panel.background = element_rect(fill= "white")
  ) +
  xlab("Age (Years)") +
  ylab("Frequency (%)")+
  scale_x_continuous(breaks = round(seq(min(0), max(100), by = 10),1))+
  coord_cartesian(ylim = c(0, 2.5))+
  ggtitle("Non-psychiatric patients")




##Sex
#Data1
data1.sex = data1 %>% 
  group_by(SEX) %>% 
  count() %>% 
  ungroup()%>% 
  arrange(desc(SEX)) %>%
  mutate(percentage = round(n/sum(n),4)*100,
         lab.pos = cumsum(percentage)-.5*percentage)

demographicplot_SEX_data1 <- ggplot(data = data1.sex, 
       aes(x = 2, y = percentage, fill = as.factor(SEX)))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  xlim(.2,2.5)+
scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("1" = "skyblue", "2" = "lightgreen"))


#Data2
data2.sex = data2 %>% 
  group_by(SEX) %>% 
  count() %>% 
  ungroup()%>% 
  arrange(desc(SEX)) %>%
  mutate(percentage = round(n/sum(n),4)*100,
         lab.pos = cumsum(percentage)-.5*percentage)

demographicplot_SEX_data2 <- ggplot(data = data2.sex, 
       aes(x = 2, y = percentage, fill = as.factor(SEX)))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  xlim(.2,2.5)+
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("1" = "skyblue", "2" = "lightgreen"))



#Pychiatric diagnoses
psychiatric_diagnoses.sex = psychiatric_diagnoses %>% 
  group_by(SEX) %>% 
  count() %>% 
  ungroup()%>% 
  arrange(desc(SEX)) %>%
  mutate(percentage = round(n/sum(n),4)*100,
         lab.pos = cumsum(percentage)-.5*percentage)

demographicplot_SEX_psychiatric <- ggplot(data = psychiatric_diagnoses.sex, 
       aes(x = 2, y = percentage, fill = as.factor(SEX)))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  xlim(.2,2.5)+
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("1" = "skyblue", "2" = "lightgreen"))



#Data2 minus psychiatric diagnoses
data2.minus.psychiatric_diagnoses.sex = data2.minus.psychiatric_diagnoses %>% 
  group_by(SEX) %>% 
  count() %>% 
  ungroup()%>% 
  arrange(desc(SEX)) %>%
  mutate(percentage = round(n/sum(n),4)*100,
         lab.pos = cumsum(percentage)-.5*percentage)

demographicplot_SEX_minuspsychiatric <- ggplot(data = data2.minus.psychiatric_diagnoses.sex, 
       aes(x = 2, y = percentage, fill = as.factor(SEX)))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  xlim(.2,2.5)+
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("1" = "skyblue", "2" = "lightgreen"))




##Race
#Data1
demographicplot_RACE_data1 <- data1 %>%
  ggplot(aes(x= as.factor(data1$RACE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("White", "Black", "American Indian/Alaskan", "Asian", "Native Hawaiian", "Other", "Multiple", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                    colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("RACE") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


######################Mess is here#############################

#Data2

#make a matrix to hold race percentages
Percentages_RACE_data2 <- as.data.frame(table(data2$RACE))
Percentages_RACE_data2$Freq.percent <- ((Percentages_RACE_data2$Freq/ sum(Percentages_RACE_data2$Freq)*100))
Percentages_RACE_data2$Freq.percent.rounded <- round(Percentages_RACE_data2$Freq.percent,digits=2)

demographicplot_RACE_data2 <-  ggplot(Percentages_RACE_data2, aes(x= as.factor(Var1), y = Freq.percent))+
  geom_bar(stat="identity", alpha=0.7, color= "black", fill= "skyblue")+
  geom_text(aes(label= Freq.percent.rounded), vjust=-1)+
scale_x_discrete(labels = c("White", "Black", "American Indian/Alaskan", "Asian", "Native Hawaiian", "Other", "Multiple", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("RACE") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60))


demographicplot_RACE_data2




demographicplot_RACE_data2 <- data2 %>%
  ggplot(aes(x= as.factor(data2$RACE), y = (((..count..) /sum(..count..))*100)))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  geom_text(aes(label=(((..count..) /sum(..count..))*100)), vjust=0)
 
  scale_x_discrete(labels = c("White", "Black", "American Indian/Alaskan", "Asian", "Native Hawaiian", "Other", "Multiple", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("RACE") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60))

######################Mess is here#############################


#Psychiatric diagnoses
demographicplot_RACE_psychiatric <- psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(psychiatric_diagnoses$RACE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("White", "Black", "American Indian/Alaskan", "Asian", "Native Hawaiian", "Other", "Multiple", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("RACE") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2 without psychiatric diagnoses
demographicplot_RACE_minuspsychiatric <- data2.minus.psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(data2.minus.psychiatric_diagnoses$RACE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("White", "Black", "American Indian/Alaskan", "Asian", "Native Hawaiian", "Other", "Multiple", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("RACE") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 








###Plot in one page
grid.arrange(demographicplot_age_data1, demographicplot_age_data2, demographicplot_age_psychiatric, demographicplot_age_minuspsychiatric,
             demographicplot_SEX_data1, demographicplot_SEX_data2, demographicplot_SEX_psychiatric, demographicplot_SEX_minuspsychiatric,
             demographicplot_RACE_data1, demographicplot_RACE_data2, demographicplot_RACE_psychiatric, demographicplot_RACE_minuspsychiatric,
             ncol=4)











