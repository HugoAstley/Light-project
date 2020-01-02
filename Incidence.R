#Incidence
########Do the diagnoses change over the year? Psychiatric diagnoses#######


#new object: patients with a psychiatric diagnos1
psychiatric_diagnos1 <-
  data2 %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <320))


#Filtering to psychiatric diagnosis (based on diagnosis1)
#For the slice: insert 652656:947308 if using the whole table. 205:257 for 1000
table2 <- X24281_0002_Data[,]
sort1.table2 <- table2[order(table2$DIAGNOS1), ]
psychiatric_diagnoses <- sort1.table2 %>% slice(652656:947308)


##for january


#number of patients in each group: for january
psychiatric_diagnoses.january <-
  psychiatric_diagnos1 %>%
  filter(ADM_MON == 1)

DIAGNOS1.tally.january <-
  psychiatric_diagnoses.january %>%
  group_by(DIAGNOS1) %>%
  summarise(count=n())

#group the patients
total.in.group.psychiatric.january <- aggregate(x = DIAGNOS1.tally.january$count, 
                                                by = list(DIAGNOS1 = substr(DIAGNOS1.tally.january$DIAGNOS1,0,3)), 
                                                FUN = sum)

#order the groups of patients in decending order
total.in.group.psychiatric.january <- total.in.group.psychiatric.january[order(total.in.group.psychiatric.january$x, decreasing = TRUE),]

#remove groups with few patients
total.in.group.psychiatric.january <-
  total.in.group.psychiatric.january%>%
  filter(x> 1000)


##for july

#number of patients in each group: for july
psychiatric_diagnoses.july <-
  psychiatric_diagnos1 %>%
  filter(ADM_MON == 7)

DIAGNOS1.tally.july <-
  psychiatric_diagnoses.july %>%
  group_by(DIAGNOS1) %>%
  summarise(count=n())

#group the patients
total.in.group.psychiatric.july <- aggregate(x = DIAGNOS1.tally.july$count, 
                                             by = list(DIAGNOS1 = substr(DIAGNOS1.tally.july$DIAGNOS1,0,3)), 
                                             FUN = sum)

#order the groups of patients in decending order
total.in.group.psychiatric.july <- total.in.group.psychiatric.july[order(total.in.group.psychiatric.july$x, decreasing = TRUE),]  


#remove groups with few patients
total.in.group.psychiatric.july <-
  total.in.group.psychiatric.july%>%
  filter(x> 1000)

###Bar graphs###

plot(ggplot(total.in.group.psychiatric.january, aes(x= reorder(total.in.group.psychiatric.january$DIAGNOS1, -total.in.group.psychiatric.january$x), y=total.in.group.psychiatric.january$x))+
       geom_bar(stat="identity", width=1))

plot(ggplot(total.in.group.psychiatric.july, aes(x=reorder(total.in.group.psychiatric.july$DIAGNOS1, -total.in.group.psychiatric.july$x), y=total.in.group.psychiatric.july$x))+
       geom_bar(stat="identity", width=1))



###pie charts###
#For psychiatric diagnoses, plot a bar graph and then turn it into a pie chart
total.in.group.psychiatric.january.pie <- ggplot(total.in.group.psychiatric.january, aes(x="", y=total.in.group.psychiatric.january$x, fill= as.factor(total.in.group.psychiatric.january$DIAGNOS1)))+
  geom_bar(stat="identity", width=1)
total.in.group.psychiatric.january.pie <- total.in.group.psychiatric.january.pie + coord_polar("y", start=0)

total.in.group.psychiatric.july.pie <- ggplot(total.in.group.psychiatric.july, aes(x="", y=total.in.group.psychiatric.july$x, fill= as.factor(total.in.group.psychiatric.july$DIAGNOS1)))+
  geom_bar(stat="identity", width=1)
total.in.group.psychiatric.july.pie <- total.in.group.psychiatric.july.pie + coord_polar("y", start=0)





#####Do the diagnoses change over the year? All diagnoses#####
##for january

#number of patients in each group: for january
data2.january <-
  data2 %>%
  filter(ADM_MON == 1)

DIAGNOS1.tally.january <-
  data2.january %>%
  group_by(DIAGNOS1) %>%
  summarise(count=n())

#group the patients
total.in.group.january <- aggregate(x = DIAGNOS1.tally.january$count, 
                                    by = list(DIAGNOS1 = substr(DIAGNOS1.tally.january$DIAGNOS1,0,3)), 
                                    FUN = sum)

#order the groups of patients in decending order
total.in.group.january <- total.in.group.january[order(total.in.group.january$x, decreasing = TRUE),]

#remove groups with few patients
total.in.group.january <-
  total.in.group.january%>%
  filter(x> 4000)


##for july
#number of patients in each group: for july
data2.july <-
  data2 %>%
  filter(ADM_MON == 7)

DIAGNOS1.tally.july <-
  data2.july %>%
  group_by(DIAGNOS1) %>%
  summarise(count=n())

#group the patients
total.in.group.july <- aggregate(x = DIAGNOS1.tally.july$count, 
                                 by = list(DIAGNOS1 = substr(DIAGNOS1.tally.july$DIAGNOS1,0,3)), 
                                 FUN = sum)

#order the groups of patients in decending order
total.in.group.july<- total.in.group.july[order(total.in.group.july$x, decreasing = TRUE),]

#remove groups with few patients
total.in.group.july <-
  total.in.group.july%>%
  filter(x> 4000)


###bar graphs###
plot(ggplot(total.in.group.january, aes(x= reorder(total.in.group.january$DIAGNOS1, -total.in.group.january$x), y=total.in.group.january$x))+
       geom_bar(stat="identity", width=1))

plot(ggplot(total.in.group.july, aes(x=reorder(total.in.group.july$DIAGNOS1, -total.in.group.july$x), y=total.in.group.july$x))+
       geom_bar(stat="identity", width=1))




