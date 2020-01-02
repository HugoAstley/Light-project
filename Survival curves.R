#####Survival analysis#####
install.packages("ggpubr")
library(ggpubr)


##If you're using this, you need to use data1. But don't use data1 blindly, you'll still have to remove some patients


##Column for defined event
#How they leave hospital
data1$HOME <- ifelse(data1$DISCSTAT == 1, "1", "0")
data1$AWOL <- ifelse(data1$DISCSTAT == 2, "1", "0")
data1$STC <- ifelse(data1$DISCSTAT == 3, "1", "0")
data1$LTC <- ifelse(data1$DISCSTAT == 4, "1", "0")
data1$death <- ifelse(data1$DISCSTAT == 6, "1", "0")


#making the 1's and 0's into numbers (not characters)
data1$HOME <- as.numeric(data1$HOME)
data1$AWOL <- as.numeric(data1$AWOL)
data1$STC <- as.numeric(data1$STC)
data1$LTC <- as.numeric(data1$LTC)
data1$death <- as.numeric(data1$death)


#Column for diagnoses flag
data1$psychiatric.diagnosis <- ifelse((data1$DIAGNOS1 > 289 & data1$DIAGNOS1 <320)| 
                                        (data1$DIAGNOS2 > 289 & data1$DIAGNOS2 <320)| 
                                        (data1$DIAGNOS3 > 289 & data1$DIAGNOS3 <320)|
                                        (data1$DIAGNOS4 > 289 & data1$DIAGNOS4 <320)|
                                        (data1$DIAGNOS5 > 289 & data1$DIAGNOS5 <320)|
                                        (data1$DIAGNOS6 > 289 & data1$DIAGNOS6 <320)|
                                        (data1$DIAGNOS7 > 289 & data1$DIAGNOS7 <320), "1", "0")

#Column for summer vs winter
data1$summer <- ifelse(data1$ADM_MON > 6 & data1$ADM_MON < 10, "1", "0")#make sure ADM_MON is numeric

##Developing survival data

#Psychiatric vs non psychiatric
survival.graph.HOME.psych.vs <- survfit(Surv(DOC, HOME)~ psychiatric.diagnosis, data=data1)
survival.graph.AWOL.psych.vs <- survfit(Surv(DOC, AWOL)~ psychiatric.diagnosis, data=data1)
survival.graph.STC.psych.vs <- survfit(Surv(DOC, STC)~ psychiatric.diagnosis, data=data1)
survival.graph.LTC.psych.vs <- survfit(Surv(DOC, LTC)~ psychiatric.diagnosis, data=data1)
survival.graph.death.psych.vs <- survfit(Surv(DOC, death)~ psychiatric.diagnosis, data=data1)

#Summer vs not summer
survival.graph.HOME <- survfit(Surv(DOC, HOME)~ summer, data=data1)
summary(survival.graph.HOME)
survival.graph.AWOL <- survfit(Surv(DOC, AWOL)~ summer, data=data1)
survival.graph.STC <- survfit(Surv(DOC, STC)~ summer, data=data1)
survival.graph.LTC <- survfit(Surv(DOC, LTC)~ summer, data=data1)
survival.graph.death <- survfit(Surv(DOC, death)~ summer, data=data1)






## List of ggsurvplots
require("survminer")
splots <- list()

#Plotting the survival curves
splots[[1]] <- ggsurvplot(survival.graph.HOME,
           data = data1,
           xlim = c(1, 15),
           main = "Survival curve",
           font.main = c(16, "bold", "darkblue"),
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           legend = c(0.2, 0.2),
           break.time.by = 5,
           palette = c("#E7B800", "#2E9FDF"),
           conf.int = T,
           pvalue= TRUE,
           risk.table.y.text.col = TRUE,
           risk.table = TRUE)

splots[[2]] <- ggsurvplot(survival.graph.HOME,
                     data = data1,
                     xlim = c(1, 15),
                     main = "Survival curve",
                     font.main = c(16, "bold", "darkblue"),
                     font.x = c(14, "bold.italic", "red"),
                     font.y = c(14, "bold.italic", "darkred"),
                     font.tickslab = c(12, "plain", "darkgreen"),
                     legend = c(0.2, 0.2),
                     break.time.by = 5,
                     palette = c("#E7B800", "#2E9FDF"),
                     conf.int = T,
                     pvalue= TRUE,
                     risk.table.y.text.col = TRUE,
                     risk.table = TRUE)




# Arrange multiple ggsurvplots and print the output
arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 2, nrow = 1, risk.table.height = 0.15)






#drop some n's
sum(data1$STC == "1")





##Summer JAS vs Winter DJF 
#Psychiatric vs nonpsychiatric
#Two datasets plotted on the same graph possible?


#Discharge groups

#Groups with different numbers of diagnoses

#Different numbers of procedures

#Different numbers of procedures