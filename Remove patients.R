#####Removing#####
data2 <- data2%>%
  filter(AGEUNITS==1,
         LOSFLAG==1,
         DISCSTAT == 1,
         !ADM_MON == 99)

n <- data1%>%
  filter(AGEUNITS == 1,
         LOSFLAG == 1,
         ADM_MON != 99)

#####Showing how many have been knocked off#####
Age.in.years <- data1%>% filter(AGEUNITS==1)
More.than.a.day <- data1%>% filter(LOSFLAG==1)
Go.home <- data1%>% filter(DISCSTAT==1)
Known.ADM_MON <- data1%>% filter(!ADM_MON == 99)

exclusion.figures <- c(nrow(X24281_0002_Data), nrow(More.than.a.day), nrow(Go.home), nrow(Known.ADM_MON), nrow(data2))
exclusion.names <- c("Existing total", "Length of stay > 0", "Discharged home", "Known admission month", "New total")
exclusion.table <- rbind(exclusion.names, exclusion.figures)