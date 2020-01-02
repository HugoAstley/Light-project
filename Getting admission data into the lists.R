#######Find the admission data for each diagnoses######
#create the lists to fill
ICD_9_code_groups.REGION1 <- list()
ICD_9_code_groups.REGION2 <- list()
ICD_9_code_groups.REGION3 <- list()
ICD_9_code_groups.REGION4 <- list()

ICD_9_code_groups.admission.counts.REGION1 <- list()
ICD_9_code_groups.admission.counts.REGION2 <- list()
ICD_9_code_groups.admission.counts.REGION3 <- list()
ICD_9_code_groups.admission.counts.REGION4 <- list()

ICD_9_code_groups.admission.counts.REGION1.2 <- list()
ICD_9_code_groups.admission.counts.REGION3.4 <- list()
ICD_9_code_groups.admission.counts <- list()

#fill with patients from one of the regions
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.REGION1[[i]] <- ICD_9_code_groups[[i]] %>% filter(ICD_9_code_groups[[i]]$REGION == 1)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.REGION2[[i]] <- ICD_9_code_groups[[i]] %>% filter(ICD_9_code_groups[[i]]$REGION == 2)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.REGION3[[i]] <- ICD_9_code_groups[[i]] %>% filter(ICD_9_code_groups[[i]]$REGION == 3)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.REGION4[[i]] <- ICD_9_code_groups[[i]] %>% filter(ICD_9_code_groups[[i]]$REGION == 4)}


#Find the number of patients diagnosed in that month and region 
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION1[[i]] <- ICD_9_code_groups.REGION1[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION2[[i]] <- ICD_9_code_groups.REGION2[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION3[[i]] <- ICD_9_code_groups.REGION3[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION4[[i]] <- ICD_9_code_groups.REGION4[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}


##Combine the data on admission counts into one table
#label the regions before the merge
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION1[[i]]$REGION <- rep(1,nrow(ICD_9_code_groups.admission.counts.REGION1[[i]]))}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION2[[i]]$REGION <- rep(2,nrow(ICD_9_code_groups.admission.counts.REGION2[[i]]))}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION3[[i]]$REGION <- rep(3,nrow(ICD_9_code_groups.admission.counts.REGION3[[i]]))}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION4[[i]]$REGION <- rep(4,nrow(ICD_9_code_groups.admission.counts.REGION4[[i]]))}


#rbind the admission data together
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION1.2[[i]] <- rbind(y= ICD_9_code_groups.admission.counts.REGION1[[i]], x=ICD_9_code_groups.admission.counts.REGION2[[i]])}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts.REGION3.4[[i]] <- rbind(y= ICD_9_code_groups.admission.counts.REGION3[[i]], x=ICD_9_code_groups.admission.counts.REGION4[[i]])}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.admission.counts[[i]] <- rbind(y= ICD_9_code_groups.admission.counts.REGION1.2[[i]], x=ICD_9_code_groups.admission.counts.REGION3.4[[i]])}


#Join the admission data to the original datatables representing a patient groups
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]] <- ICD_9_code_groups[[i]] %>% left_join(ICD_9_code_groups.admission.counts[[i]], by= c("SVYEAR", "ADM_MON", "REGION"))}


save(ICD_9_code_groups, file = "ICD_9_code_groups.RData")



#####





#######Find the admission data for each diagnoses######
#create the lists to fill
primary.diagnosis.ICD_9_code_groups.REGION1 <- list()
primary.diagnosis.ICD_9_code_groups.REGION2 <- list()
primary.diagnosis.ICD_9_code_groups.REGION3 <- list()
primary.diagnosis.ICD_9_code_groups.REGION4 <- list()

primary.diagnosis.ICD_9_code_groups.admission.counts.REGION1 <- list()
primary.diagnosis.ICD_9_code_groups.admission.counts.REGION2 <- list()
primary.diagnosis.ICD_9_code_groups.admission.counts.REGION3 <- list()
primary.diagnosis.ICD_9_code_groups.admission.counts.REGION4 <- list()

primary.diagnosis.ICD_9_code_groups.admission.counts.REGION1.2 <- list()
primary.diagnosis.ICD_9_code_groups.admission.counts.REGION3.4 <- list()
primary.diagnosis.ICD_9_code_groups.admission.counts <- list()

#fill with patients from one of the regions
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]] <- primary.diagnosis.ICD_9_code_groups[[i]] %>% filter(primary.diagnosis.ICD_9_code_groups[[i]]$REGION == 1)}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.REGION2[[i]] <- primary.diagnosis.ICD_9_code_groups[[i]] %>% filter(primary.diagnosis.ICD_9_code_groups[[i]]$REGION == 2)}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.REGION3[[i]] <- primary.diagnosis.ICD_9_code_groups[[i]] %>% filter(primary.diagnosis.ICD_9_code_groups[[i]]$REGION == 3)}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.REGION4[[i]] <- primary.diagnosis.ICD_9_code_groups[[i]] %>% filter(primary.diagnosis.ICD_9_code_groups[[i]]$REGION == 4)}


#Find the number of patients diagnosed in that month and region 
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION1[[i]] <- primary.diagnosis.ICD_9_code_groups.REGION1[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION2[[i]] <- primary.diagnosis.ICD_9_code_groups.REGION2[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION3[[i]] <- primary.diagnosis.ICD_9_code_groups.REGION3[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION4[[i]] <- primary.diagnosis.ICD_9_code_groups.REGION4[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}



##Combine the data on admission counts into one table
#label the regions before the merge
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION1[[i]]$REGION <- rep(1,nrow(primary.diagnosis.ICD_9_code_groups.admission.counts.REGION1[[i]]))}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION2[[i]]$REGION <- rep(2,nrow(primary.diagnosis.ICD_9_code_groups.admission.counts.REGION2[[i]]))}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION3[[i]]$REGION <- rep(3,nrow(primary.diagnosis.ICD_9_code_groups.admission.counts.REGION3[[i]]))}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION4[[i]]$REGION <- rep(4,nrow(primary.diagnosis.ICD_9_code_groups.admission.counts.REGION4[[i]]))}


#rbind the admission data together
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION1.2[[i]] <- rbind(y= primary.diagnosis.ICD_9_code_groups.admission.counts.REGION1[[i]], x= primary.diagnosis.ICD_9_code_groups.admission.counts.REGION2[[i]])}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts.REGION3.4[[i]] <- rbind(y= primary.diagnosis.ICD_9_code_groups.admission.counts.REGION3[[i]], x= primary.diagnosis.ICD_9_code_groups.admission.counts.REGION4[[i]])}
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.admission.counts[[i]] <- rbind(y= primary.diagnosis.ICD_9_code_groups.admission.counts.REGION1.2[[i]], x= primary.diagnosis.ICD_9_code_groups.admission.counts.REGION3.4[[i]])}


#Join the admission data to the original datatables representing a patient groups
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups[[i]] <- primary.diagnosis.ICD_9_code_groups[[i]] %>% left_join(primary.diagnosis.ICD_9_code_groups.admission.counts[[i]], by= c("SVYEAR", "ADM_MON", "REGION"))}

save(primary.diagnosis.ICD_9_code_groups, file = "primary.diagnosis.ICD_9_code_groups.RData")

#########


#######Find the admission data for the procedure groups######
#create the lists to fill
procedure_groups.REGION1 <- list()
procedure_groups.REGION2 <- list()
procedure_groups.REGION3 <- list()
procedure_groups.REGION4 <- list()

procedure_groups.counts.REGION1 <- list()
procedure_groups.counts.REGION2 <- list()
procedure_groups.counts.REGION3 <- list()
procedure_groups.counts.REGION4 <- list()

procedure_groups.counts.REGION1.2 <- list()
procedure_groups.counts.REGION3.4 <- list()
procedure_groups.counts <- list()

#fill with patients from one of the regions
for(i in 1:length(procedure_groups)){procedure_groups.REGION1[[i]] <- procedure_groups[[i]] %>% filter(procedure_groups[[i]]$REGION == 1)}
for(i in 1:length(procedure_groups)){procedure_groups.REGION2[[i]] <- procedure_groups[[i]] %>% filter(procedure_groups[[i]]$REGION == 2)}
for(i in 1:length(procedure_groups)){procedure_groups.REGION3[[i]] <- procedure_groups[[i]] %>% filter(procedure_groups[[i]]$REGION == 3)}
for(i in 1:length(procedure_groups)){procedure_groups.REGION4[[i]] <- procedure_groups[[i]] %>% filter(procedure_groups[[i]]$REGION == 4)}


#Find the number of patients diagnosed in that month and region 
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION1[[i]] <- procedure_groups.REGION1[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION2[[i]] <- procedure_groups.REGION2[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION3[[i]] <- procedure_groups.REGION3[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION4[[i]] <- procedure_groups.REGION4[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}



##Combine the data on admission counts into one table
#label the regions before the merge
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION1[[i]]$REGION <- rep(1,nrow(procedure_groups.counts.REGION1[[i]]))}
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION2[[i]]$REGION <- rep(2,nrow(procedure_groups.counts.REGION2[[i]]))}
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION3[[i]]$REGION <- rep(3,nrow(procedure_groups.counts.REGION3[[i]]))}
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION4[[i]]$REGION <- rep(4,nrow(procedure_groups.counts.REGION4[[i]]))}


#rbind the admission data together
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION1.2[[i]] <- rbind(y= procedure_groups.counts.REGION1[[i]], x= procedure_groups.counts.REGION2[[i]])}
for(i in 1:length(procedure_groups)){procedure_groups.counts.REGION3.4[[i]] <- rbind(y= procedure_groups.counts.REGION3[[i]], x= procedure_groups.counts.REGION4[[i]])}
for(i in 1:length(procedure_groups)){procedure_groups.counts[[i]] <- rbind(y= procedure_groups.counts.REGION1.2[[i]], x= procedure_groups.counts.REGION3.4[[i]])}


#Join the admission data to the original datatables representing a patient groups
for(i in 1:length(procedure_groups)){procedure_groups[[i]] <- procedure_groups[[i]] %>% left_join(procedure_groups.counts[[i]], by= c("SVYEAR", "ADM_MON", "REGION"))}

save(procedure_groups, file = "procedure_groups.RData")




###################ICD 10 ##########################

###Primary diagnoses####

#######Find the admission data for each diagnoses######
#create the lists to fill
primary.diagnosis.ICD_10_code_groups.REGION1 <- list()
primary.diagnosis.ICD_10_code_groups.REGION2 <- list()
primary.diagnosis.ICD_10_code_groups.REGION3 <- list()
primary.diagnosis.ICD_10_code_groups.REGION4 <- list()

primary.diagnosis.ICD_10_code_groups.admission.counts.REGION1 <- list()
primary.diagnosis.ICD_10_code_groups.admission.counts.REGION2 <- list()
primary.diagnosis.ICD_10_code_groups.admission.counts.REGION3 <- list()
primary.diagnosis.ICD_10_code_groups.admission.counts.REGION4 <- list()

primary.diagnosis.ICD_10_code_groups.admission.counts.REGION1.2 <- list()
primary.diagnosis.ICD_10_code_groups.admission.counts.REGION3.4 <- list()
primary.diagnosis.ICD_10_code_groups.admission.counts <- list()

#fill with patients from one of the regions
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.REGION1[[i]] <- primary.diagnosis.ICD_10_code_groups[[i]] %>% filter(primary.diagnosis.ICD_10_code_groups[[i]]$REGION == 1)}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.REGION2[[i]] <- primary.diagnosis.ICD_10_code_groups[[i]] %>% filter(primary.diagnosis.ICD_10_code_groups[[i]]$REGION == 2)}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.REGION3[[i]] <- primary.diagnosis.ICD_10_code_groups[[i]] %>% filter(primary.diagnosis.ICD_10_code_groups[[i]]$REGION == 3)}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.REGION4[[i]] <- primary.diagnosis.ICD_10_code_groups[[i]] %>% filter(primary.diagnosis.ICD_10_code_groups[[i]]$REGION == 4)}


#Find the number of patients diagnosed in that month and region 
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION1[[i]] <- primary.diagnosis.ICD_10_code_groups.REGION1[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION2[[i]] <- primary.diagnosis.ICD_10_code_groups.REGION2[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION3[[i]] <- primary.diagnosis.ICD_10_code_groups.REGION3[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION4[[i]] <- primary.diagnosis.ICD_10_code_groups.REGION4[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}



##Combine the data on admission counts into one table
#label the regions before the merge
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION1[[i]]$REGION <- rep(1,nrow(primary.diagnosis.ICD_10_code_groups.admission.counts.REGION1[[i]]))}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION2[[i]]$REGION <- rep(2,nrow(primary.diagnosis.ICD_10_code_groups.admission.counts.REGION2[[i]]))}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION3[[i]]$REGION <- rep(3,nrow(primary.diagnosis.ICD_10_code_groups.admission.counts.REGION3[[i]]))}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION4[[i]]$REGION <- rep(4,nrow(primary.diagnosis.ICD_10_code_groups.admission.counts.REGION4[[i]]))}


#rbind the admission data together
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION1.2[[i]] <- rbind(y= primary.diagnosis.ICD_10_code_groups.admission.counts.REGION1[[i]], x= primary.diagnosis.ICD_10_code_groups.admission.counts.REGION2[[i]])}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts.REGION3.4[[i]] <- rbind(y= primary.diagnosis.ICD_10_code_groups.admission.counts.REGION3[[i]], x= primary.diagnosis.ICD_10_code_groups.admission.counts.REGION4[[i]])}
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups.admission.counts[[i]] <- rbind(y= primary.diagnosis.ICD_10_code_groups.admission.counts.REGION1.2[[i]], x= primary.diagnosis.ICD_10_code_groups.admission.counts.REGION3.4[[i]])}


#Join the admission data to the original datatables representing a patient groups
for(i in 1:length(ICD_10_code_groups)){primary.diagnosis.ICD_10_code_groups[[i]] <- primary.diagnosis.ICD_10_code_groups[[i]] %>% left_join(primary.diagnosis.ICD_10_code_groups.admission.counts[[i]], by= c("SVYEAR", "ADM_MON", "REGION"))}








#####All diagnoses#####
#######Find the admission data for each diagnoses######
#create the lists to fill
ICD_10_code_groups.REGION1 <- list()
ICD_10_code_groups.REGION2 <- list()
ICD_10_code_groups.REGION3 <- list()
ICD_10_code_groups.REGION4 <- list()

ICD_10_code_groups.admission.counts.REGION1 <- list()
ICD_10_code_groups.admission.counts.REGION2 <- list()
ICD_10_code_groups.admission.counts.REGION3 <- list()
ICD_10_code_groups.admission.counts.REGION4 <- list()

ICD_10_code_groups.admission.counts.REGION1.2 <- list()
ICD_10_code_groups.admission.counts.REGION3.4 <- list()
ICD_10_code_groups.admission.counts <- list()

#fill with patients from one of the regions
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.REGION1[[i]] <- ICD_10_code_groups[[i]] %>% filter(ICD_10_code_groups[[i]]$REGION == 1)}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.REGION2[[i]] <- ICD_10_code_groups[[i]] %>% filter(ICD_10_code_groups[[i]]$REGION == 2)}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.REGION3[[i]] <- ICD_10_code_groups[[i]] %>% filter(ICD_10_code_groups[[i]]$REGION == 3)}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.REGION4[[i]] <- ICD_10_code_groups[[i]] %>% filter(ICD_10_code_groups[[i]]$REGION == 4)}


#Find the number of patients diagnosed in that month and region 
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION1[[i]] <- ICD_10_code_groups.REGION1[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION2[[i]] <- ICD_10_code_groups.REGION2[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION3[[i]] <- ICD_10_code_groups.REGION3[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION4[[i]] <- ICD_10_code_groups.REGION4[[i]] %>% group_by(ADM_MON, SVYEAR) %>% summarise(n = sum(WEIGHT))}


##Combine the data on admission counts into one table
#label the regions before the merge
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION1[[i]]$REGION <- rep(1,nrow(ICD_10_code_groups.admission.counts.REGION1[[i]]))}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION2[[i]]$REGION <- rep(2,nrow(ICD_10_code_groups.admission.counts.REGION2[[i]]))}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION3[[i]]$REGION <- rep(3,nrow(ICD_10_code_groups.admission.counts.REGION3[[i]]))}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION4[[i]]$REGION <- rep(4,nrow(ICD_10_code_groups.admission.counts.REGION4[[i]]))}


#rbind the admission data together
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION1.2[[i]] <- rbind(y= ICD_10_code_groups.admission.counts.REGION1[[i]], x=ICD_10_code_groups.admission.counts.REGION2[[i]])}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts.REGION3.4[[i]] <- rbind(y= ICD_10_code_groups.admission.counts.REGION3[[i]], x=ICD_10_code_groups.admission.counts.REGION4[[i]])}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups.admission.counts[[i]] <- rbind(y= ICD_10_code_groups.admission.counts.REGION1.2[[i]], x=ICD_10_code_groups.admission.counts.REGION3.4[[i]])}


#Join the admission data to the original datatables representing a patient groups
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups[[i]] <- ICD_10_code_groups[[i]] %>% left_join(ICD_10_code_groups.admission.counts[[i]], by= c("SVYEAR", "ADM_MON", "REGION"))}

save(ICD_10_code_groups, file= "ICD_10_code_groups.RData")
save(primary.diagnosis.ICD_10_code_groups, file= "primary.diagnosis.ICD_10_code_groups.Rdata")
