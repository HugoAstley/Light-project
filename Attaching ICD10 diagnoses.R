#####Adding in ICD 10 codes#####
#Remove the dashes
data2$DIAGNOS1 <- gsub("-", "", data2$DIAGNOS1)
data2$DIAGNOS2 <- gsub("-", "", data2$DIAGNOS2)
data2$DIAGNOS3 <- gsub("-", "", data2$DIAGNOS3)
data2$DIAGNOS4 <- gsub("-", "", data2$DIAGNOS4)
data2$DIAGNOS5 <- gsub("-", "", data2$DIAGNOS5)
data2$DIAGNOS6 <- gsub("-", "", data2$DIAGNOS6)
data2$DIAGNOS7 <- gsub("-", "", data2$DIAGNOS7)

#Remove the dashes again (for if they have double dashes)
data2$DIAGNOS1 <- gsub("-", "", data2$DIAGNOS1)
data2$DIAGNOS2 <- gsub("-", "", data2$DIAGNOS2)
data2$DIAGNOS3 <- gsub("-", "", data2$DIAGNOS3)
data2$DIAGNOS4 <- gsub("-", "", data2$DIAGNOS4)
data2$DIAGNOS5 <- gsub("-", "", data2$DIAGNOS5)
data2$DIAGNOS6 <- gsub("-", "", data2$DIAGNOS6)
data2$DIAGNOS7 <- gsub("-", "", data2$DIAGNOS7)

#As numeric
data2$DIAGNOS1 <- as.numeric(data2$DIAGNOS1)
data2$DIAGNOS2 <- as.numeric(data2$DIAGNOS2)
data2$DIAGNOS3 <- as.numeric(data2$DIAGNOS3)
data2$DIAGNOS4 <- as.numeric(data2$DIAGNOS4)
data2$DIAGNOS5 <- as.numeric(data2$DIAGNOS5)
data2$DIAGNOS6 <- as.numeric(data2$DIAGNOS6)
data2$DIAGNOS7 <- as.numeric(data2$DIAGNOS7)

#Make a duplicate of the ICD conversion table
icd9toicd10cmgem.long <- icd9toicd10cmgem



#In the conversion table, shorten the codes so that they are in groups
icd9toicd10cmgem$icd10cm <- substr(icd9toicd10cmgem$icd10cm,0,3)

#some ICD9 diagnoses have two or more ICD10 diagnoses. Group them into the same columns so you don't duplicate patients in these cases
icd9toicd10cmgem.condensed <- aggregate(icd9toicd10cmgem, icd9toicd10cmgem[1], function(x) paste0(unique(x)))


#left join 
data2 <- merge(x= data2, y= icd9toicd10cmgem.condensed[,2:3], by.x = "DIAGNOS1", by.y= "icd9cm", all.x= TRUE)
names(data2)[46]<-"DIAGNOS1.long.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.condensed[,2:3], by.x = "DIAGNOS2", by.y= "icd9cm", all.x= TRUE)
names(data2)[47]<-"DIAGNOS2.long.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.condensed[,2:3], by.x = "DIAGNOS3", by.y= "icd9cm", all.x= TRUE)
names(data2)[48]<-"DIAGNOS3.long.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.condensed[,2:3], by.x = "DIAGNOS4", by.y= "icd9cm", all.x= TRUE)
names(data2)[49]<-"DIAGNOS4.long.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.condensed[,2:3], by.x = "DIAGNOS5", by.y= "icd9cm", all.x= TRUE)
names(data2)[50]<-"DIAGNOS5.long.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.condensed[,2:3], by.x = "DIAGNOS6", by.y= "icd9cm", all.x= TRUE)
names(data2)[51]<-"DIAGNOS6.long.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.condensed[,2:3], by.x = "DIAGNOS7", by.y= "icd9cm", all.x= TRUE)
names(data2)[52]<-"DIAGNOS7.long.icd10"


#convert to character vectors to use filters later
data2$DIAGNOS1.long.icd10 <- as.character(data2$DIAGNOS1.long.icd10)
data2$DIAGNOS2.long.icd10 <- as.character(data2$DIAGNOS2.long.icd10)
data2$DIAGNOS3.long.icd10 <- as.character(data2$DIAGNOS3.long.icd10)
data2$DIAGNOS4.long.icd10 <- as.character(data2$DIAGNOS4.long.icd10)
data2$DIAGNOS5.long.icd10 <- as.character(data2$DIAGNOS5.long.icd10)
data2$DIAGNOS6.long.icd10 <- as.character(data2$DIAGNOS6.long.icd10)
data2$DIAGNOS7.long.icd10 <- as.character(data2$DIAGNOS7.long.icd10)

###Same but keeping the codes long for borderline classification
#some ICD9 diagnoses have two or more ICD10 diagnoses. Group them into the same columns so you don't duplicate patients in these cases
icd9toicd10cmgem.long.condensed <- aggregate(icd9toicd10cmgem.long, icd9toicd10cmgem.long[1], function(x) paste0(unique(x)))


#left join 
data2 <- merge(x= data2, y= icd9toicd10cmgem.long.condensed[,2:3], by.x = "DIAGNOS1", by.y= "icd9cm", all.x= TRUE)
names(data2)[53]<-"DIAGNOS1.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.long.condensed[,2:3], by.x = "DIAGNOS2", by.y= "icd9cm", all.x= TRUE)
names(data2)[54]<-"DIAGNOS2.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.long.condensed[,2:3], by.x = "DIAGNOS3", by.y= "icd9cm", all.x= TRUE)
names(data2)[55]<-"DIAGNOS3.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.long.condensed[,2:3], by.x = "DIAGNOS4", by.y= "icd9cm", all.x= TRUE)
names(data2)[56]<-"DIAGNOS4.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.long.condensed[,2:3], by.x = "DIAGNOS5", by.y= "icd9cm", all.x= TRUE)
names(data2)[57]<-"DIAGNOS5.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.long.condensed[,2:3], by.x = "DIAGNOS6", by.y= "icd9cm", all.x= TRUE)
names(data2)[58]<-"DIAGNOS6.icd10"

data2 <- merge(x= data2, y= icd9toicd10cmgem.long.condensed[,2:3], by.x = "DIAGNOS7", by.y= "icd9cm", all.x= TRUE)
names(data2)[59]<-"DIAGNOS7.icd10"


#convert to character vectors to use filters later
data2$DIAGNOS1.icd10 <- as.character(data2$DIAGNOS1.icd10)
data2$DIAGNOS2.icd10 <- as.character(data2$DIAGNOS2.icd10)
data2$DIAGNOS3.icd10 <- as.character(data2$DIAGNOS3.icd10)
data2$DIAGNOS4.icd10 <- as.character(data2$DIAGNOS4.icd10)
data2$DIAGNOS5.icd10 <- as.character(data2$DIAGNOS5.icd10)
data2$DIAGNOS6.icd10 <- as.character(data2$DIAGNOS6.icd10)
data2$DIAGNOS7.icd10 <- as.character(data2$DIAGNOS7.icd10)


##Simplfy the ICD9 diagnoses into their ICD 9 groups
data2$DIAGNOS1 <- substr(data2$DIAGNOS1,0,3)
data2$DIAGNOS2 <- substr(data2$DIAGNOS2,0,3)
data2$DIAGNOS3 <- substr(data2$DIAGNOS3,0,3)
data2$DIAGNOS4 <- substr(data2$DIAGNOS4,0,3)
data2$DIAGNOS5 <- substr(data2$DIAGNOS5,0,3)
data2$DIAGNOS6 <- substr(data2$DIAGNOS6,0,3)
data2$DIAGNOS7 <- substr(data2$DIAGNOS7,0,3)