##################### Pearson's Correlation coefficient###############

#All patients
#All-psychiatric patients
#Psychiatric patients
#Difference between all-psychiatric patients and psychiatric patients

#Correlation between light level and admission time for all patients
cor.test(data= data2.REGION1, data2.REGION1$DOC, data2.REGION1$Average)
cor.test(data= data2.REGION2, data2.REGION2$DOC, data2.REGION2$Average)
cor.test(data= data2.REGION3, data2.REGION3$DOC, data2.REGION3$Average)
cor.test(data= data2.REGION4, data2.REGION4$DOC, data2.REGION4$Average)

#Correlation between light level and DOC for all-psychiatric patients
cor.test(data= data2.minus.psychiatric_diagnoses.REGION1, data2.minus.psychiatric_diagnoses.REGION1$DOC, data2.minus.psychiatric_diagnoses.REGION1$Average)
cor.test(data= data2.minus.psychiatric_diagnoses.REGION2, data2.minus.psychiatric_diagnoses.REGION2$DOC, data2.minus.psychiatric_diagnoses.REGION2$Average)
cor.test(data= data2.minus.psychiatric_diagnoses.REGION3, data2.minus.psychiatric_diagnoses.REGION3$DOC, data2.minus.psychiatric_diagnoses.REGION3$Average)
cor.test(data= data2.minus.psychiatric_diagnoses.REGION4, data2.minus.psychiatric_diagnoses.REGION4$DOC, data2.minus.psychiatric_diagnoses.REGION4$Average)

#Correlation between light level and admission time for psychiatric patients
cor.test(data= psychiatric_diagnoses.REGION1, psychiatric_diagnoses.REGION1$DOC, psychiatric_diagnoses.REGION1$Average)
cor.test(data= psychiatric_diagnoses.REGION2, psychiatric_diagnoses.REGION2$DOC, psychiatric_diagnoses.REGION2$Average)
cor.test(data= psychiatric_diagnoses.REGION3, psychiatric_diagnoses.REGION3$DOC, psychiatric_diagnoses.REGION3$Average)
cor.test(data= psychiatric_diagnoses.REGION4, psychiatric_diagnoses.REGION4$DOC, psychiatric_diagnoses.REGION4$Average)


#Correlation coefficient to compare the differences
cor.test(data= Compare_means.light_region1, Compare_means.light_region1$Average, Compare_means.light_region1$difference_means)
cor.test(data= Compare_means.light_region2, Compare_means.light_region2$Average, Compare_means.light_region2$difference_means)
cor.test(data= Compare_means.light_region3, Compare_means.light_region3$Average, Compare_means.light_region3$difference_means)
cor.test(data= Compare_means.light_region4, Compare_means.light_region4$Average, Compare_means.light_region4$difference_means)
