#Include patients that have been diagnosed with at least one psychiatric disorder into an object called 'psychiatric_diagnoses'
psychiatric_diagnoses <-
  data2 %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <320)| 
           (DIAGNOS2 > 289 & DIAGNOS2 <320)| 
           (DIAGNOS3 > 289 & DIAGNOS3 <320)|
           (DIAGNOS4 > 289 & DIAGNOS4 <320)|
           (DIAGNOS5 > 289 & DIAGNOS5 <320)|
           (DIAGNOS6 > 289 & DIAGNOS6 <320)|
           (DIAGNOS7 > 289 & DIAGNOS7 <320))

#Group the remaining patients into a different dataframe
data2.minus.psychiatric_diagnoses <-
  setdiff(data2, psychiatric_diagnoses)

write.csv(psychiatric_diagnoses, "psychiatric_diagnoses")
write.csv(data2.minus.psychiatric_diagnoses, "data2.minus.psychiatric_diagnoses")
save(ICD_9_code_groups, file="ICD_9_code_groups.RData")
save(ICD_9_code_groups_psychiatric, file="ICD_9_code_groups_psychiatric.RData")


save(ICD_9_code_groups_skin, file="ICD_9_code_groups_skin.RData")
save(ICD_9_code_groups_genitourinal, file="ICD_9_code_groups_genitourinal.RData")
save(ICD_9_code_groups_circulatory, file="ICD_9_code_groups_circulatory.RData")
save(ICD_9_code_groups_genitourinal, file="ICD_9_code_groups_genitourinal.RData")
save(ICD_9_code_groups_respiratory, file="ICD_9_code_groups_respiratory.RData")


#####ICD 9 code groups#####
#######Prepare data on different diagnoses#####
infectious_and_parasitic_diseases <-
  data2 %>%
  filter((DIAGNOS1 > 000 & DIAGNOS1 <140)| 
           (DIAGNOS2 > 000 & DIAGNOS2 <140)| 
           (DIAGNOS3 > 000 & DIAGNOS3 <140)|
           (DIAGNOS4 > 000 & DIAGNOS4 <140)|
           (DIAGNOS5 > 000 & DIAGNOS5 <140)|
           (DIAGNOS6 > 000 & DIAGNOS6 <140)|
           (DIAGNOS7 > 000 & DIAGNOS7 <140))

neoplasms <-
  data2 %>%
  filter((DIAGNOS1 > 139 & DIAGNOS1 <240)| 
           (DIAGNOS2 > 139 & DIAGNOS2 <240)| 
           (DIAGNOS3 > 139 & DIAGNOS3 <240)|
           (DIAGNOS4 > 139 & DIAGNOS4 <240)|
           (DIAGNOS5 > 139 & DIAGNOS5 <240)|
           (DIAGNOS6 > 139 & DIAGNOS6 <240)|
           (DIAGNOS7 > 139 & DIAGNOS7 <240))

endocrine_metabolic_immunity_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 239 & DIAGNOS1 <280)| 
           (DIAGNOS2 > 239 & DIAGNOS2 <280)| 
           (DIAGNOS3 > 239 & DIAGNOS3 <280)|
           (DIAGNOS4 > 239 & DIAGNOS4 <280)|
           (DIAGNOS5 > 239 & DIAGNOS5 <280)|
           (DIAGNOS6 > 239 & DIAGNOS6 <280)|
           (DIAGNOS7 > 239 & DIAGNOS7 <280))

diseases_of_blood <-
  data2 %>%
  filter((DIAGNOS1 > 279 & DIAGNOS1 <290)| 
           (DIAGNOS2 > 279 & DIAGNOS2 <290)| 
           (DIAGNOS3 > 279 & DIAGNOS3 <290)|
           (DIAGNOS4 > 279 & DIAGNOS4 <290)|
           (DIAGNOS5 > 279 & DIAGNOS5 <290)|
           (DIAGNOS6 > 279 & DIAGNOS6 <290)|
           (DIAGNOS7 > 279 & DIAGNOS7 <290))

psychiatric_diagnoses <-
  data2 %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <320)| 
           (DIAGNOS2 > 289 & DIAGNOS2 <320)| 
           (DIAGNOS3 > 289 & DIAGNOS3 <320)|
           (DIAGNOS4 > 289 & DIAGNOS4 <320)|
           (DIAGNOS5 > 289 & DIAGNOS5 <320)|
           (DIAGNOS6 > 289 & DIAGNOS6 <320)|
           (DIAGNOS7 > 289 & DIAGNOS7 <320))

diseases_of_nervous_system <-
  data2 %>%
  filter((DIAGNOS1 > 319 & DIAGNOS1 <390)| 
           (DIAGNOS2 > 319 & DIAGNOS2 <390)| 
           (DIAGNOS3 > 319 & DIAGNOS3 <390)|
           (DIAGNOS4 > 319 & DIAGNOS4 <390)|
           (DIAGNOS5 > 319 & DIAGNOS5 <390)|
           (DIAGNOS6 > 319 & DIAGNOS6 <390)|
           (DIAGNOS7 > 319 & DIAGNOS7 <390))

diseases_of_circulatory_system <-
  data2 %>%
  filter((DIAGNOS1 > 389 & DIAGNOS1 <460)| 
           (DIAGNOS2 > 389 & DIAGNOS2 <460)| 
           (DIAGNOS3 > 389 & DIAGNOS3 <460)|
           (DIAGNOS4 > 389 & DIAGNOS4 <460)|
           (DIAGNOS5 > 389 & DIAGNOS5 <460)|
           (DIAGNOS6 > 389 & DIAGNOS6 <460)|
           (DIAGNOS7 > 389 & DIAGNOS7 <460))

diseases_of_respiratory_system <-
  data2 %>%
  filter((DIAGNOS1 > 459 & DIAGNOS1 <520)| 
           (DIAGNOS2 > 459 & DIAGNOS2 <520)| 
           (DIAGNOS3 > 459 & DIAGNOS3 <520)|
           (DIAGNOS4 > 459 & DIAGNOS4 <520)|
           (DIAGNOS5 > 459 & DIAGNOS5 <520)|
           (DIAGNOS6 > 459 & DIAGNOS6 <520)|
           (DIAGNOS7 > 459 & DIAGNOS7 <520))

diseases_of_digestive_system <-
  data2 %>%
  filter((DIAGNOS1 > 519 & DIAGNOS1 <580)| 
           (DIAGNOS2 > 519 & DIAGNOS2 <580)| 
           (DIAGNOS3 > 519 & DIAGNOS3 <580)|
           (DIAGNOS4 > 519 & DIAGNOS4 <580)|
           (DIAGNOS5 > 519 & DIAGNOS5 <580)|
           (DIAGNOS6 > 519 & DIAGNOS6 <580)|
           (DIAGNOS7 > 519 & DIAGNOS7 <580))

diseases_of_genitourinary_system <-
  data2 %>%
  filter((DIAGNOS1 > 579 & DIAGNOS1 <630)| 
           (DIAGNOS2 > 579 & DIAGNOS2 <630)| 
           (DIAGNOS3 > 579 & DIAGNOS3 <630)|
           (DIAGNOS4 > 579 & DIAGNOS4 <630)|
           (DIAGNOS5 > 579 & DIAGNOS5 <630)|
           (DIAGNOS6 > 579 & DIAGNOS6 <630)|
           (DIAGNOS7 > 579 & DIAGNOS7 <630))

complicatoins_of_pregnancy_and_childbirth <-
  data2 %>%
  filter((DIAGNOS1 > 629 & DIAGNOS1 <680)| 
           (DIAGNOS2 > 629 & DIAGNOS2 <680)| 
           (DIAGNOS3 > 629 & DIAGNOS3 <680)|
           (DIAGNOS4 > 629 & DIAGNOS4 <680)|
           (DIAGNOS5 > 629 & DIAGNOS5 <680)|
           (DIAGNOS6 > 629 & DIAGNOS6 <680)|
           (DIAGNOS7 > 629 & DIAGNOS7 <680))

diseases_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 679 & DIAGNOS1 <710)| 
           (DIAGNOS2 > 679 & DIAGNOS2 <710)| 
           (DIAGNOS3 > 679 & DIAGNOS3 <710)|
           (DIAGNOS4 > 679 & DIAGNOS4 <710)|
           (DIAGNOS5 > 679 & DIAGNOS5 <710)|
           (DIAGNOS6 > 679 & DIAGNOS6 <710)|
           (DIAGNOS7 > 679 & DIAGNOS7 <710))

diseases_of_musculoskeletal_and_connective_tissue <-
  data2 %>%
  filter((DIAGNOS1 > 709 & DIAGNOS1 <740)| 
           (DIAGNOS2 > 709 & DIAGNOS2 <740)| 
           (DIAGNOS3 > 709 & DIAGNOS3 <740)|
           (DIAGNOS4 > 709 & DIAGNOS4 <740)|
           (DIAGNOS5 > 709 & DIAGNOS5 <740)|
           (DIAGNOS6 > 709 & DIAGNOS6 <740)|
           (DIAGNOS7 > 709 & DIAGNOS7 <740))

congenital_anomalies <-
  data2 %>%
  filter((DIAGNOS1 > 739 & DIAGNOS1 <760)| 
           (DIAGNOS2 > 739 & DIAGNOS2 <760)| 
           (DIAGNOS3 > 739 & DIAGNOS3 <760)|
           (DIAGNOS4 > 739 & DIAGNOS4 <760)|
           (DIAGNOS5 > 739 & DIAGNOS5 <760)|
           (DIAGNOS6 > 739 & DIAGNOS6 <760)|
           (DIAGNOS7 > 739 & DIAGNOS7 <760))

conditions_of_perinatal_period <-
  data2 %>%
  filter((DIAGNOS1 > 759 & DIAGNOS1 <780)| 
           (DIAGNOS2 > 759 & DIAGNOS2 <780)| 
           (DIAGNOS3 > 759 & DIAGNOS3 <780)|
           (DIAGNOS4 > 759 & DIAGNOS4 <780)|
           (DIAGNOS5 > 759 & DIAGNOS5 <780)|
           (DIAGNOS6 > 759 & DIAGNOS6 <780)|
           (DIAGNOS7 > 759 & DIAGNOS7 <780))

ill_defined_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 779 & DIAGNOS1 <800)| 
           (DIAGNOS2 > 779 & DIAGNOS2 <800)| 
           (DIAGNOS3 > 779 & DIAGNOS3 <800)|
           (DIAGNOS4 > 779 & DIAGNOS4 <800)|
           (DIAGNOS5 > 779 & DIAGNOS5 <800)|
           (DIAGNOS6 > 779 & DIAGNOS6 <800)|
           (DIAGNOS7 > 779 & DIAGNOS7 <800))


injury_and_poisoning <-
  data2 %>%
  filter((DIAGNOS1 > 799 & DIAGNOS1 <= 999)| 
           (DIAGNOS2 > 799 & DIAGNOS2 <= 999)| 
           (DIAGNOS3 > 799 & DIAGNOS3 <= 999)|
           (DIAGNOS4 > 799 & DIAGNOS4 <= 999)|
           (DIAGNOS5 > 799 & DIAGNOS5 <= 999)|
           (DIAGNOS6 > 799 & DIAGNOS6 <= 999)|
           (DIAGNOS7 > 799 & DIAGNOS7 <= 999))

##
#Being more specific on the psychiatric diagnoses
Organic_psychotic_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <295)| 
           (DIAGNOS2 > 289 & DIAGNOS2 <295)| 
           (DIAGNOS3 > 289 & DIAGNOS3 <295)|
           (DIAGNOS4 > 289 & DIAGNOS4 <295)|
           (DIAGNOS5 > 289 & DIAGNOS5 <295)|
           (DIAGNOS6 > 289 & DIAGNOS6 <295)|
           (DIAGNOS7 > 289 & DIAGNOS7 <295))


Senile_and_presenile_organic_psychotic_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <291)| 
           (DIAGNOS2 > 289 & DIAGNOS2 <291)| 
           (DIAGNOS3 > 289 & DIAGNOS3 <291)|
           (DIAGNOS4 > 289 & DIAGNOS4 <291)|
           (DIAGNOS5 > 289 & DIAGNOS5 <291)|
           (DIAGNOS6 > 289 & DIAGNOS6 <291)|
           (DIAGNOS7 > 289 & DIAGNOS7 <291))



Alcoholic_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 290 & DIAGNOS1 <292)| 
           (DIAGNOS2 > 290 & DIAGNOS2 <292)| 
           (DIAGNOS3 > 290 & DIAGNOS3 <292)|
           (DIAGNOS4 > 290 & DIAGNOS4 <292)|
           (DIAGNOS5 > 290 & DIAGNOS5 <292)|
           (DIAGNOS6 > 290 & DIAGNOS6 <292)|
           (DIAGNOS7 > 290 & DIAGNOS7 <292))



Drug_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 291 & DIAGNOS1 <293)| 
           (DIAGNOS2 > 291 & DIAGNOS2 <293)| 
           (DIAGNOS3 > 291 & DIAGNOS3 <293)|
           (DIAGNOS4 > 291 & DIAGNOS4 <293)|
           (DIAGNOS5 > 291 & DIAGNOS5 <293)|
           (DIAGNOS6 > 291 & DIAGNOS6 <293)|
           (DIAGNOS7 > 291 & DIAGNOS7 <293))


Transient_organic_psychotic_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 292 & DIAGNOS1 <294)| 
           (DIAGNOS2 > 292 & DIAGNOS2 <294)| 
           (DIAGNOS3 > 292 & DIAGNOS3 <294)|
           (DIAGNOS4 > 292 & DIAGNOS4 <294)|
           (DIAGNOS5 > 292 & DIAGNOS5 <294)|
           (DIAGNOS6 > 292 & DIAGNOS6 <294)|
           (DIAGNOS7 > 292 & DIAGNOS7 <294))


Other_organic_psychotic_conditions_chronic <-
  data2 %>%
  filter((DIAGNOS1 > 293 & DIAGNOS1 <295)| 
           (DIAGNOS2 > 293 & DIAGNOS2 <295)| 
           (DIAGNOS3 > 293 & DIAGNOS3 <295)|
           (DIAGNOS4 > 293 & DIAGNOS4 <295)|
           (DIAGNOS5 > 293 & DIAGNOS5 <295)|
           (DIAGNOS6 > 293 & DIAGNOS6 <295)|
           (DIAGNOS7 > 293 & DIAGNOS7 <295))


Other_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 294 & DIAGNOS1 <300)| 
           (DIAGNOS2 > 294 & DIAGNOS2 <300)| 
           (DIAGNOS3 > 294 & DIAGNOS3 <300)|
           (DIAGNOS4 > 294 & DIAGNOS4 <300)|
           (DIAGNOS5 > 294 & DIAGNOS5 <300)|
           (DIAGNOS6 > 294 & DIAGNOS6 <300)|
           (DIAGNOS7 > 294 & DIAGNOS7 <300))



Schizophrenic_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 294 & DIAGNOS1 <296)| 
           (DIAGNOS2 > 294 & DIAGNOS2 <296)| 
           (DIAGNOS3 > 294 & DIAGNOS3 <296)|
           (DIAGNOS4 > 294 & DIAGNOS4 <296)|
           (DIAGNOS5 > 294 & DIAGNOS5 <296)|
           (DIAGNOS6 > 294 & DIAGNOS6 <296)|
           (DIAGNOS7 > 294 & DIAGNOS7 <296))




Affective_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 295 & DIAGNOS1 <297)| 
           (DIAGNOS2 > 295 & DIAGNOS2 <297)| 
           (DIAGNOS3 > 295 & DIAGNOS3 <297)|
           (DIAGNOS4 > 295 & DIAGNOS4 <297)|
           (DIAGNOS5 > 295 & DIAGNOS5 <297)|
           (DIAGNOS6 > 295 & DIAGNOS6 <297)|
           (DIAGNOS7 > 295 & DIAGNOS7 <297))


Paranoid_states <-
  data2 %>%
  filter((DIAGNOS1 > 296 & DIAGNOS1 <298)| 
           (DIAGNOS2 > 296 & DIAGNOS2 <298)| 
           (DIAGNOS3 > 296 & DIAGNOS3 <298)|
           (DIAGNOS4 > 296 & DIAGNOS4 <298)|
           (DIAGNOS5 > 296 & DIAGNOS5 <298)|
           (DIAGNOS6 > 296 & DIAGNOS6 <298)|
           (DIAGNOS7 > 296 & DIAGNOS7 <298))


Other_nonorganic_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 297 & DIAGNOS1 <299)| 
           (DIAGNOS2 > 297 & DIAGNOS2 <299)| 
           (DIAGNOS3 > 297 & DIAGNOS3 <299)|
           (DIAGNOS4 > 297 & DIAGNOS4 <299)|
           (DIAGNOS5 > 297 & DIAGNOS5 <299)|
           (DIAGNOS6 > 297 & DIAGNOS6 <299)|
           (DIAGNOS7 > 297 & DIAGNOS7 <299))



Psychoses_with_origin_specific_to_childhood <-
  data2 %>%
  filter((DIAGNOS1 > 298 & DIAGNOS1 <300)| 
           (DIAGNOS2 > 298 & DIAGNOS2 <300)| 
           (DIAGNOS3 > 298 & DIAGNOS3 <300)|
           (DIAGNOS4 > 298 & DIAGNOS4 <300)|
           (DIAGNOS5 > 298 & DIAGNOS5 <300)|
           (DIAGNOS6 > 298 & DIAGNOS6 <300)|
           (DIAGNOS7 > 298 & DIAGNOS7 <300))




Neurotic_disorders_personality_disorders_and_nonpsychotic_mental_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 299 & DIAGNOS1 <317)| 
           (DIAGNOS2 > 299 & DIAGNOS2 <317)| 
           (DIAGNOS3 > 299 & DIAGNOS3 <317)|
           (DIAGNOS4 > 299 & DIAGNOS4 <317)|
           (DIAGNOS5 > 299 & DIAGNOS5 <317)|
           (DIAGNOS6 > 299 & DIAGNOS6 <317)|
           (DIAGNOS7 > 299 & DIAGNOS7 <317))


Neurotic_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 299 & DIAGNOS1 <301)| 
           (DIAGNOS2 > 299 & DIAGNOS2 <301)| 
           (DIAGNOS3 > 299 & DIAGNOS3 <301)|
           (DIAGNOS4 > 299 & DIAGNOS4 <301)|
           (DIAGNOS5 > 299 & DIAGNOS5 <301)|
           (DIAGNOS6 > 299 & DIAGNOS6 <301)|
           (DIAGNOS7 > 299 & DIAGNOS7 <301))


Personality_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 300 & DIAGNOS1 <302)| 
           (DIAGNOS2 > 300 & DIAGNOS2 <302)| 
           (DIAGNOS3 > 300 & DIAGNOS3 <302)|
           (DIAGNOS4 > 300 & DIAGNOS4 <302)|
           (DIAGNOS5 > 300 & DIAGNOS5 <302)|
           (DIAGNOS6 > 300 & DIAGNOS6 <302)|
           (DIAGNOS7 > 300 & DIAGNOS7 <302))

Sexual_deviations <-
  data2 %>%
  filter((DIAGNOS1 > 301 & DIAGNOS1 <303)| 
           (DIAGNOS2 > 301 & DIAGNOS2 <303)| 
           (DIAGNOS3 > 301 & DIAGNOS3 <303)|
           (DIAGNOS4 > 301 & DIAGNOS4 <303)|
           (DIAGNOS5 > 301 & DIAGNOS5 <303)|
           (DIAGNOS6 > 301 & DIAGNOS6 <303)|
           (DIAGNOS7 > 301 & DIAGNOS7 <303))


Psychoactive_substance <-
  data2 %>%
  filter((DIAGNOS1 > 302 & DIAGNOS1 <306)| 
           (DIAGNOS2 > 302 & DIAGNOS2 <306)| 
           (DIAGNOS3 > 302 & DIAGNOS3 <306)|
           (DIAGNOS4 > 302 & DIAGNOS4 <306)|
           (DIAGNOS5 > 302 & DIAGNOS5 <306)|
           (DIAGNOS6 > 302 & DIAGNOS6 <306)|
           (DIAGNOS7 > 302 & DIAGNOS7 <306))

Alcohol_dependence_syndrome <-
  data2 %>%
  filter((DIAGNOS1 > 302 & DIAGNOS1 <304)| 
           (DIAGNOS2 > 302 & DIAGNOS2 <304)| 
           (DIAGNOS3 > 302 & DIAGNOS3 <304)|
           (DIAGNOS4 > 302 & DIAGNOS4 <304)|
           (DIAGNOS5 > 302 & DIAGNOS5 <304)|
           (DIAGNOS6 > 302 & DIAGNOS6 <304)|
           (DIAGNOS7 > 302 & DIAGNOS7 <304))


Drug_dependence <-
  data2 %>%
  filter((DIAGNOS1 > 303 & DIAGNOS1 <305)| 
           (DIAGNOS2 > 303 & DIAGNOS2 <305)| 
           (DIAGNOS3 > 303 & DIAGNOS3 <305)|
           (DIAGNOS4 > 303 & DIAGNOS4 <305)|
           (DIAGNOS5 > 303 & DIAGNOS5 <305)|
           (DIAGNOS6 > 303 & DIAGNOS6 <305)|
           (DIAGNOS7 > 303 & DIAGNOS7 <305))


Nondependent_aduse_of_drugs <-
  data2 %>%
  filter((DIAGNOS1 > 304 & DIAGNOS1 <306)| 
           (DIAGNOS2 > 304 & DIAGNOS2 <306)| 
           (DIAGNOS3 > 304 & DIAGNOS3 <306)|
           (DIAGNOS4 > 304 & DIAGNOS4 <306)|
           (DIAGNOS5 > 304 & DIAGNOS5 <306)|
           (DIAGNOS6 > 304 & DIAGNOS6 <306)|
           (DIAGNOS7 > 304 & DIAGNOS7 <306))


Other_primarily_adult_onset <-
  data2 %>%
  filter((DIAGNOS1 > 305 & DIAGNOS1 < 312)| 
           (DIAGNOS2 > 305 & DIAGNOS2 < 312)| 
           (DIAGNOS3 > 305 & DIAGNOS3 < 312)|
           (DIAGNOS4 > 305 & DIAGNOS4 < 312)|
           (DIAGNOS5 > 305 & DIAGNOS5 < 312)|
           (DIAGNOS6 > 305 & DIAGNOS6 < 312)|
           (DIAGNOS7 > 305 & DIAGNOS7 < 312))


Physiological_malfunction_arising_from_mental_factors <-
  data2 %>%
  filter((DIAGNOS1 > 305 & DIAGNOS1 < 307)| 
           (DIAGNOS2 > 305 & DIAGNOS2 < 307)| 
           (DIAGNOS3 > 305 & DIAGNOS3 < 307)|
           (DIAGNOS4 > 305 & DIAGNOS4 < 307)|
           (DIAGNOS5 > 305 & DIAGNOS5 < 307)|
           (DIAGNOS6 > 305 & DIAGNOS6 < 307)|
           (DIAGNOS7 > 305 & DIAGNOS7 < 307))


Special_symptoms_or_syndromes_not_elsewhere_classified <-
  data2 %>%
  filter((DIAGNOS1 > 306 & DIAGNOS1 < 308)| 
           (DIAGNOS2 > 306 & DIAGNOS2 < 308)| 
           (DIAGNOS3 > 306 & DIAGNOS3 < 308)|
           (DIAGNOS4 > 306 & DIAGNOS4 < 308)|
           (DIAGNOS5 > 306 & DIAGNOS5 < 308)|
           (DIAGNOS6 > 306 & DIAGNOS6 < 308)|
           (DIAGNOS7 > 306 & DIAGNOS7 < 308))


Acute_reaction_to_stress <-
  data2 %>%
  filter((DIAGNOS1 > 307 & DIAGNOS1 < 309)| 
           (DIAGNOS2 > 307 & DIAGNOS2 < 309)| 
           (DIAGNOS3 > 307 & DIAGNOS3 < 309)|
           (DIAGNOS4 > 307 & DIAGNOS4 < 309)|
           (DIAGNOS5 > 307 & DIAGNOS5 < 309)|
           (DIAGNOS6 > 307 & DIAGNOS6 < 309)|
           (DIAGNOS7 > 307 & DIAGNOS7 < 309))


Adjustment_reaction <-
  data2 %>%
  filter((DIAGNOS1 > 308 & DIAGNOS1 < 310)| 
           (DIAGNOS2 > 308 & DIAGNOS2 < 310)| 
           (DIAGNOS3 > 308 & DIAGNOS3 < 310)|
           (DIAGNOS4 > 308 & DIAGNOS4 < 310)|
           (DIAGNOS5 > 308 & DIAGNOS5 < 310)|
           (DIAGNOS6 > 308 & DIAGNOS6 < 310)|
           (DIAGNOS7 > 308 & DIAGNOS7 < 310))



Specific_nonpsychotic_mental_disorders_following_brain_damage <-
  data2 %>%
  filter((DIAGNOS1 > 309 & DIAGNOS1 < 311)| 
           (DIAGNOS2 > 309 & DIAGNOS2 < 311)| 
           (DIAGNOS3 > 309 & DIAGNOS3 < 311)|
           (DIAGNOS4 > 309 & DIAGNOS4 < 311)|
           (DIAGNOS5 > 309 & DIAGNOS5 < 311)|
           (DIAGNOS6 > 309 & DIAGNOS6 < 311)|
           (DIAGNOS7 > 309 & DIAGNOS7 < 311))


Depressive_disorder_not_elsewhere_classified <-
  data2 %>%
  filter((DIAGNOS1 > 310 & DIAGNOS1 < 312)| 
           (DIAGNOS2 > 310 & DIAGNOS2 < 312)| 
           (DIAGNOS3 > 310 & DIAGNOS3 < 312)|
           (DIAGNOS4 > 310 & DIAGNOS4 < 312)|
           (DIAGNOS5 > 310 & DIAGNOS5 < 312)|
           (DIAGNOS6 > 310 & DIAGNOS6 < 312)|
           (DIAGNOS7 > 310 & DIAGNOS7 < 312))


Mental_disorders_childhood <-
  data2 %>%
  filter((DIAGNOS1 > 311 & DIAGNOS1 <317)| 
           (DIAGNOS2 > 311 & DIAGNOS2 <317)| 
           (DIAGNOS3 > 311 & DIAGNOS3 <317)|
           (DIAGNOS4 > 311 & DIAGNOS4 <317)|
           (DIAGNOS5 > 311 & DIAGNOS5 <317)|
           (DIAGNOS6 > 311 & DIAGNOS6 <317)|
           (DIAGNOS7 > 311 & DIAGNOS7 <317))


Disturbance_of_conduct_not_elsewhere_classified <-
  data2 %>%
  filter((DIAGNOS1 > 311 & DIAGNOS1 <313)| 
           (DIAGNOS2 > 311 & DIAGNOS2 <313)| 
           (DIAGNOS3 > 311 & DIAGNOS3 <313)|
           (DIAGNOS4 > 311 & DIAGNOS4 <313)|
           (DIAGNOS5 > 311 & DIAGNOS5 <313)|
           (DIAGNOS6 > 311 & DIAGNOS6 <313)|
           (DIAGNOS7 > 311 & DIAGNOS7 <313))


Disturbance_of_emotions_specific_to_childhood_and_adolescence <-
  data2 %>%
  filter((DIAGNOS1 > 312 & DIAGNOS1 <314)| 
           (DIAGNOS2 > 312 & DIAGNOS2 <314)| 
           (DIAGNOS3 > 312 & DIAGNOS3 <314)|
           (DIAGNOS4 > 312 & DIAGNOS4 <314)|
           (DIAGNOS5 > 312 & DIAGNOS5 <314)|
           (DIAGNOS6 > 312 & DIAGNOS6 <314)|
           (DIAGNOS7 > 312 & DIAGNOS7 <314))


Hyperkinetic_syndrome_of_childhood <-
  data2 %>%
  filter((DIAGNOS1 > 313 & DIAGNOS1 <315)| 
           (DIAGNOS2 > 313 & DIAGNOS2 <315)| 
           (DIAGNOS3 > 313 & DIAGNOS3 <315)|
           (DIAGNOS4 > 313 & DIAGNOS4 <315)|
           (DIAGNOS5 > 313 & DIAGNOS5 <315)|
           (DIAGNOS6 > 313 & DIAGNOS6 <315)|
           (DIAGNOS7 > 313 & DIAGNOS7 <315))


Specific_delays_in_development <-
  data2 %>%
  filter((DIAGNOS1 > 314 & DIAGNOS1 <316)| 
           (DIAGNOS2 > 314 & DIAGNOS2 <316)| 
           (DIAGNOS3 > 314 & DIAGNOS3 <316)|
           (DIAGNOS4 > 314 & DIAGNOS4 <316)|
           (DIAGNOS5 > 314 & DIAGNOS5 <316)|
           (DIAGNOS6 > 314 & DIAGNOS6 <316)|
           (DIAGNOS7 > 314 & DIAGNOS7 <316))


Psychic_factors <-
  data2 %>%
  filter((DIAGNOS1 > 315 & DIAGNOS1 <317)| 
           (DIAGNOS2 > 315 & DIAGNOS2 <317)| 
           (DIAGNOS3 > 315 & DIAGNOS3 <317)|
           (DIAGNOS4 > 315 & DIAGNOS4 <317)|
           (DIAGNOS5 > 315 & DIAGNOS5 <317)|
           (DIAGNOS6 > 315 & DIAGNOS6 <317)|
           (DIAGNOS7 > 315 & DIAGNOS7 <317))


Mental_retardation <-
  data2 %>%
  filter((DIAGNOS1 > 316 & DIAGNOS1 <320)| 
           (DIAGNOS2 > 316 & DIAGNOS2 <320)| 
           (DIAGNOS3 > 316 & DIAGNOS3 <320)|
           (DIAGNOS4 > 316 & DIAGNOS4 <320)|
           (DIAGNOS5 > 316 & DIAGNOS5 <320)|
           (DIAGNOS6 > 316 & DIAGNOS6 <320)|
           (DIAGNOS7 > 316 & DIAGNOS7 <320))

#Substratification of skin and subcutaneous disorders

Infections_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 679 & DIAGNOS1 <687)| 
           (DIAGNOS2 > 679 & DIAGNOS2 <687)| 
           (DIAGNOS3 > 679 & DIAGNOS3 <687)|
           (DIAGNOS4 > 679 & DIAGNOS4 <687)|
           (DIAGNOS5 > 679 & DIAGNOS5 <687)|
           (DIAGNOS6 > 679 & DIAGNOS6 <687)|
           (DIAGNOS7 > 679 & DIAGNOS7 <687))


Inflammatory_conditions_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 689 & DIAGNOS1 < 699)| 
           (DIAGNOS2 > 689 & DIAGNOS2 <699)| 
           (DIAGNOS3 > 689 & DIAGNOS3 <699)|
           (DIAGNOS4 > 689 & DIAGNOS4 <699)|
           (DIAGNOS5 > 689 & DIAGNOS5 <699)|
           (DIAGNOS6 > 689 & DIAGNOS6 <699)|
           (DIAGNOS7 > 689 & DIAGNOS7 <699))


Other_diseases_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 699 & DIAGNOS1 < 710)| 
           (DIAGNOS2 > 699 & DIAGNOS2 <710)| 
           (DIAGNOS3 > 699 & DIAGNOS3 <710)|
           (DIAGNOS4 > 699 & DIAGNOS4 <710)|
           (DIAGNOS5 > 699 & DIAGNOS5 <710)|
           (DIAGNOS6 > 699 & DIAGNOS6 <710)|
           (DIAGNOS7 > 699 & DIAGNOS7 <710))


Psoriasis_and_similar_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 695 & DIAGNOS1 < 697)| 
           (DIAGNOS2 > 695 & DIAGNOS2 <697)| 
           (DIAGNOS3 > 695 & DIAGNOS3 <697)|
           (DIAGNOS4 > 695 & DIAGNOS4 <697)|
           (DIAGNOS5 > 695 & DIAGNOS5 <697)|
           (DIAGNOS6 > 695 & DIAGNOS6 <697)|
           (DIAGNOS7 > 695 & DIAGNOS7 <697))


#Diseases of the genitourinal system
Nephritis_nephrotic_sydrome_and_nephrosis <-
  data2 %>%
  filter((DIAGNOS1 > 579 & DIAGNOS1 < 590)| 
           (DIAGNOS2 > 579 & DIAGNOS2 <590)| 
           (DIAGNOS3 > 579 & DIAGNOS3 <590)|
           (DIAGNOS4 > 579 & DIAGNOS4 <590)|
           (DIAGNOS5 > 579 & DIAGNOS5 <590)|
           (DIAGNOS6 > 579 & DIAGNOS6 <590)|
           (DIAGNOS7 > 579 & DIAGNOS7 <590))

Other_diseases_of_urinary_system <-
  data2 %>%
  filter((DIAGNOS1 > 589 & DIAGNOS1 < 600)| 
           (DIAGNOS2 > 589 & DIAGNOS2 <600)| 
           (DIAGNOS3 > 589 & DIAGNOS3 <600)|
           (DIAGNOS4 > 589 & DIAGNOS4 <600)|
           (DIAGNOS5 > 589 & DIAGNOS5 <600)|
           (DIAGNOS6 > 589 & DIAGNOS6 <600)|
           (DIAGNOS7 > 589 & DIAGNOS7 <600))


Diseases_of_male_genital_organs <-
  data2 %>%
  filter((DIAGNOS1 > 599 & DIAGNOS1 < 609)| 
           (DIAGNOS2 > 599 & DIAGNOS2 <609)| 
           (DIAGNOS3 > 599 & DIAGNOS3 <609)|
           (DIAGNOS4 > 599 & DIAGNOS4 <609)|
           (DIAGNOS5 > 599 & DIAGNOS5 <609)|
           (DIAGNOS6 > 599 & DIAGNOS6 <609)|
           (DIAGNOS7 > 599 & DIAGNOS7 <609))



Diseases_of_breast <-
  data2 %>%
  filter((DIAGNOS1 > 609 & DIAGNOS1 < 613)| 
           (DIAGNOS2 > 609 & DIAGNOS2 <613)| 
           (DIAGNOS3 > 609 & DIAGNOS3 <613)|
           (DIAGNOS4 > 609 & DIAGNOS4 <613)|
           (DIAGNOS5 > 609 & DIAGNOS5 <613)|
           (DIAGNOS6 > 609 & DIAGNOS6 <613)|
           (DIAGNOS7 > 609 & DIAGNOS7 <613))


Inflammatory_disease_of_female_pelvic_organs <-
  data2 %>%
  filter((DIAGNOS1 > 613 & DIAGNOS1 < 617)| 
           (DIAGNOS2 > 613 & DIAGNOS2 <617)| 
           (DIAGNOS3 > 613 & DIAGNOS3 <617)|
           (DIAGNOS4 > 613 & DIAGNOS4 <617)|
           (DIAGNOS5 > 613 & DIAGNOS5 <617)|
           (DIAGNOS6 > 613 & DIAGNOS6 <617)|
           (DIAGNOS7 > 613 & DIAGNOS7 <617))

Disorders_of_female_genital_tract <-
  data2 %>%
  filter((DIAGNOS1 > 616 & DIAGNOS1 < 630)| 
           (DIAGNOS2 > 616 & DIAGNOS2 <630)| 
           (DIAGNOS3 > 616 & DIAGNOS3 <630)|
           (DIAGNOS4 > 616 & DIAGNOS4 <630)|
           (DIAGNOS5 > 616 & DIAGNOS5 <630)|
           (DIAGNOS6 > 616 & DIAGNOS6 <630)|
           (DIAGNOS7 > 616 & DIAGNOS7 <630))


#Diseases of the circulatory system

Acute_rheumatic_fever <-
  data2 %>%
  filter((DIAGNOS1 > 389 & DIAGNOS1 < 393)| 
           (DIAGNOS2 > 389 & DIAGNOS2 <393)| 
           (DIAGNOS3 > 389 & DIAGNOS3 <393)|
           (DIAGNOS4 > 389 & DIAGNOS4 <393)|
           (DIAGNOS5 > 389 & DIAGNOS5 <393)|
           (DIAGNOS6 > 389 & DIAGNOS6 <393)|
           (DIAGNOS7 > 389 & DIAGNOS7 <393))

Chronic_rheumatic_heart_disease <-
  data2 %>%
  filter((DIAGNOS1 > 392 & DIAGNOS1 < 399)| 
           (DIAGNOS2 > 392 & DIAGNOS2 <399)| 
           (DIAGNOS3 > 392 & DIAGNOS3 <399)|
           (DIAGNOS4 > 392 & DIAGNOS4 <399)|
           (DIAGNOS5 > 392 & DIAGNOS5 <399)|
           (DIAGNOS6 > 392 & DIAGNOS6 <399)|
           (DIAGNOS7 > 392 & DIAGNOS7 <399))


Hypertensive_disease <-
  data2 %>%
  filter((DIAGNOS1 > 400 & DIAGNOS1 < 406)| 
           (DIAGNOS2 > 400 & DIAGNOS2 <406)| 
           (DIAGNOS3 > 400 & DIAGNOS3 <406)|
           (DIAGNOS4 > 400 & DIAGNOS4 <406)|
           (DIAGNOS5 > 400 & DIAGNOS5 <406)|
           (DIAGNOS6 > 400 & DIAGNOS6 <406)|
           (DIAGNOS7 > 400 & DIAGNOS7 <406))


Ischemic_heart_disease <-
  data2 %>%
  filter((DIAGNOS1 > 409 & DIAGNOS1 < 415)| 
           (DIAGNOS2 > 409 & DIAGNOS2 <415)| 
           (DIAGNOS3 > 409 & DIAGNOS3 <415)|
           (DIAGNOS4 > 409 & DIAGNOS4 <415)|
           (DIAGNOS5 > 409 & DIAGNOS5 <415)|
           (DIAGNOS6 > 409 & DIAGNOS6 <415)|
           (DIAGNOS7 > 409 & DIAGNOS7 <415))


Diseases_of_pulmonary_circulation <-
  data2 %>%
  filter((DIAGNOS1 > 414 & DIAGNOS1 < 418)| 
           (DIAGNOS2 > 414 & DIAGNOS2 <418)| 
           (DIAGNOS3 > 414 & DIAGNOS3 <418)|
           (DIAGNOS4 > 414 & DIAGNOS4 <418)|
           (DIAGNOS5 > 414 & DIAGNOS5 <418)|
           (DIAGNOS6 > 414 & DIAGNOS6 <418)|
           (DIAGNOS7 > 414 & DIAGNOS7 <418))


Other_heart_diseases <-
  data2 %>%
  filter((DIAGNOS1 > 419 & DIAGNOS1 < 430)| 
           (DIAGNOS2 > 419 & DIAGNOS2 <430)| 
           (DIAGNOS3 > 419 & DIAGNOS3 <430)|
           (DIAGNOS4 > 419 & DIAGNOS4 <430)|
           (DIAGNOS5 > 419 & DIAGNOS5 <430)|
           (DIAGNOS6 > 419 & DIAGNOS6 <430)|
           (DIAGNOS7 > 419 & DIAGNOS7 <430))


Cerebrovascular_disease <-
  data2 %>%
  filter((DIAGNOS1 > 429 & DIAGNOS1 < 439)| 
           (DIAGNOS2 > 429 & DIAGNOS2 < 439)| 
           (DIAGNOS3 > 429 & DIAGNOS3 < 439)|
           (DIAGNOS4 > 429 & DIAGNOS4 < 439)|
           (DIAGNOS5 > 429 & DIAGNOS5 < 439)|
           (DIAGNOS6 > 429 & DIAGNOS6 < 439)|
           (DIAGNOS7 > 429 & DIAGNOS7 < 439))


Diseases_of_arteries_and_capillaries <-
  data2 %>%
  filter((DIAGNOS1 > 439 & DIAGNOS1 < 450)| 
           (DIAGNOS2 > 439 & DIAGNOS2 < 450)| 
           (DIAGNOS3 > 439 & DIAGNOS3 < 450)|
           (DIAGNOS4 > 439 & DIAGNOS4 < 450)|
           (DIAGNOS5 > 439 & DIAGNOS5 < 450)|
           (DIAGNOS6 > 439 & DIAGNOS6 < 450)|
           (DIAGNOS7 > 439 & DIAGNOS7 < 450))


Diseases_of_veins_and_lymphatics_and_other <-
  data2 %>%
  filter((DIAGNOS1 > 450 & DIAGNOS1 < 460)| 
           (DIAGNOS2 > 450 & DIAGNOS2 < 460)| 
           (DIAGNOS3 > 450 & DIAGNOS3 < 460)|
           (DIAGNOS4 > 450 & DIAGNOS4 < 460)|
           (DIAGNOS5 > 450 & DIAGNOS5 < 460)|
           (DIAGNOS6 > 450 & DIAGNOS6 < 460)|
           (DIAGNOS7 > 450 & DIAGNOS7 < 460))

#Neoplasms

Neoplasms_of_oral_cavity <-
  data2 %>%
  filter((DIAGNOS1 > 139 & DIAGNOS1 < 150)| 
           (DIAGNOS2 > 139 & DIAGNOS2 < 150)| 
           (DIAGNOS3 > 139 & DIAGNOS3 < 150)|
           (DIAGNOS4 > 139 & DIAGNOS4 < 150)|
           (DIAGNOS5 > 139 & DIAGNOS5 < 150)|
           (DIAGNOS6 > 139 & DIAGNOS6 < 150)|
           (DIAGNOS7 > 139 & DIAGNOS7 < 150))



Neoplasms_of_digestive_organs_and_peritoneum <-
  data2 %>%
  filter((DIAGNOS1 > 149 & DIAGNOS1 < 160)| 
           (DIAGNOS2 > 149 & DIAGNOS2 < 160)| 
           (DIAGNOS3 > 149 & DIAGNOS3 < 160)|
           (DIAGNOS4 > 149 & DIAGNOS4 < 160)|
           (DIAGNOS5 > 149 & DIAGNOS5 < 160)|
           (DIAGNOS6 > 149 & DIAGNOS6 < 160)|
           (DIAGNOS7 > 149 & DIAGNOS7 < 160))



Neoplasms_of_respiratory_and_intrathoracic_organs <-
  data2 %>%
  filter((DIAGNOS1 > 159 & DIAGNOS1 < 166)| 
           (DIAGNOS2 > 159 & DIAGNOS2 < 166)| 
           (DIAGNOS3 > 159 & DIAGNOS3 < 166)|
           (DIAGNOS4 > 159 & DIAGNOS4 < 166)|
           (DIAGNOS5 > 159 & DIAGNOS5 < 166)|
           (DIAGNOS6 > 159 & DIAGNOS6 < 166)|
           (DIAGNOS7 > 159 & DIAGNOS7 < 166))


Neoplasms_of_bone_connective_tissue_skin_and_breast <-
  data2 %>%
  filter((DIAGNOS1 > 169 & DIAGNOS1 < 176)| 
           (DIAGNOS2 > 169 & DIAGNOS2 < 176)| 
           (DIAGNOS3 > 169 & DIAGNOS3 < 176)|
           (DIAGNOS4 > 169 & DIAGNOS4 < 176)|
           (DIAGNOS5 > 169 & DIAGNOS5 < 176)|
           (DIAGNOS6 > 169 & DIAGNOS6 < 176)|
           (DIAGNOS7 > 169 & DIAGNOS7 < 176))


Kaposis_sarcoma <-
  data2 %>%
  filter((DIAGNOS1 > 175 & DIAGNOS1 < 177)| 
           (DIAGNOS2 > 175 & DIAGNOS2 < 177)| 
           (DIAGNOS3 > 175 & DIAGNOS3 < 177)|
           (DIAGNOS4 > 175 & DIAGNOS4 < 177)|
           (DIAGNOS5 > 175 & DIAGNOS5 < 177)|
           (DIAGNOS6 > 175 & DIAGNOS6 < 177)|
           (DIAGNOS7 > 175 & DIAGNOS7 < 177))


Malignant_neoplasms_of_genitourinary_organs <-
  data2 %>%
  filter((DIAGNOS1 > 178 & DIAGNOS1 < 190)| 
           (DIAGNOS2 > 178 & DIAGNOS2 < 190)| 
           (DIAGNOS3 > 178 & DIAGNOS3 < 190)|
           (DIAGNOS4 > 178 & DIAGNOS4 < 190)|
           (DIAGNOS5 > 178 & DIAGNOS5 < 190)|
           (DIAGNOS6 > 178 & DIAGNOS6 < 190)|
           (DIAGNOS7 > 178 & DIAGNOS7 < 190))


Malignant_neoplasms_of_other_and_unspecified_sites <-
  data2 %>%
  filter((DIAGNOS1 > 189 & DIAGNOS1 < 200)| 
           (DIAGNOS2 > 189 & DIAGNOS2 < 200)| 
           (DIAGNOS3 > 189 & DIAGNOS3 < 200)|
           (DIAGNOS4 > 189 & DIAGNOS4 < 200)|
           (DIAGNOS5 > 189 & DIAGNOS5 < 200)|
           (DIAGNOS6 > 189 & DIAGNOS6 < 200)|
           (DIAGNOS7 > 189 & DIAGNOS7 < 200))



Malignant_neoplasms_of_lymphatic_and_hematopoietic_tissue <-
  data2 %>%
  filter((DIAGNOS1 > 199 & DIAGNOS1 < 209)| 
           (DIAGNOS2 > 199 & DIAGNOS2 < 209)| 
           (DIAGNOS3 > 199 & DIAGNOS3 < 209)|
           (DIAGNOS4 > 199 & DIAGNOS4 < 209)|
           (DIAGNOS5 > 199 & DIAGNOS5 < 209)|
           (DIAGNOS6 > 199 & DIAGNOS6 < 209)|
           (DIAGNOS7 > 199 & DIAGNOS7 < 209))


Benign_neoplasms <-
  data2 %>%
  filter((DIAGNOS1 > 219 & DIAGNOS1 < 230)| 
           (DIAGNOS2 > 219 & DIAGNOS2 < 230)| 
           (DIAGNOS3 > 219 & DIAGNOS3 < 230)|
           (DIAGNOS4 > 219 & DIAGNOS4 < 230)|
           (DIAGNOS5 > 219 & DIAGNOS5 < 230)|
           (DIAGNOS6 > 219 & DIAGNOS6 < 230)|
           (DIAGNOS7 > 219 & DIAGNOS7 < 230))


Carcinoma_in_situ <-
  data2 %>%
  filter((DIAGNOS1 > 229 & DIAGNOS1 < 235)| 
           (DIAGNOS2 > 229 & DIAGNOS2 < 235)| 
           (DIAGNOS3 > 229 & DIAGNOS3 < 235)|
           (DIAGNOS4 > 229 & DIAGNOS4 < 235)|
           (DIAGNOS5 > 229 & DIAGNOS5 < 235)|
           (DIAGNOS6 > 229 & DIAGNOS6 < 235)|
           (DIAGNOS7 > 229 & DIAGNOS7 < 235))


Neoplasms_of_uncertain_behavior <-
  data2 %>%
  filter((DIAGNOS1 > 234 & DIAGNOS1 < 239)| 
           (DIAGNOS2 > 234 & DIAGNOS2 < 239)| 
           (DIAGNOS3 > 234 & DIAGNOS3 < 239)|
           (DIAGNOS4 > 234 & DIAGNOS4 < 239)|
           (DIAGNOS5 > 234 & DIAGNOS5 < 239)|
           (DIAGNOS6 > 234 & DIAGNOS6 < 239)|
           (DIAGNOS7 > 234 & DIAGNOS7 < 239))


Neoplasms_of_unspecified_nature <-
  data2 %>%
  filter((DIAGNOS1 > 238 & DIAGNOS1 < 240)| 
           (DIAGNOS2 > 238 & DIAGNOS2 < 240)| 
           (DIAGNOS3 > 238 & DIAGNOS3 < 240)|
           (DIAGNOS4 > 238 & DIAGNOS4 < 240)|
           (DIAGNOS5 > 238 & DIAGNOS5 < 240)|
           (DIAGNOS6 > 238 & DIAGNOS6 < 240)|
           (DIAGNOS7 > 238 & DIAGNOS7 < 240))


Malignant_neoplasm_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 139 & DIAGNOS1 < 150)| 
           (DIAGNOS2 > 139 & DIAGNOS2 < 150)| 
           (DIAGNOS3 > 139 & DIAGNOS3 < 150)|
           (DIAGNOS4 > 139 & DIAGNOS4 < 150)|
           (DIAGNOS5 > 139 & DIAGNOS5 < 150)|
           (DIAGNOS6 > 139 & DIAGNOS6 < 150)|
           (DIAGNOS7 > 139 & DIAGNOS7 < 150))



#Diseases of the respiratory system

Acute_respiratory_infections <-
  data2 %>%
  filter((DIAGNOS1 > 459 & DIAGNOS1 < 467)| 
           (DIAGNOS2 > 459 & DIAGNOS2 < 467)| 
           (DIAGNOS3 > 459 & DIAGNOS3 < 467)|
           (DIAGNOS4 > 459 & DIAGNOS4 < 467)|
           (DIAGNOS5 > 459 & DIAGNOS5 < 467)|
           (DIAGNOS6 > 459 & DIAGNOS6 < 467)|
           (DIAGNOS7 > 459 & DIAGNOS7 < 467))




Other_diseases_of_the_upper_respiratory_tract <-
  data2 %>%
  filter((DIAGNOS1 > 469 & DIAGNOS1 < 479)| 
           (DIAGNOS2 > 469 & DIAGNOS2 < 479)| 
           (DIAGNOS3 > 469 & DIAGNOS3 < 479)|
           (DIAGNOS4 > 469 & DIAGNOS4 < 479)|
           (DIAGNOS5 > 469 & DIAGNOS5 < 479)|
           (DIAGNOS6 > 469 & DIAGNOS6 < 479)|
           (DIAGNOS7 > 469 & DIAGNOS7 < 479))



Pneumonia_and_influenza <-
  data2 %>%
  filter((DIAGNOS1 > 479 & DIAGNOS1 < 489)| 
           (DIAGNOS2 > 479 & DIAGNOS2 < 489)| 
           (DIAGNOS3 > 479 & DIAGNOS3 < 489)|
           (DIAGNOS4 > 479 & DIAGNOS4 < 489)|
           (DIAGNOS5 > 479 & DIAGNOS5 < 489)|
           (DIAGNOS6 > 479 & DIAGNOS6 < 489)|
           (DIAGNOS7 > 479 & DIAGNOS7 < 489))



Chronic_obstructive_pulmonary_disease_and_allied_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 489 & DIAGNOS1 < 497)| 
           (DIAGNOS2 > 489 & DIAGNOS2 < 497)| 
           (DIAGNOS3 > 489 & DIAGNOS3 < 497)|
           (DIAGNOS4 > 489 & DIAGNOS4 < 497)|
           (DIAGNOS5 > 489 & DIAGNOS5 < 497)|
           (DIAGNOS6 > 489 & DIAGNOS6 < 497)|
           (DIAGNOS7 > 489 & DIAGNOS7 < 497))


Pneumoconioses_and_other_lung_diseases_due_to_external_agents <-
  data2 %>%
  filter((DIAGNOS1 > 499 & DIAGNOS1 < 509)| 
           (DIAGNOS2 > 499 & DIAGNOS2 < 509)| 
           (DIAGNOS3 > 499 & DIAGNOS3 < 509)|
           (DIAGNOS4 > 499 & DIAGNOS4 < 509)|
           (DIAGNOS5 > 499 & DIAGNOS5 < 509)|
           (DIAGNOS6 > 499 & DIAGNOS6 < 509)|
           (DIAGNOS7 > 499 & DIAGNOS7 < 509))




Other_diseases_of_respiratory_system <-
  data2 %>%
  filter((DIAGNOS1 > 509 & DIAGNOS1 < 520)| 
           (DIAGNOS2 > 509 & DIAGNOS2 < 520)| 
           (DIAGNOS3 > 509 & DIAGNOS3 < 520)|
           (DIAGNOS4 > 509 & DIAGNOS4 < 520)|
           (DIAGNOS5 > 509 & DIAGNOS5 < 520)|
           (DIAGNOS6 > 509 & DIAGNOS6 < 520)|
           (DIAGNOS7 > 509 & DIAGNOS7 < 520))




##Put the different diagnosis groups into a list

ICD_9_code_groups <- list(infectious_and_parasitic_diseases,
neoplasms,
endocrine_metabolic_immunity_disorders, 
diseases_of_blood, 
psychiatric_diagnoses, 
diseases_of_nervous_system, 
diseases_of_circulatory_system, 
diseases_of_respiratory_system, 
diseases_of_digestive_system, 
diseases_of_genitourinary_system, 
complicatoins_of_pregnancy_and_childbirth, 
diseases_of_skin,
diseases_of_musculoskeletal_and_connective_tissue,
congenital_anomalies, 
conditions_of_perinatal_period, 
ill_defined_conditions,
injury_and_poisoning) 



ICD_9_code_groups_psychiatric <- list(Organic_psychotic_conditions,
                                            Senile_and_presenile_organic_psychotic_conditions,
                                            Alcoholic_psychoses,
                                            Drug_psychoses,
                                            Transient_organic_psychotic_conditions,
                                            Other_organic_psychotic_conditions_chronic,
                                            Other_psychoses, 
                                            Schizophrenic_psychoses, 
                                            Affective_psychoses, 
                                            Paranoid_states, 
                                            Other_nonorganic_psychoses, 
                                            Psychoses_with_origin_specific_to_childhood, 
                                            Neurotic_disorders_personality_disorders_and_nonpsychotic_mental_disorders, 
                                            Neurotic_disorders, 
                                            Personality_disorders, 
                                            Sexual_deviations, 
                                            Psychoactive_substance, 
                                            Alcohol_dependence_syndrome, 
                                            Drug_dependence, 
                                            Nondependent_aduse_of_drugs, 
                                            Other_primarily_adult_onset, 
                                            Physiological_malfunction_arising_from_mental_factors, 
                                            Special_symptoms_or_syndromes_not_elsewhere_classified, 
                                            Acute_reaction_to_stress, 
                                            Adjustment_reaction, 
                                            Specific_nonpsychotic_mental_disorders_following_brain_damage, 
                                            Depressive_disorder_not_elsewhere_classified, 
                                            Mental_disorders_childhood, 
                                            Disturbance_of_conduct_not_elsewhere_classified, 
                                            Disturbance_of_emotions_specific_to_childhood_and_adolescence, 
                                            Hyperkinetic_syndrome_of_childhood, 
                                            Specific_delays_in_development, 
                                            Psychic_factors, 
                                            Mental_retardation) 

#Diseases of skin
ICD_9_code_groups_skin <- list(
Infections_of_skin, 
Inflammatory_conditions_of_skin, 
Other_diseases_of_skin, 
Psoriasis_and_similar_disorders) 



#Diseases of the genitourinal system
ICD_9_code_groups_genitourinal <- list(
Nephritis_nephrotic_sydrome_and_nephrosis, 
Other_diseases_of_urinary_system, 
Diseases_of_male_genital_organs, 
Diseases_of_breast,
Inflammatory_disease_of_female_pelvic_organs, 
Disorders_of_female_genital_tract)


#Diseases of the circulatory system

ICD_9_code_groups_circulatory <- list(
Acute_rheumatic_fever, 
Chronic_rheumatic_heart_disease,
Hypertensive_disease, 
Ischemic_heart_disease, 
Diseases_of_pulmonary_circulation, 
Other_heart_diseases, 
Cerebrovascular_disease, 
Diseases_of_arteries_and_capillaries, 
Diseases_of_veins_and_lymphatics_and_other) 

#Neoplasms

ICD_9_code_groups_neoplasms <- list(
Neoplasms_of_oral_cavity, 
Neoplasms_of_digestive_organs_and_peritoneum,
Neoplasms_of_respiratory_and_intrathoracic_organs,
Neoplasms_of_bone_connective_tissue_skin_and_breast, 
Kaposis_sarcoma, 
Malignant_neoplasms_of_genitourinary_organs, 
Malignant_neoplasms_of_other_and_unspecified_sites, 
Malignant_neoplasms_of_lymphatic_and_hematopoietic_tissue, 
Benign_neoplasms, 
Carcinoma_in_situ, 
Neoplasms_of_uncertain_behavior, 
Neoplasms_of_unspecified_nature, 
Malignant_neoplasm_of_skin) 



#Diseases of the respiratory system

ICD_9_code_groups_respiratory <- list(
Acute_respiratory_infections, 
Other_diseases_of_the_upper_respiratory_tract, 
Pneumonia_and_influenza, 
Chronic_obstructive_pulmonary_disease_and_allied_conditions, 
Pneumoconioses_and_other_lung_diseases_due_to_external_agents, 
Other_diseases_of_respiratory_system) 








#the whole list
list(infectious_and_parasitic_diseases, 
                          neoplasms,
                          Neoplasms_of_oral_cavity,
                          Neoplasms_of_digestive_organs_and_peritoneum,
                          Neoplasms_of_respiratory_and_intrathoracic_organs,
                          Neoplasms_of_bone_connective_tissue_skin_and_breast,
                          Kaposis_sarcoma,
                          Malignant_neoplasms_of_genitourinary_organs,
                          Malignant_neoplasms_of_other_and_unspecified_sites,
                          Malignant_neoplasms_of_lymphatic_and_hematopoietic_tissue,
                          Benign_neoplasms,
                          Carcinoma_in_situ,
                          Neoplasms_of_uncertain_behavior,
                          Neoplasms_of_unspecified_nature,
                          Malignant_neoplasm_of_skin,
                          endocrine_metabolic_immunity_disorders, 
                          diseases_of_blood,
                          psychiatric_diagnoses,
                          Organic_psychotic_conditions,
                          Senile_and_presenile_organic_psychotic_conditions,
                          Alcoholic_psychoses,
                          Drug_psychoses,
                          Transient_organic_psychotic_conditions,
                          Other_organic_psychotic_conditions_chronic,
                          Other_psychoses,
                          Schizophrenic_psychoses,
                          Affective_psychoses,
                          Paranoid_states,
                          Other_nonorganic_psychoses,
                          Psychoses_with_origin_specific_to_childhood,
                          Neurotic_disorders_personality_disorders_and_nonpsychotic_mental_disorders,
                          Neurotic_disorders,
                          Personality_disorders,
                          Sexual_deviations,
                          Psychoactive_substance,
                          Alcohol_dependence_syndrome,
                          Drug_dependence,
                          Nondependent_aduse_of_drugs,
                          Other_primarily_adult_onset,
                          Physiological_malfunction_arising_from_mental_factors,
                          Special_symptoms_or_syndromes_not_elsewhere_classified,
                          Acute_reaction_to_stress,
                          Adjustment_reaction,
                          Specific_nonpsychotic_mental_disorders_following_brain_damage,
                          Depressive_disorder_not_elsewhere_classified,
                          Mental_disorders_childhood,
                          Disturbance_of_conduct_not_elsewhere_classified,
                          Disturbance_of_emotions_specific_to_childhood_and_adolescence,
                          Hyperkinetic_syndrome_of_childhood,
                          Specific_delays_in_development,
                          Psychic_factors,
                          Mental_retardation,
                          diseases_of_nervous_system, 
                          diseases_of_circulatory_system,
                          Acute_rheumatic_fever,
                          Chronic_rheumatic_heart_disease,
                          Hypertensive_disease,
                          Ischemic_heart_disease,
                          Diseases_of_pulmonary_circulation,
                          Other_heart_diseases,
                          Cerebrovascular_disease,
                          Diseases_of_arteries_and_capillaries,
                          Diseases_of_veins_and_lymphatics_and_other,
                          diseases_of_respiratory_system,
                          Acute_respiratory_infections,
                          Other_diseases_of_the_upper_respiratory_tract,
                          Pneumonia_and_influenza,
                          Chronic_obstructive_pulmonary_disease_and_allied_conditions,
                          Pneumoconioses_and_other_lung_diseases_due_to_external_agents,
                          Other_diseases_of_respiratory_system,
                          diseases_of_digestive_system, 
                          diseases_of_genitourinary_system,
                          Nephritis_nephrotic_sydrome_and_nephrosis,
                          Other_diseases_of_urinary_system,
                          Diseases_of_breast,
                          complicatoins_of_pregnancy_and_childbirth, 
                          diseases_of_skin,
                          Infections_of_skin,
                          Inflammatory_conditions_of_skin,
                          Psoriasis_and_similar_disorders,
                          Other_diseases_of_skin,
                          diseases_of_musculoskeletal_and_connective_tissue, 
                          congenital_anomalies, 
                          conditions_of_perinatal_period, 
                          ill_defined_conditions, 
                          injury_and_poisoning,
                          Diseases_of_male_genital_organs,
                          Inflammatory_disease_of_female_pelvic_organs,
                          Disorders_of_female_genital_tract)






#######Prepare data on different diagnoses: primary diagnosis#####
primary.diagnosis.infectious_and_parasitic_diseases <-
  data2 %>%
  filter((DIAGNOS1 > 000 & DIAGNOS1 <140))

primary.diagnosis.neoplasms <-
  data2 %>%
  filter((DIAGNOS1 > 139 & DIAGNOS1 <240))

primary.diagnosis.endocrine_metabolic_immunity_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 239 & DIAGNOS1 <280))

primary.diagnosis.diseases_of_blood <-
  data2 %>%
  filter((DIAGNOS1 > 279 & DIAGNOS1 <290))

primary.diagnosis.psychiatric_diagnoses <-
  data2 %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <320))

primary.diagnosis.diseases_of_nervous_system <-
  data2 %>%
  filter((DIAGNOS1 > 319 & DIAGNOS1 <390))

primary.diagnosis.diseases_of_circulatory_system <-
  data2 %>%
  filter((DIAGNOS1 > 389 & DIAGNOS1 <460))

primary.diagnosis.diseases_of_respiratory_system <-
  data2 %>%
  filter((DIAGNOS1 > 459 & DIAGNOS1 <520))

primary.diagnosis.diseases_of_digestive_system <-
  data2 %>%
  filter((DIAGNOS1 > 519 & DIAGNOS1 <580))

primary.diagnosis.diseases_of_genitourinary_system <-
  data2 %>%
  filter((DIAGNOS1 > 579 & DIAGNOS1 <630))

primary.diagnosis.complicatoins_of_pregnancy_and_childbirth <-
  data2 %>%
  filter((DIAGNOS1 > 629 & DIAGNOS1 <680))

primary.diagnosis.diseases_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 679 & DIAGNOS1 <710))

primary.diagnosis.diseases_of_musculoskeletal_and_connective_tissue <-
  data2 %>%
  filter((DIAGNOS1 > 709 & DIAGNOS1 <740))

primary.diagnosis.congenital_anomalies <-
  data2 %>%
  filter((DIAGNOS1 > 739 & DIAGNOS1 <760))

primary.diagnosis.conditions_of_perinatal_period <-
  data2 %>%
  filter((DIAGNOS1 > 759 & DIAGNOS1 <780))

primary.diagnosis.ill_defined_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 779 & DIAGNOS1 <800))


primary.diagnosis.injury_and_poisoning <-
  data2 %>%
  filter((DIAGNOS1 > 799 & DIAGNOS1 <= 999))

##
#Being more specific on the psychiatric diagnoses
primary.diagnosis.Organic_psychotic_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <295))


primary.diagnosis.Senile_and_presenile_organic_psychotic_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <291))



primary.diagnosis.Alcoholic_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 290 & DIAGNOS1 <292))



primary.diagnosis.Drug_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 291 & DIAGNOS1 <293))


primary.diagnosis.Transient_organic_psychotic_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 292 & DIAGNOS1 <294))


primary.diagnosis.Other_organic_psychotic_conditions_chronic <-
  data2 %>%
  filter((DIAGNOS1 > 293 & DIAGNOS1 <295))


primary.diagnosis.Other_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 294 & DIAGNOS1 <300))



primary.diagnosis.Schizophrenic_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 294 & DIAGNOS1 <296))




primary.diagnosis.Affective_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 295 & DIAGNOS1 <297))


primary.diagnosis.Paranoid_states <-
  data2 %>%
  filter((DIAGNOS1 > 296 & DIAGNOS1 <298))


primary.diagnosis.Other_nonorganic_psychoses <-
  data2 %>%
  filter((DIAGNOS1 > 297 & DIAGNOS1 <299))



primary.diagnosis.Psychoses_with_origin_specific_to_childhood <-
  data2 %>%
  filter((DIAGNOS1 > 298 & DIAGNOS1 <300))




primary.diagnosis.Neurotic_disorders_personality_disorders_and_nonpsychotic_mental_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 299 & DIAGNOS1 <317))


primary.diagnosis.Neurotic_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 299 & DIAGNOS1 <301))


primary.diagnosis.Personality_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 300 & DIAGNOS1 <302))

primary.diagnosis.Sexual_deviations <-
  data2 %>%
  filter((DIAGNOS1 > 301 & DIAGNOS1 <303))


primary.diagnosis.Psychoactive_substance <-
  data2 %>%
  filter((DIAGNOS1 > 302 & DIAGNOS1 <306))

primary.diagnosis.Alcohol_dependence_syndrome <-
  data2 %>%
  filter((DIAGNOS1 > 302 & DIAGNOS1 <304))


primary.diagnosis.Drug_dependence <-
  data2 %>%
  filter((DIAGNOS1 > 303 & DIAGNOS1 <305))


primary.diagnosis.Nondependent_aduse_of_drugs <-
  data2 %>%
  filter((DIAGNOS1 > 304 & DIAGNOS1 <306))


primary.diagnosis.Other_primarily_adult_onset <-
  data2 %>%
  filter((DIAGNOS1 > 305 & DIAGNOS1 < 312))


primary.diagnosis.Physiological_malfunction_arising_from_mental_factors <-
  data2 %>%
  filter((DIAGNOS1 > 305 & DIAGNOS1 < 307))


primary.diagnosis.Special_symptoms_or_syndromes_not_elsewhere_classified <-
  data2 %>%
  filter((DIAGNOS1 > 306 & DIAGNOS1 < 308))


primary.diagnosis.Acute_reaction_to_stress <-
  data2 %>%
  filter((DIAGNOS1 > 307 & DIAGNOS1 < 309))


primary.diagnosis.Adjustment_reaction <-
  data2 %>%
  filter((DIAGNOS1 > 308 & DIAGNOS1 < 310))



primary.diagnosis.Specific_nonpsychotic_mental_disorders_following_brain_damage <-
  data2 %>%
  filter((DIAGNOS1 > 309 & DIAGNOS1 < 311))


primary.diagnosis.Depressive_disorder_not_elsewhere_classified <-
  data2 %>%
  filter((DIAGNOS1 > 310 & DIAGNOS1 < 312))


primary.diagnosis.Mental_disorders_childhood <-
  data2 %>%
  filter((DIAGNOS1 > 311 & DIAGNOS1 <317))


primary.diagnosis.Disturbance_of_conduct_not_elsewhere_classified <-
  data2 %>%
  filter((DIAGNOS1 > 311 & DIAGNOS1 <313))


primary.diagnosis.Disturbance_of_emotions_specific_to_childhood_and_adolescence <-
  data2 %>%
  filter((DIAGNOS1 > 312 & DIAGNOS1 <314))


primary.diagnosis.Hyperkinetic_syndrome_of_childhood <-
  data2 %>%
  filter((DIAGNOS1 > 313 & DIAGNOS1 <315))


primary.diagnosis.Specific_delays_in_development <-
  data2 %>%
  filter((DIAGNOS1 > 314 & DIAGNOS1 <316))


primary.diagnosis.Psychic_factors <-
  data2 %>%
  filter((DIAGNOS1 > 315 & DIAGNOS1 <317))


primary.diagnosis.Mental_retardation <-
  data2 %>%
  filter((DIAGNOS1 > 316 & DIAGNOS1 <320))

#Substratification of skin and subcutaneous disorders

primary.diagnosis.Infections_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 679 & DIAGNOS1 <687))


primary.diagnosis.Inflammatory_conditions_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 689 & DIAGNOS1 < 699))


primary.diagnosis.Other_diseases_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 699 & DIAGNOS1 < 710))


primary.diagnosis.Psoriasis_and_similar_disorders <-
  data2 %>%
  filter((DIAGNOS1 > 695 & DIAGNOS1 < 697))


#Diseases of the genitourinal system
primary.diagnosis.Nephritis_nephrotic_sydrome_and_nephrosis <-
  data2 %>%
  filter((DIAGNOS1 > 579 & DIAGNOS1 < 590))

primary.diagnosis.Other_diseases_of_urinary_system <-
  data2 %>%
  filter((DIAGNOS1 > 589 & DIAGNOS1 < 600))


primary.diagnosis.Diseases_of_male_genital_organs <-
  data2 %>%
  filter((DIAGNOS1 > 599 & DIAGNOS1 < 609))



primary.diagnosis.Diseases_of_breast <-
  data2 %>%
  filter((DIAGNOS1 > 609 & DIAGNOS1 < 613))


primary.diagnosis.Inflammatory_disease_of_female_pelvic_organs <-
  data2 %>%
  filter((DIAGNOS1 > 613 & DIAGNOS1 < 617))

primary.diagnosis.Disorders_of_female_genital_tract <-
  data2 %>%
  filter((DIAGNOS1 > 616 & DIAGNOS1 < 630))


#Diseases of the circulatory system

primary.diagnosis.Acute_rheumatic_fever <-
  data2 %>%
  filter((DIAGNOS1 > 389 & DIAGNOS1 < 393))

primary.diagnosis.Chronic_rheumatic_heart_disease <-
  data2 %>%
  filter((DIAGNOS1 > 392 & DIAGNOS1 < 399))


primary.diagnosis.Hypertensive_disease <-
  data2 %>%
  filter((DIAGNOS1 > 400 & DIAGNOS1 < 406))


primary.diagnosis.Ischemic_heart_disease <-
  data2 %>%
  filter((DIAGNOS1 > 409 & DIAGNOS1 < 415))


primary.diagnosis.Diseases_of_pulmonary_circulation <-
  data2 %>%
  filter((DIAGNOS1 > 414 & DIAGNOS1 < 418))


primary.diagnosis.Other_heart_diseases <-
  data2 %>%
  filter((DIAGNOS1 > 419 & DIAGNOS1 < 430))


primary.diagnosis.Cerebrovascular_disease <-
  data2 %>%
  filter((DIAGNOS1 > 429 & DIAGNOS1 < 439))


primary.diagnosis.Diseases_of_arteries_and_capillaries <-
  data2 %>%
  filter((DIAGNOS1 > 439 & DIAGNOS1 < 450))


primary.diagnosis.Diseases_of_veins_and_lymphatics_and_other <-
  data2 %>%
  filter((DIAGNOS1 > 450 & DIAGNOS1 < 460))

#Neoplasms

primary.diagnosis.Neoplasms_of_oral_cavity <-
  data2 %>%
  filter((DIAGNOS1 > 139 & DIAGNOS1 < 150))



primary.diagnosis.Neoplasms_of_digestive_organs_and_peritoneum <-
  data2 %>%
  filter((DIAGNOS1 > 149 & DIAGNOS1 < 160))



primary.diagnosis.Neoplasms_of_respiratory_and_intrathoracic_organs <-
  data2 %>%
  filter((DIAGNOS1 > 159 & DIAGNOS1 < 166))


primary.diagnosis.Neoplasms_of_bone_connective_tissue_skin_and_breast <-
  data2 %>%
  filter((DIAGNOS1 > 169 & DIAGNOS1 < 176))


primary.diagnosis.Kaposis_sarcoma <-
  data2 %>%
  filter((DIAGNOS1 > 175 & DIAGNOS1 < 177))


primary.diagnosis.Malignant_neoplasms_of_genitourinary_organs <-
  data2 %>%
  filter((DIAGNOS1 > 178 & DIAGNOS1 < 190))


primary.diagnosis.Malignant_neoplasms_of_other_and_unspecified_sites <-
  data2 %>%
  filter((DIAGNOS1 > 189 & DIAGNOS1 < 200))



primary.diagnosis.Malignant_neoplasms_of_lymphatic_and_hematopoietic_tissue <-
  data2 %>%
  filter((DIAGNOS1 > 199 & DIAGNOS1 < 209))


primary.diagnosis.Benign_neoplasms <-
  data2 %>%
  filter((DIAGNOS1 > 219 & DIAGNOS1 < 230))


primary.diagnosis.Carcinoma_in_situ <-
  data2 %>%
  filter((DIAGNOS1 > 229 & DIAGNOS1 < 235))


primary.diagnosis.Neoplasms_of_uncertain_behavior <-
  data2 %>%
  filter((DIAGNOS1 > 234 & DIAGNOS1 < 239))


primary.diagnosis.Neoplasms_of_unspecified_nature <-
  data2 %>%
  filter((DIAGNOS1 > 238 & DIAGNOS1 < 240))


primary.diagnosis.Malignant_neoplasm_of_skin <-
  data2 %>%
  filter((DIAGNOS1 > 139 & DIAGNOS1 < 150))



#Diseases of the respiratory system

primary.diagnosis.Acute_respiratory_infections <-
  data2 %>%
  filter((DIAGNOS1 > 459 & DIAGNOS1 < 467))



primary.diagnosis.Other_diseases_of_the_upper_respiratory_tract <-
  data2 %>%
  filter((DIAGNOS1 > 469 & DIAGNOS1 < 479))



primary.diagnosis.Pneumonia_and_influenza <-
  data2 %>%
  filter((DIAGNOS1 > 479 & DIAGNOS1 < 489))

primary.diagnosis.Chronic_obstructive_pulmonary_disease_and_allied_conditions <-
  data2 %>%
  filter((DIAGNOS1 > 489 & DIAGNOS1 < 497))


primary.diagnosis.Pneumoconioses_and_other_lung_diseases_due_to_external_agents <-
  data2 %>%
  filter((DIAGNOS1 > 499 & DIAGNOS1 < 509))


primary.diagnosis.Other_diseases_of_respiratory_system <-
  data2 %>%
  filter((DIAGNOS1 > 509 & DIAGNOS1 < 520))


##Put the different diagnosis groups into a list
primary.diagnosis.ICD_9_code_groups <- list(primary.diagnosis.infectious_and_parasitic_diseases, 
                                            primary.diagnosis.neoplasms,
                                            primary.diagnosis.Neoplasms_of_oral_cavity,
                                            primary.diagnosis.Neoplasms_of_digestive_organs_and_peritoneum,
                                            primary.diagnosis.Neoplasms_of_respiratory_and_intrathoracic_organs,
                                            primary.diagnosis.Neoplasms_of_bone_connective_tissue_skin_and_breast,
                                            primary.diagnosis.Kaposis_sarcoma,
                                            primary.diagnosis.Malignant_neoplasms_of_genitourinary_organs,
                                            primary.diagnosis.Malignant_neoplasms_of_other_and_unspecified_sites,
                                            primary.diagnosis.Malignant_neoplasms_of_lymphatic_and_hematopoietic_tissue,
                                            primary.diagnosis.Benign_neoplasms,
                                            primary.diagnosis.Carcinoma_in_situ,
                                            primary.diagnosis.Neoplasms_of_uncertain_behavior,
                                            primary.diagnosis.Neoplasms_of_unspecified_nature,
                                            primary.diagnosis.Malignant_neoplasm_of_skin,
                                            primary.diagnosis.endocrine_metabolic_immunity_disorders, 
                                            primary.diagnosis.diseases_of_blood,
                                            primary.diagnosis.psychiatric_diagnoses,
                                            primary.diagnosis.Organic_psychotic_conditions,
                                            primary.diagnosis.Senile_and_presenile_organic_psychotic_conditions,
                                            primary.diagnosis.Alcoholic_psychoses,
                                            primary.diagnosis.Drug_psychoses,
                                            primary.diagnosis.Transient_organic_psychotic_conditions,
                                            primary.diagnosis.Other_organic_psychotic_conditions_chronic,
                                            primary.diagnosis.Other_psychoses,
                                            primary.diagnosis.Schizophrenic_psychoses,
                                            primary.diagnosis.Affective_psychoses,
                                            primary.diagnosis.Paranoid_states,
                                            primary.diagnosis.Other_nonorganic_psychoses,
                                            primary.diagnosis.Psychoses_with_origin_specific_to_childhood,
                                            primary.diagnosis.Neurotic_disorders_personality_disorders_and_nonpsychotic_mental_disorders,
                                            primary.diagnosis.Neurotic_disorders,
                                            primary.diagnosis.Personality_disorders,
                                            primary.diagnosis.Sexual_deviations,
                                            primary.diagnosis.Psychoactive_substance,
                                            primary.diagnosis.Alcohol_dependence_syndrome,
                                            primary.diagnosis.Drug_dependence,
                                            primary.diagnosis.Nondependent_aduse_of_drugs,
                                            primary.diagnosis.Other_primarily_adult_onset,
                                            primary.diagnosis.Physiological_malfunction_arising_from_mental_factors,
                                            primary.diagnosis.Special_symptoms_or_syndromes_not_elsewhere_classified,
                                            primary.diagnosis.Acute_reaction_to_stress,
                                            primary.diagnosis.Adjustment_reaction,
                                            primary.diagnosis.Specific_nonpsychotic_mental_disorders_following_brain_damage,
                                            primary.diagnosis.Depressive_disorder_not_elsewhere_classified,
                                            primary.diagnosis.Mental_disorders_childhood,
                                            primary.diagnosis.Disturbance_of_conduct_not_elsewhere_classified,
                                            primary.diagnosis.Disturbance_of_emotions_specific_to_childhood_and_adolescence,
                                            primary.diagnosis.Hyperkinetic_syndrome_of_childhood,
                                            primary.diagnosis.Specific_delays_in_development,
                                            primary.diagnosis.Psychic_factors,
                                            primary.diagnosis.Mental_retardation,
                                            primary.diagnosis.diseases_of_nervous_system, 
                                            primary.diagnosis.diseases_of_circulatory_system,
                                            primary.diagnosis.Acute_rheumatic_fever,
                                            primary.diagnosis.Chronic_rheumatic_heart_disease,
                                            primary.diagnosis.Hypertensive_disease,
                                            primary.diagnosis.Ischemic_heart_disease,
                                            primary.diagnosis.Diseases_of_pulmonary_circulation,
                                            primary.diagnosis.Other_heart_diseases,
                                            primary.diagnosis.Cerebrovascular_disease,
                                            primary.diagnosis.Diseases_of_arteries_and_capillaries,
                                            primary.diagnosis.Diseases_of_veins_and_lymphatics_and_other,
                                            primary.diagnosis.diseases_of_respiratory_system,
                                            primary.diagnosis.Acute_respiratory_infections,
                                            primary.diagnosis.Other_diseases_of_the_upper_respiratory_tract,
                                            primary.diagnosis.Pneumonia_and_influenza,
                                            primary.diagnosis.Chronic_obstructive_pulmonary_disease_and_allied_conditions,
                                            primary.diagnosis.Pneumoconioses_and_other_lung_diseases_due_to_external_agents,
                                            primary.diagnosis.Other_diseases_of_respiratory_system,
                                            primary.diagnosis.diseases_of_digestive_system, 
                                            primary.diagnosis.diseases_of_genitourinary_system,
                                            primary.diagnosis.Nephritis_nephrotic_sydrome_and_nephrosis,
                                            primary.diagnosis.Other_diseases_of_urinary_system,
                                            primary.diagnosis.Diseases_of_breast,
                                            primary.diagnosis.complicatoins_of_pregnancy_and_childbirth, 
                                            primary.diagnosis.diseases_of_skin,
                                            primary.diagnosis.Infections_of_skin,
                                            primary.diagnosis.Inflammatory_conditions_of_skin,
                                            primary.diagnosis.Psoriasis_and_similar_disorders,
                                            primary.diagnosis.Other_diseases_of_skin,
                                            primary.diagnosis.diseases_of_musculoskeletal_and_connective_tissue, 
                                            primary.diagnosis.congenital_anomalies, 
                                            primary.diagnosis.conditions_of_perinatal_period, 
                                            primary.diagnosis.ill_defined_conditions, 
                                            primary.diagnosis.injury_and_poisoning,
                                            primary.diagnosis.Diseases_of_male_genital_organs,
                                            primary.diagnosis.Inflammatory_disease_of_female_pelvic_organs,
                                            primary.diagnosis.Disorders_of_female_genital_tract)





#####Grouping procedures#####
#Simplfy the procedures into their ICD 9 groups
data2$PROCEDU1 <- substr(data2$PROCEDU1,0,2)
data2$PROCEDU2 <- substr(data2$PROCEDU2,0,2)
data2$PROCEDU3 <- substr(data2$PROCEDU3,0,2)
data2$PROCEDU4 <- substr(data2$PROCEDU4,0,2)


#Remove the dashes
data2$PROCEDU1 <- gsub("-", "", data2$PROCEDU1)
data2$PROCEDU2 <- gsub("-", "", data2$PROCEDU2)
data2$PROCEDU3 <- gsub("-", "", data2$PROCEDU3)
data2$PROCEDU4 <- gsub("-", "", data2$PROCEDU4)

#Again for double dashes
data2$PROCEDU1 <- gsub("-", "", data2$PROCEDU1)
data2$PROCEDU2 <- gsub("-", "", data2$PROCEDU2)
data2$PROCEDU3 <- gsub("-", "", data2$PROCEDU3)
data2$PROCEDU4 <- gsub("-", "", data2$PROCEDU4)

#As numeric
data2$PROCEDU1 <- as.numeric(data2$PROCEDU1)
data2$PROCEDU2 <- as.numeric(data2$PROCEDU2)
data2$PROCEDU3 <- as.numeric(data2$PROCEDU3)
data2$PROCEDU4 <- as.numeric(data2$PROCEDU4)




Operations_on_the_nervous_system <-
  data2 %>%
  filter((PROCEDU1 > 0 & PROCEDU1  < 6)| 
           (PROCEDU2  > 0 & PROCEDU2 < 6)| 
           (PROCEDU3  > 0 & PROCEDU3 < 6)|
           (PROCEDU4  > 0 & PROCEDU4 < 6))


Operations_on_the_endocrine_system <-
  data2 %>%
  filter((PROCEDU1 > 5 & PROCEDU1  < 8)| 
           (PROCEDU2  > 5 & PROCEDU2 < 8)| 
           (PROCEDU3  > 5 & PROCEDU3 < 8)|
           (PROCEDU4  > 5 & PROCEDU4 < 8))


Operations_on_the_eye <-
  data2 %>%
  filter((PROCEDU1 > 7 & PROCEDU1  < 17)| 
           (PROCEDU2  > 7 & PROCEDU2 < 17)| 
           (PROCEDU3  > 7 & PROCEDU3 < 17)|
           (PROCEDU4  > 7 & PROCEDU4 < 17))


Operations_on_the_ear <-
  data2 %>%
  filter((PROCEDU1 > 17 & PROCEDU1  < 21)| 
           (PROCEDU2  > 17 & PROCEDU2 < 21)| 
           (PROCEDU3  > 17 & PROCEDU3 < 21)|
           (PROCEDU4  > 17 & PROCEDU4 < 21))


Operations_on_the_nose_mouth_pharynx <-
  data2 %>%
  filter((PROCEDU1 > 20 & PROCEDU1  < 30)| 
           (PROCEDU2  > 20 & PROCEDU2 < 30)| 
           (PROCEDU3  > 20 & PROCEDU3 < 30)|
           (PROCEDU4  > 20 & PROCEDU4 < 30))


Operations_on_the_respiratory_system <-
  data2 %>%
  filter((PROCEDU1 > 29 & PROCEDU1  < 35)| 
           (PROCEDU2  > 29 & PROCEDU2 < 35)| 
           (PROCEDU3  > 29 & PROCEDU3 < 35)|
           (PROCEDU4  > 29 & PROCEDU4 < 35))


Operations_on_the_cardiovascular_system <-
  data2 %>%
  filter((PROCEDU1 > 34 & PROCEDU1  < 40)| 
           (PROCEDU2  > 34 & PROCEDU2 < 40)| 
           (PROCEDU3  > 34 & PROCEDU3 < 40)|
           (PROCEDU4  > 34 & PROCEDU4 < 40))


Operations_on_the_hemic_and_lymphatic_system <-
  data2 %>%
  filter((PROCEDU1 > 39 & PROCEDU1  < 42)| 
           (PROCEDU2  > 39 & PROCEDU2 < 42)| 
           (PROCEDU3  > 39 & PROCEDU3 < 42)|
           (PROCEDU4  > 39 & PROCEDU4 < 42))


Operations_on_the_digestive_system <-
  data2 %>%
  filter((PROCEDU1 > 41 & PROCEDU1  < 55)| 
           (PROCEDU2  > 41 & PROCEDU2 < 55)| 
           (PROCEDU3  > 41 & PROCEDU3 < 55)|
           (PROCEDU4  > 41 & PROCEDU4 < 55))


Operations_on_the_urinary_system <-
  data2 %>%
  filter((PROCEDU1 > 54 & PROCEDU1  < 60)| 
           (PROCEDU2  > 54 & PROCEDU2 < 60)| 
           (PROCEDU3  > 54 & PROCEDU3 < 60)|
           (PROCEDU4  > 54 & PROCEDU4 < 60))


Operations_on_the_male_genital_organs <-
  data2 %>%
  filter((PROCEDU1 > 59 & PROCEDU1  < 65)| 
           (PROCEDU2  > 59 & PROCEDU2 < 65)| 
           (PROCEDU3  > 59 & PROCEDU3 < 65)|
           (PROCEDU4  > 59 & PROCEDU4 < 65))


Operations_on_the_female_genital_organs <-
  data2 %>%
  filter((PROCEDU1 > 64 & PROCEDU1  < 72)| 
           (PROCEDU2  > 64 & PROCEDU2 < 72)| 
           (PROCEDU3  > 64 & PROCEDU3 < 72)|
           (PROCEDU4  > 64 & PROCEDU4 < 72))


Obstetrical_procedures <-
  data2 %>%
  filter((PROCEDU1 > 71 & PROCEDU1  < 76)| 
           (PROCEDU2  > 71 & PROCEDU2 < 76)| 
           (PROCEDU3  > 71 & PROCEDU3 < 76)|
           (PROCEDU4  > 71 & PROCEDU4 < 76))


Operations_on_the_musculoskeletal_system <-
  data2 %>%
  filter((PROCEDU1 > 75 & PROCEDU1  < 85)| 
           (PROCEDU2  > 75 & PROCEDU2 < 85)| 
           (PROCEDU3  > 75 & PROCEDU3 < 85)|
           (PROCEDU4  > 75 & PROCEDU4 < 85))


Operations_on_the_integumentary_system <-
  data2 %>%
  filter((PROCEDU1 > 84 & PROCEDU1  < 87)| 
           (PROCEDU2  > 84 & PROCEDU2 < 87)| 
           (PROCEDU3  > 84 & PROCEDU3 < 87)|
           (PROCEDU4  > 84 & PROCEDU4 < 87))


Miscellaneous_diagnostic_and_therapeutic_procedures <-
  data2 %>%
  filter((PROCEDU1 > 86)| 
           (PROCEDU2  > 86)| 
           (PROCEDU3  > 86)|
           (PROCEDU4  > 86))


Procedures_related_to_psyche <-
  data2 %>%
  filter((PROCEDU1 > 93 & PROCEDU1  < 95)| 
           (PROCEDU2  > 93 & PROCEDU2 < 95)| 
           (PROCEDU3  > 93 & PROCEDU3 < 95)|
           (PROCEDU4  > 93 & PROCEDU4 < 95))


procedure_groups <- list(Operations_on_the_nervous_system,
                         Operations_on_the_endocrine_system,
                         Operations_on_the_eye,
                         Operations_on_the_ear,
                         Operations_on_the_nose_mouth_pharynx,
                         Operations_on_the_respiratory_system,
                         Operations_on_the_cardiovascular_system,
                         Operations_on_the_hemic_and_lymphatic_system,
                         Operations_on_the_digestive_system,
                         Operations_on_the_urinary_system,
                         Operations_on_the_male_genital_organs,
                         Operations_on_the_female_genital_organs,
                         Obstetrical_procedures,
                         Operations_on_the_musculoskeletal_system,
                         Operations_on_the_integumentary_system,
                         Miscellaneous_diagnostic_and_therapeutic_procedures,
                         Procedures_related_to_psyche)











##Primary diagnosis
#Large scale (!!!!!unnecessary!!!!)
icd10.Organic_mental_disorders <- data2 %>%
  filter(DIAGNOS1.icd10 == "F00"|
           DIAGNOS1.icd10 == "F01"|
           DIAGNOS1.icd10 == "F02"|
           DIAGNOS1.icd10 == "F03"|
           DIAGNOS1.icd10 == "F04"|
           DIAGNOS1.icd10 == "F05"|
           DIAGNOS1.icd10 == "F06"|
           DIAGNOS1.icd10 == "F07"|
           DIAGNOS1.icd10 == "F08"|
           DIAGNOS1.icd10 == "F09")

icd10.Mental_and_behavioural_disorders_from_substance_use <- data2 %>%
  filter(DIAGNOS1.icd10 == "F10"|
           DIAGNOS1.icd10 == "F11"|
           DIAGNOS1.icd10 == "F12"|
           DIAGNOS1.icd10 == "F13"|
           DIAGNOS1.icd10 == "F14"|
           DIAGNOS1.icd10 == "F15"|
           DIAGNOS1.icd10 == "F16"|
           DIAGNOS1.icd10 == "F17"|
           DIAGNOS1.icd10 == "F18"|
           DIAGNOS1.icd10 == "F19")

icd10.Schizophrenia_schizotypal_and_delusional_disorders <- data2 %>%
  filter(DIAGNOS1.icd10 == "F20"|
           DIAGNOS1.icd10 == "F21"|
           DIAGNOS1.icd10 == "F22"|
           DIAGNOS1.icd10 == "F23"|
           DIAGNOS1.icd10 == "F24"|
           DIAGNOS1.icd10 == "F25"|
           DIAGNOS1.icd10 == "F26"|
           DIAGNOS1.icd10 == "F27"|
           DIAGNOS1.icd10 == "F28"|
           DIAGNOS1.icd10 == "F29")

icd10.Mood_affective_disorders <- data2 %>%
  filter(DIAGNOS1.icd10 == "F30"|
           DIAGNOS1.icd10 == "F31"|
           DIAGNOS1.icd10 == "F32"|
           DIAGNOS1.icd10 == "F33"|
           DIAGNOS1.icd10 == "F34"|
           DIAGNOS1.icd10 == "F35"|
           DIAGNOS1.icd10 == "F36"|
           DIAGNOS1.icd10 == "F37"|
           DIAGNOS1.icd10 == "F38"|
           DIAGNOS1.icd10 == "F39")

icd10.Neurotic_stress_related_and_somatoform_disorders <- data2 %>%
  filter(DIAGNOS1.icd10 == "F40"|
           DIAGNOS1.icd10 == "F41"|
           DIAGNOS1.icd10 == "F42"|
           DIAGNOS1.icd10 == "F43"|
           DIAGNOS1.icd10 == "F44"|
           DIAGNOS1.icd10 == "F45"|
           DIAGNOS1.icd10 == "F46"|
           DIAGNOS1.icd10 == "F47"|
           DIAGNOS1.icd10 == "F48")

icd10.Behavioural_syndromes_associated_with_physiological_disturbances_and_physical_factors <- data2 %>%
  filter(DIAGNOS1.icd10 == "F50"|
           DIAGNOS1.icd10 == "F51"|
           DIAGNOS1.icd10 == "F52"|
           DIAGNOS1.icd10 == "F53"|
           DIAGNOS1.icd10 == "F54"|
           DIAGNOS1.icd10 == "F55"|
           DIAGNOS1.icd10 == "F56"|
           DIAGNOS1.icd10 == "F57"|
           DIAGNOS1.icd10 == "F58"|
           DIAGNOS1.icd10 == "F59")

icd10.Disorders_of_adult_personality_and_behaviour <- data2 %>%
  filter(DIAGNOS1.icd10 == "F60"|
           DIAGNOS1.icd10 == "F61"|
           DIAGNOS1.icd10 == "F62"|
           DIAGNOS1.icd10 == "F63"|
           DIAGNOS1.icd10 == "F64"|
           DIAGNOS1.icd10 == "F65"|
           DIAGNOS1.icd10 == "F66"|
           DIAGNOS1.icd10 == "F67"|
           DIAGNOS1.icd10 == "F68"|
           DIAGNOS1.icd10 == "F69")

icd10.Mental_retardation <- data2 %>%
  filter(DIAGNOS1.icd10 == "F70"|
           DIAGNOS1.icd10 == "F71"|
           DIAGNOS1.icd10 == "F72"|
           DIAGNOS1.icd10 == "F73"|
           DIAGNOS1.icd10 == "F74"|
           DIAGNOS1.icd10 == "F75"|
           DIAGNOS1.icd10 == "F76"|
           DIAGNOS1.icd10 == "F77"|
           DIAGNOS1.icd10 == "F78"|
           DIAGNOS1.icd10 == "F79")

icd10.Disorders_of_psychological_development <- data2 %>%
  filter(DIAGNOS1.icd10 == "F80"|
           DIAGNOS1.icd10 == "F81"|
           DIAGNOS1.icd10 == "F82"|
           DIAGNOS1.icd10 == "F83"|
           DIAGNOS1.icd10 == "F84"|
           DIAGNOS1.icd10 == "F85"|
           DIAGNOS1.icd10 == "F86"|
           DIAGNOS1.icd10 == "F87"|
           DIAGNOS1.icd10 == "F88"|
           DIAGNOS1.icd10 == "F89")

icd10.Behavioural_and_emotional_disorders_with_onset_usually_occuring_in_childhood <- data2 %>%
  filter(DIAGNOS1.icd10 == "F90"|
           DIAGNOS1.icd10 == "F91"|
           DIAGNOS1.icd10 == "F92"|
           DIAGNOS1.icd10 == "F93"|
           DIAGNOS1.icd10 == "F94"|
           DIAGNOS1.icd10 == "F95"|
           DIAGNOS1.icd10 == "F96"|
           DIAGNOS1.icd10 == "F97"|
           DIAGNOS1.icd10 == "F98")

icd10.Disorders_of_psychological_development <- data2 %>%
  filter(DIAGNOS1.icd10 == "F80"|
           DIAGNOS1.icd10 == "F81"|
           DIAGNOS1.icd10 == "F82"|
           DIAGNOS1.icd10 == "F83"|
           DIAGNOS1.icd10 == "F84"|
           DIAGNOS1.icd10 == "F85"|
           DIAGNOS1.icd10 == "F86"|
           DIAGNOS1.icd10 == "F87"|
           DIAGNOS1.icd10 == "F88"|
           DIAGNOS1.icd10 == "F89")





#######



#####ICD10 diagnoses#####
#subsets of the mood affective disorders
primary.diagnosis.icd10.Manic_episode <- data2 %>%
  filter(DIAGNOS1.icd10 == "F30")

primary.diagnosis.icd10.Bipolar_affective_disorder <- data2 %>%
  filter(DIAGNOS1.icd10 == "F31")

primary.diagnosis.icd10.Depressive_episode <- data2 %>%
  filter(DIAGNOS1.icd10 == "F32")

primary.diagnosis.icd10.Recurrent_depressive_disorder <- data2 %>%
  filter(DIAGNOS1.icd10 == "F33")

primary.diagnosis.icd10.Persistent_mood_affective_disorders <- data2 %>%
  filter(DIAGNOS1.icd10 == "F34")

primary.diagnosis.icd10.Other_mood_affective_disorders <- data2 %>%
  filter(DIAGNOS1.icd10 == "F38")


primary.diagnosis.ICD_10_code_groups <- list(primary.diagnosis.icd10.Manic_episode,
                                             primary.diagnosis.icd10.Bipolar_affective_disorder,
                                             primary.diagnosis.icd10.Depressive_episode,
                                             primary.diagnosis.icd10.Recurrent_depressive_disorder,
                                             primary.diagnosis.icd10.Persistent_mood_affective_disorders,
                                             primary.diagnosis.icd10.Other_mood_affective_disorders)



#All diagnosis
library(data.table)

icd10.Manic_episode <- data2[data2[,30:36] %like% "F30", ]


icd10.Bipolar_affective_disorder <- data2[data2[,30:36] %like% "F31", ]


icd10.Depressive_episode <- data2[data2[,30:36] %like% "F32", ]



icd10.Recurrent_depressive_disorder <- data2[data2[,30:36] %like% "F33", ]



icd10.Persistent_mood_affective_disorders <- data2[data2[,30:36] %like% "F34", ]




icd10.Other_mood_affective_disorders <- data2[data2[,30:36] %like% "F38", ]





ICD_10_code_groups_psychiatric <- list(icd10.Manic_episode,
                           icd10.Bipolar_affective_disorder,
                           icd10.Depressive_episode,
                           icd10.Recurrent_depressive_disorder,
                           icd10.Persistent_mood_affective_disorders,
                           icd10.Other_mood_affective_disorders)


