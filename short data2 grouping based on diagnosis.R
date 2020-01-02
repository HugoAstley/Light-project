#grouping based on diagnoses- short
save(ICD_9_code_groups.short, file="ICD_9_code_groups.short.RData")

#Note: complications of childbirth have been removed because it is female only and in the model sex is a factor

infectious_and_parasitic_diseases.short <-
  data2.short %>%
  filter((DIAGNOS1 > 000 & DIAGNOS1 <140)| 
           (DIAGNOS2 > 000 & DIAGNOS2 <140)| 
           (DIAGNOS3 > 000 & DIAGNOS3 <140)|
           (DIAGNOS4 > 000 & DIAGNOS4 <140)|
           (DIAGNOS5 > 000 & DIAGNOS5 <140)|
           (DIAGNOS6 > 000 & DIAGNOS6 <140)|
           (DIAGNOS7 > 000 & DIAGNOS7 <140))

neoplasms.short <-
  data2.short %>%
  filter((DIAGNOS1 > 139 & DIAGNOS1 <240)| 
           (DIAGNOS2 > 139 & DIAGNOS2 <240)| 
           (DIAGNOS3 > 139 & DIAGNOS3 <240)|
           (DIAGNOS4 > 139 & DIAGNOS4 <240)|
           (DIAGNOS5 > 139 & DIAGNOS5 <240)|
           (DIAGNOS6 > 139 & DIAGNOS6 <240)|
           (DIAGNOS7 > 139 & DIAGNOS7 <240))

endocrine_metabolic_immunity_disorders.short <-
  data2.short %>%
  filter((DIAGNOS1 > 239 & DIAGNOS1 <280)| 
           (DIAGNOS2 > 239 & DIAGNOS2 <280)| 
           (DIAGNOS3 > 239 & DIAGNOS3 <280)|
           (DIAGNOS4 > 239 & DIAGNOS4 <280)|
           (DIAGNOS5 > 239 & DIAGNOS5 <280)|
           (DIAGNOS6 > 239 & DIAGNOS6 <280)|
           (DIAGNOS7 > 239 & DIAGNOS7 <280))

diseases_of_blood.short <-
  data2.short %>%
  filter((DIAGNOS1 > 279 & DIAGNOS1 <290)| 
           (DIAGNOS2 > 279 & DIAGNOS2 <290)| 
           (DIAGNOS3 > 279 & DIAGNOS3 <290)|
           (DIAGNOS4 > 279 & DIAGNOS4 <290)|
           (DIAGNOS5 > 279 & DIAGNOS5 <290)|
           (DIAGNOS6 > 279 & DIAGNOS6 <290)|
           (DIAGNOS7 > 279 & DIAGNOS7 <290))

psychiatric_diagnoses.short <-
  data2.short %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <320)| 
           (DIAGNOS2 > 289 & DIAGNOS2 <320)| 
           (DIAGNOS3 > 289 & DIAGNOS3 <320)|
           (DIAGNOS4 > 289 & DIAGNOS4 <320)|
           (DIAGNOS5 > 289 & DIAGNOS5 <320)|
           (DIAGNOS6 > 289 & DIAGNOS6 <320)|
           (DIAGNOS7 > 289 & DIAGNOS7 <320))

diseases_of_nervous_system.short <-
  data2.short %>%
  filter((DIAGNOS1 > 319 & DIAGNOS1 <390)| 
           (DIAGNOS2 > 319 & DIAGNOS2 <390)| 
           (DIAGNOS3 > 319 & DIAGNOS3 <390)|
           (DIAGNOS4 > 319 & DIAGNOS4 <390)|
           (DIAGNOS5 > 319 & DIAGNOS5 <390)|
           (DIAGNOS6 > 319 & DIAGNOS6 <390)|
           (DIAGNOS7 > 319 & DIAGNOS7 <390))

diseases_of_circulatory_system.short <-
  data2.short %>%
  filter((DIAGNOS1 > 389 & DIAGNOS1 <460)| 
           (DIAGNOS2 > 389 & DIAGNOS2 <460)| 
           (DIAGNOS3 > 389 & DIAGNOS3 <460)|
           (DIAGNOS4 > 389 & DIAGNOS4 <460)|
           (DIAGNOS5 > 389 & DIAGNOS5 <460)|
           (DIAGNOS6 > 389 & DIAGNOS6 <460)|
           (DIAGNOS7 > 389 & DIAGNOS7 <460))

diseases_of_respiratory_system.short <-
  data2.short %>%
  filter((DIAGNOS1 > 459 & DIAGNOS1 <520)| 
           (DIAGNOS2 > 459 & DIAGNOS2 <520)| 
           (DIAGNOS3 > 459 & DIAGNOS3 <520)|
           (DIAGNOS4 > 459 & DIAGNOS4 <520)|
           (DIAGNOS5 > 459 & DIAGNOS5 <520)|
           (DIAGNOS6 > 459 & DIAGNOS6 <520)|
           (DIAGNOS7 > 459 & DIAGNOS7 <520))

diseases_of_digestive_system.short <-
  data2.short %>%
  filter((DIAGNOS1 > 519 & DIAGNOS1 <580)| 
           (DIAGNOS2 > 519 & DIAGNOS2 <580)| 
           (DIAGNOS3 > 519 & DIAGNOS3 <580)|
           (DIAGNOS4 > 519 & DIAGNOS4 <580)|
           (DIAGNOS5 > 519 & DIAGNOS5 <580)|
           (DIAGNOS6 > 519 & DIAGNOS6 <580)|
           (DIAGNOS7 > 519 & DIAGNOS7 <580))

diseases_of_genitourinary_system.short <-
  data2.short %>%
  filter((DIAGNOS1 > 579 & DIAGNOS1 <630)| 
           (DIAGNOS2 > 579 & DIAGNOS2 <630)| 
           (DIAGNOS3 > 579 & DIAGNOS3 <630)|
           (DIAGNOS4 > 579 & DIAGNOS4 <630)|
           (DIAGNOS5 > 579 & DIAGNOS5 <630)|
           (DIAGNOS6 > 579 & DIAGNOS6 <630)|
           (DIAGNOS7 > 579 & DIAGNOS7 <630))

diseases_of_skin.short <-
  data2.short %>%
  filter((DIAGNOS1 > 679 & DIAGNOS1 <710)| 
           (DIAGNOS2 > 679 & DIAGNOS2 <710)| 
           (DIAGNOS3 > 679 & DIAGNOS3 <710)|
           (DIAGNOS4 > 679 & DIAGNOS4 <710)|
           (DIAGNOS5 > 679 & DIAGNOS5 <710)|
           (DIAGNOS6 > 679 & DIAGNOS6 <710)|
           (DIAGNOS7 > 679 & DIAGNOS7 <710))

diseases_of_musculoskeletal_and_connective_tissue.short <-
  data2.short %>%
  filter((DIAGNOS1 > 709 & DIAGNOS1 <740)| 
           (DIAGNOS2 > 709 & DIAGNOS2 <740)| 
           (DIAGNOS3 > 709 & DIAGNOS3 <740)|
           (DIAGNOS4 > 709 & DIAGNOS4 <740)|
           (DIAGNOS5 > 709 & DIAGNOS5 <740)|
           (DIAGNOS6 > 709 & DIAGNOS6 <740)|
           (DIAGNOS7 > 709 & DIAGNOS7 <740))

congenital_anomalies.short <-
  data2.short %>%
  filter((DIAGNOS1 > 739 & DIAGNOS1 <760)| 
           (DIAGNOS2 > 739 & DIAGNOS2 <760)| 
           (DIAGNOS3 > 739 & DIAGNOS3 <760)|
           (DIAGNOS4 > 739 & DIAGNOS4 <760)|
           (DIAGNOS5 > 739 & DIAGNOS5 <760)|
           (DIAGNOS6 > 739 & DIAGNOS6 <760)|
           (DIAGNOS7 > 739 & DIAGNOS7 <760))

conditions_of_perinatal_period.short <-
  data2.short %>%
  filter((DIAGNOS1 > 759 & DIAGNOS1 <780)| 
           (DIAGNOS2 > 759 & DIAGNOS2 <780)| 
           (DIAGNOS3 > 759 & DIAGNOS3 <780)|
           (DIAGNOS4 > 759 & DIAGNOS4 <780)|
           (DIAGNOS5 > 759 & DIAGNOS5 <780)|
           (DIAGNOS6 > 759 & DIAGNOS6 <780)|
           (DIAGNOS7 > 759 & DIAGNOS7 <780))

ill_defined_conditions.short <-
  data2.short %>%
  filter((DIAGNOS1 > 779 & DIAGNOS1 <800)| 
           (DIAGNOS2 > 779 & DIAGNOS2 <800)| 
           (DIAGNOS3 > 779 & DIAGNOS3 <800)|
           (DIAGNOS4 > 779 & DIAGNOS4 <800)|
           (DIAGNOS5 > 779 & DIAGNOS5 <800)|
           (DIAGNOS6 > 779 & DIAGNOS6 <800)|
           (DIAGNOS7 > 779 & DIAGNOS7 <800))


injury_and_poisoning.short <-
  data2.short %>%
  filter((DIAGNOS1 > 799 & DIAGNOS1 <= 999)| 
           (DIAGNOS2 > 799 & DIAGNOS2 <= 999)| 
           (DIAGNOS3 > 799 & DIAGNOS3 <= 999)|
           (DIAGNOS4 > 799 & DIAGNOS4 <= 999)|
           (DIAGNOS5 > 799 & DIAGNOS5 <= 999)|
           (DIAGNOS6 > 799 & DIAGNOS6 <= 999)|
           (DIAGNOS7 > 799 & DIAGNOS7 <= 999))



ICD_9_code_groups.short <- list(infectious_and_parasitic_diseases.short,
                          neoplasms.short,
                          endocrine_metabolic_immunity_disorders.short, 
                          diseases_of_blood.short, 
                          psychiatric_diagnoses.short, 
                          diseases_of_nervous_system.short, 
                          diseases_of_circulatory_system.short, 
                          diseases_of_respiratory_system.short, 
                          diseases_of_digestive_system.short, 
                          diseases_of_genitourinary_system.short, 
                          diseases_of_skin.short,
                          diseases_of_musculoskeletal_and_connective_tissue.short,
                          congenital_anomalies.short, 
                          conditions_of_perinatal_period.short, 
                          ill_defined_conditions.short,
                          injury_and_poisoning.short)


################################
complicatoins_of_pregnancy_and_childbirth.short,
complicatoins_of_pregnancy_and_childbirth.short <-
  data2.short %>%
  filter((DIAGNOS1 > 629 & DIAGNOS1 <680)| 
           (DIAGNOS2 > 629 & DIAGNOS2 <680)| 
           (DIAGNOS3 > 629 & DIAGNOS3 <680)|
           (DIAGNOS4 > 629 & DIAGNOS4 <680)|
           (DIAGNOS5 > 629 & DIAGNOS5 <680)|
           (DIAGNOS6 > 629 & DIAGNOS6 <680)|
           (DIAGNOS7 > 629 & DIAGNOS7 <680))

for(i in 1:length(ICD_9_code_groups.short)){print(levels(ICD_9_code_groups.short[[i]]$SEX))}
################################


#Being more specific on the psychiatric diagnoses
Organic_psychotic_conditions.short <-
  data2.short %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <295)| 
           (DIAGNOS2 > 289 & DIAGNOS2 <295)| 
           (DIAGNOS3 > 289 & DIAGNOS3 <295)|
           (DIAGNOS4 > 289 & DIAGNOS4 <295)|
           (DIAGNOS5 > 289 & DIAGNOS5 <295)|
           (DIAGNOS6 > 289 & DIAGNOS6 <295)|
           (DIAGNOS7 > 289 & DIAGNOS7 <295))


Senile_and_presenile_organic_psychotic_conditions.short <-
  data2.short %>%
  filter((DIAGNOS1 > 289 & DIAGNOS1 <291)| 
           (DIAGNOS2 > 289 & DIAGNOS2 <291)| 
           (DIAGNOS3 > 289 & DIAGNOS3 <291)|
           (DIAGNOS4 > 289 & DIAGNOS4 <291)|
           (DIAGNOS5 > 289 & DIAGNOS5 <291)|
           (DIAGNOS6 > 289 & DIAGNOS6 <291)|
           (DIAGNOS7 > 289 & DIAGNOS7 <291))



Alcoholic_psychoses.short <-
  data2.short %>%
  filter((DIAGNOS1 > 290 & DIAGNOS1 <292)| 
           (DIAGNOS2 > 290 & DIAGNOS2 <292)| 
           (DIAGNOS3 > 290 & DIAGNOS3 <292)|
           (DIAGNOS4 > 290 & DIAGNOS4 <292)|
           (DIAGNOS5 > 290 & DIAGNOS5 <292)|
           (DIAGNOS6 > 290 & DIAGNOS6 <292)|
           (DIAGNOS7 > 290 & DIAGNOS7 <292))



Drug_psychoses.short <-
  data2.short %>%
  filter((DIAGNOS1 > 291 & DIAGNOS1 <293)| 
           (DIAGNOS2 > 291 & DIAGNOS2 <293)| 
           (DIAGNOS3 > 291 & DIAGNOS3 <293)|
           (DIAGNOS4 > 291 & DIAGNOS4 <293)|
           (DIAGNOS5 > 291 & DIAGNOS5 <293)|
           (DIAGNOS6 > 291 & DIAGNOS6 <293)|
           (DIAGNOS7 > 291 & DIAGNOS7 <293))


Transient_organic_psychotic_conditions.short <-
  data2.short %>%
  filter((DIAGNOS1 > 292 & DIAGNOS1 <294)| 
           (DIAGNOS2 > 292 & DIAGNOS2 <294)| 
           (DIAGNOS3 > 292 & DIAGNOS3 <294)|
           (DIAGNOS4 > 292 & DIAGNOS4 <294)|
           (DIAGNOS5 > 292 & DIAGNOS5 <294)|
           (DIAGNOS6 > 292 & DIAGNOS6 <294)|
           (DIAGNOS7 > 292 & DIAGNOS7 <294))


Other_organic_psychotic_conditions_chronic.short <-
  data2.short %>%
  filter((DIAGNOS1 > 293 & DIAGNOS1 <295)| 
           (DIAGNOS2 > 293 & DIAGNOS2 <295)| 
           (DIAGNOS3 > 293 & DIAGNOS3 <295)|
           (DIAGNOS4 > 293 & DIAGNOS4 <295)|
           (DIAGNOS5 > 293 & DIAGNOS5 <295)|
           (DIAGNOS6 > 293 & DIAGNOS6 <295)|
           (DIAGNOS7 > 293 & DIAGNOS7 <295))


Other_psychoses.short <-
  data2.short %>%
  filter((DIAGNOS1 > 294 & DIAGNOS1 <300)| 
           (DIAGNOS2 > 294 & DIAGNOS2 <300)| 
           (DIAGNOS3 > 294 & DIAGNOS3 <300)|
           (DIAGNOS4 > 294 & DIAGNOS4 <300)|
           (DIAGNOS5 > 294 & DIAGNOS5 <300)|
           (DIAGNOS6 > 294 & DIAGNOS6 <300)|
           (DIAGNOS7 > 294 & DIAGNOS7 <300))



Schizophrenic_psychoses.short <-
  data2.short %>%
  filter((DIAGNOS1 > 294 & DIAGNOS1 <296)| 
           (DIAGNOS2 > 294 & DIAGNOS2 <296)| 
           (DIAGNOS3 > 294 & DIAGNOS3 <296)|
           (DIAGNOS4 > 294 & DIAGNOS4 <296)|
           (DIAGNOS5 > 294 & DIAGNOS5 <296)|
           (DIAGNOS6 > 294 & DIAGNOS6 <296)|
           (DIAGNOS7 > 294 & DIAGNOS7 <296))




Affective_psychoses.short <-
  data2.short %>%
  filter((DIAGNOS1 > 295 & DIAGNOS1 <297)| 
           (DIAGNOS2 > 295 & DIAGNOS2 <297)| 
           (DIAGNOS3 > 295 & DIAGNOS3 <297)|
           (DIAGNOS4 > 295 & DIAGNOS4 <297)|
           (DIAGNOS5 > 295 & DIAGNOS5 <297)|
           (DIAGNOS6 > 295 & DIAGNOS6 <297)|
           (DIAGNOS7 > 295 & DIAGNOS7 <297))


Paranoid_states.short <-
  data2.short %>%
  filter((DIAGNOS1 > 296 & DIAGNOS1 <298)| 
           (DIAGNOS2 > 296 & DIAGNOS2 <298)| 
           (DIAGNOS3 > 296 & DIAGNOS3 <298)|
           (DIAGNOS4 > 296 & DIAGNOS4 <298)|
           (DIAGNOS5 > 296 & DIAGNOS5 <298)|
           (DIAGNOS6 > 296 & DIAGNOS6 <298)|
           (DIAGNOS7 > 296 & DIAGNOS7 <298))


Other_nonorganic_psychoses.short <-
  data2.short %>%
  filter((DIAGNOS1 > 297 & DIAGNOS1 <299)| 
           (DIAGNOS2 > 297 & DIAGNOS2 <299)| 
           (DIAGNOS3 > 297 & DIAGNOS3 <299)|
           (DIAGNOS4 > 297 & DIAGNOS4 <299)|
           (DIAGNOS5 > 297 & DIAGNOS5 <299)|
           (DIAGNOS6 > 297 & DIAGNOS6 <299)|
           (DIAGNOS7 > 297 & DIAGNOS7 <299))



Psychoses_with_origin_specific_to_childhood.short <-
  data2.short %>%
  filter((DIAGNOS1 > 298 & DIAGNOS1 <300)| 
           (DIAGNOS2 > 298 & DIAGNOS2 <300)| 
           (DIAGNOS3 > 298 & DIAGNOS3 <300)|
           (DIAGNOS4 > 298 & DIAGNOS4 <300)|
           (DIAGNOS5 > 298 & DIAGNOS5 <300)|
           (DIAGNOS6 > 298 & DIAGNOS6 <300)|
           (DIAGNOS7 > 298 & DIAGNOS7 <300))




Neurotic_disorders_personality_disorders_and_nonpsychotic_mental_disorders.short <-
  data2.short %>%
  filter((DIAGNOS1 > 299 & DIAGNOS1 <317)| 
           (DIAGNOS2 > 299 & DIAGNOS2 <317)| 
           (DIAGNOS3 > 299 & DIAGNOS3 <317)|
           (DIAGNOS4 > 299 & DIAGNOS4 <317)|
           (DIAGNOS5 > 299 & DIAGNOS5 <317)|
           (DIAGNOS6 > 299 & DIAGNOS6 <317)|
           (DIAGNOS7 > 299 & DIAGNOS7 <317))


Neurotic_disorders.short <-
  data2.short %>%
  filter((DIAGNOS1 > 299 & DIAGNOS1 <301)| 
           (DIAGNOS2 > 299 & DIAGNOS2 <301)| 
           (DIAGNOS3 > 299 & DIAGNOS3 <301)|
           (DIAGNOS4 > 299 & DIAGNOS4 <301)|
           (DIAGNOS5 > 299 & DIAGNOS5 <301)|
           (DIAGNOS6 > 299 & DIAGNOS6 <301)|
           (DIAGNOS7 > 299 & DIAGNOS7 <301))


Personality_disorders.short <-
  data2.short %>%
  filter((DIAGNOS1 > 300 & DIAGNOS1 <302)| 
           (DIAGNOS2 > 300 & DIAGNOS2 <302)| 
           (DIAGNOS3 > 300 & DIAGNOS3 <302)|
           (DIAGNOS4 > 300 & DIAGNOS4 <302)|
           (DIAGNOS5 > 300 & DIAGNOS5 <302)|
           (DIAGNOS6 > 300 & DIAGNOS6 <302)|
           (DIAGNOS7 > 300 & DIAGNOS7 <302))

Sexual_deviations.short <-
  data2.short %>%
  filter((DIAGNOS1 > 301 & DIAGNOS1 <303)| 
           (DIAGNOS2 > 301 & DIAGNOS2 <303)| 
           (DIAGNOS3 > 301 & DIAGNOS3 <303)|
           (DIAGNOS4 > 301 & DIAGNOS4 <303)|
           (DIAGNOS5 > 301 & DIAGNOS5 <303)|
           (DIAGNOS6 > 301 & DIAGNOS6 <303)|
           (DIAGNOS7 > 301 & DIAGNOS7 <303))


Psychoactive_substance.short <-
  data2.short %>%
  filter((DIAGNOS1 > 302 & DIAGNOS1 <306)| 
           (DIAGNOS2 > 302 & DIAGNOS2 <306)| 
           (DIAGNOS3 > 302 & DIAGNOS3 <306)|
           (DIAGNOS4 > 302 & DIAGNOS4 <306)|
           (DIAGNOS5 > 302 & DIAGNOS5 <306)|
           (DIAGNOS6 > 302 & DIAGNOS6 <306)|
           (DIAGNOS7 > 302 & DIAGNOS7 <306))

Alcohol_dependence_syndrome.short <-
  data2.short %>%
  filter((DIAGNOS1 > 302 & DIAGNOS1 <304)| 
           (DIAGNOS2 > 302 & DIAGNOS2 <304)| 
           (DIAGNOS3 > 302 & DIAGNOS3 <304)|
           (DIAGNOS4 > 302 & DIAGNOS4 <304)|
           (DIAGNOS5 > 302 & DIAGNOS5 <304)|
           (DIAGNOS6 > 302 & DIAGNOS6 <304)|
           (DIAGNOS7 > 302 & DIAGNOS7 <304))


Drug_dependence.short <-
  data2.short %>%
  filter((DIAGNOS1 > 303 & DIAGNOS1 <305)| 
           (DIAGNOS2 > 303 & DIAGNOS2 <305)| 
           (DIAGNOS3 > 303 & DIAGNOS3 <305)|
           (DIAGNOS4 > 303 & DIAGNOS4 <305)|
           (DIAGNOS5 > 303 & DIAGNOS5 <305)|
           (DIAGNOS6 > 303 & DIAGNOS6 <305)|
           (DIAGNOS7 > 303 & DIAGNOS7 <305))


Nondependent_aduse_of_drugs.short <-
  data2.short %>%
  filter((DIAGNOS1 > 304 & DIAGNOS1 <306)| 
           (DIAGNOS2 > 304 & DIAGNOS2 <306)| 
           (DIAGNOS3 > 304 & DIAGNOS3 <306)|
           (DIAGNOS4 > 304 & DIAGNOS4 <306)|
           (DIAGNOS5 > 304 & DIAGNOS5 <306)|
           (DIAGNOS6 > 304 & DIAGNOS6 <306)|
           (DIAGNOS7 > 304 & DIAGNOS7 <306))


Other_primarily_adult_onset.short <-
  data2.short %>%
  filter((DIAGNOS1 > 305 & DIAGNOS1 < 312)| 
           (DIAGNOS2 > 305 & DIAGNOS2 < 312)| 
           (DIAGNOS3 > 305 & DIAGNOS3 < 312)|
           (DIAGNOS4 > 305 & DIAGNOS4 < 312)|
           (DIAGNOS5 > 305 & DIAGNOS5 < 312)|
           (DIAGNOS6 > 305 & DIAGNOS6 < 312)|
           (DIAGNOS7 > 305 & DIAGNOS7 < 312))


Physiological_malfunction_arising_from_mental_factors.short <-
  data2.short %>%
  filter((DIAGNOS1 > 305 & DIAGNOS1 < 307)| 
           (DIAGNOS2 > 305 & DIAGNOS2 < 307)| 
           (DIAGNOS3 > 305 & DIAGNOS3 < 307)|
           (DIAGNOS4 > 305 & DIAGNOS4 < 307)|
           (DIAGNOS5 > 305 & DIAGNOS5 < 307)|
           (DIAGNOS6 > 305 & DIAGNOS6 < 307)|
           (DIAGNOS7 > 305 & DIAGNOS7 < 307))


Special_symptoms_or_syndromes_not_elsewhere_classified.short <-
  data2.short %>%
  filter((DIAGNOS1 > 306 & DIAGNOS1 < 308)| 
           (DIAGNOS2 > 306 & DIAGNOS2 < 308)| 
           (DIAGNOS3 > 306 & DIAGNOS3 < 308)|
           (DIAGNOS4 > 306 & DIAGNOS4 < 308)|
           (DIAGNOS5 > 306 & DIAGNOS5 < 308)|
           (DIAGNOS6 > 306 & DIAGNOS6 < 308)|
           (DIAGNOS7 > 306 & DIAGNOS7 < 308))


Acute_reaction_to_stress.short <-
  data2.short %>%
  filter((DIAGNOS1 > 307 & DIAGNOS1 < 309)| 
           (DIAGNOS2 > 307 & DIAGNOS2 < 309)| 
           (DIAGNOS3 > 307 & DIAGNOS3 < 309)|
           (DIAGNOS4 > 307 & DIAGNOS4 < 309)|
           (DIAGNOS5 > 307 & DIAGNOS5 < 309)|
           (DIAGNOS6 > 307 & DIAGNOS6 < 309)|
           (DIAGNOS7 > 307 & DIAGNOS7 < 309))


Adjustment_reaction.short <-
  data2.short %>%
  filter((DIAGNOS1 > 308 & DIAGNOS1 < 310)| 
           (DIAGNOS2 > 308 & DIAGNOS2 < 310)| 
           (DIAGNOS3 > 308 & DIAGNOS3 < 310)|
           (DIAGNOS4 > 308 & DIAGNOS4 < 310)|
           (DIAGNOS5 > 308 & DIAGNOS5 < 310)|
           (DIAGNOS6 > 308 & DIAGNOS6 < 310)|
           (DIAGNOS7 > 308 & DIAGNOS7 < 310))



Specific_nonpsychotic_mental_disorders_following_brain_damage.short <-
  data2.short %>%
  filter((DIAGNOS1 > 309 & DIAGNOS1 < 311)| 
           (DIAGNOS2 > 309 & DIAGNOS2 < 311)| 
           (DIAGNOS3 > 309 & DIAGNOS3 < 311)|
           (DIAGNOS4 > 309 & DIAGNOS4 < 311)|
           (DIAGNOS5 > 309 & DIAGNOS5 < 311)|
           (DIAGNOS6 > 309 & DIAGNOS6 < 311)|
           (DIAGNOS7 > 309 & DIAGNOS7 < 311))


Depressive_disorder_not_elsewhere_classified.short <-
  data2.short %>%
  filter((DIAGNOS1 > 310 & DIAGNOS1 < 312)| 
           (DIAGNOS2 > 310 & DIAGNOS2 < 312)| 
           (DIAGNOS3 > 310 & DIAGNOS3 < 312)|
           (DIAGNOS4 > 310 & DIAGNOS4 < 312)|
           (DIAGNOS5 > 310 & DIAGNOS5 < 312)|
           (DIAGNOS6 > 310 & DIAGNOS6 < 312)|
           (DIAGNOS7 > 310 & DIAGNOS7 < 312))


Mental_disorders_childhood.short <-
  data2.short %>%
  filter((DIAGNOS1 > 311 & DIAGNOS1 <317)| 
           (DIAGNOS2 > 311 & DIAGNOS2 <317)| 
           (DIAGNOS3 > 311 & DIAGNOS3 <317)|
           (DIAGNOS4 > 311 & DIAGNOS4 <317)|
           (DIAGNOS5 > 311 & DIAGNOS5 <317)|
           (DIAGNOS6 > 311 & DIAGNOS6 <317)|
           (DIAGNOS7 > 311 & DIAGNOS7 <317))


Disturbance_of_conduct_not_elsewhere_classified.short <-
  data2.short %>%
  filter((DIAGNOS1 > 311 & DIAGNOS1 <313)| 
           (DIAGNOS2 > 311 & DIAGNOS2 <313)| 
           (DIAGNOS3 > 311 & DIAGNOS3 <313)|
           (DIAGNOS4 > 311 & DIAGNOS4 <313)|
           (DIAGNOS5 > 311 & DIAGNOS5 <313)|
           (DIAGNOS6 > 311 & DIAGNOS6 <313)|
           (DIAGNOS7 > 311 & DIAGNOS7 <313))


Disturbance_of_emotions_specific_to_childhood_and_adolescence.short <-
  data2.short %>%
  filter((DIAGNOS1 > 312 & DIAGNOS1 <314)| 
           (DIAGNOS2 > 312 & DIAGNOS2 <314)| 
           (DIAGNOS3 > 312 & DIAGNOS3 <314)|
           (DIAGNOS4 > 312 & DIAGNOS4 <314)|
           (DIAGNOS5 > 312 & DIAGNOS5 <314)|
           (DIAGNOS6 > 312 & DIAGNOS6 <314)|
           (DIAGNOS7 > 312 & DIAGNOS7 <314))


Hyperkinetic_syndrome_of_childhood.short <-
  data2.short %>%
  filter((DIAGNOS1 > 313 & DIAGNOS1 <315)| 
           (DIAGNOS2 > 313 & DIAGNOS2 <315)| 
           (DIAGNOS3 > 313 & DIAGNOS3 <315)|
           (DIAGNOS4 > 313 & DIAGNOS4 <315)|
           (DIAGNOS5 > 313 & DIAGNOS5 <315)|
           (DIAGNOS6 > 313 & DIAGNOS6 <315)|
           (DIAGNOS7 > 313 & DIAGNOS7 <315))


Specific_delays_in_development.short <-
  data2.short %>%
  filter((DIAGNOS1 > 314 & DIAGNOS1 <316)| 
           (DIAGNOS2 > 314 & DIAGNOS2 <316)| 
           (DIAGNOS3 > 314 & DIAGNOS3 <316)|
           (DIAGNOS4 > 314 & DIAGNOS4 <316)|
           (DIAGNOS5 > 314 & DIAGNOS5 <316)|
           (DIAGNOS6 > 314 & DIAGNOS6 <316)|
           (DIAGNOS7 > 314 & DIAGNOS7 <316))


Psychic_factors.short <-
  data2.short %>%
  filter((DIAGNOS1 > 315 & DIAGNOS1 <317)| 
           (DIAGNOS2 > 315 & DIAGNOS2 <317)| 
           (DIAGNOS3 > 315 & DIAGNOS3 <317)|
           (DIAGNOS4 > 315 & DIAGNOS4 <317)|
           (DIAGNOS5 > 315 & DIAGNOS5 <317)|
           (DIAGNOS6 > 315 & DIAGNOS6 <317)|
           (DIAGNOS7 > 315 & DIAGNOS7 <317))


Mental_retardation.short <-
  data2.short %>%
  filter((DIAGNOS1 > 316 & DIAGNOS1 <320)| 
           (DIAGNOS2 > 316 & DIAGNOS2 <320)| 
           (DIAGNOS3 > 316 & DIAGNOS3 <320)|
           (DIAGNOS4 > 316 & DIAGNOS4 <320)|
           (DIAGNOS5 > 316 & DIAGNOS5 <320)|
           (DIAGNOS6 > 316 & DIAGNOS6 <320)|
           (DIAGNOS7 > 316 & DIAGNOS7 <320))



ICD_9_code_groups_psychiatric.short <- list(Organic_psychotic_conditions.short,
Senile_and_presenile_organic_psychotic_conditions.short,
Alcoholic_psychoses.short,
Drug_psychoses.short,
Transient_organic_psychotic_conditions.short,
Other_organic_psychotic_conditions_chronic.short,
Other_psychoses.short, 
Schizophrenic_psychoses.short, 
Affective_psychoses.short, 
Paranoid_states.short, 
Other_nonorganic_psychoses.short, 
Psychoses_with_origin_specific_to_childhood.short, 
Neurotic_disorders_personality_disorders_and_nonpsychotic_mental_disorders.short, 
Neurotic_disorders.short, 
Personality_disorders.short, 
Sexual_deviations.short, 
Psychoactive_substance.short, 
Alcohol_dependence_syndrome.short, 
Drug_dependence.short, 
Nondependent_aduse_of_drugs.short, 
Other_primarily_adult_onset.short, 
Physiological_malfunction_arising_from_mental_factors.short, 
Special_symptoms_or_syndromes_not_elsewhere_classified.short, 
Acute_reaction_to_stress.short, 
Adjustment_reaction.short, 
Specific_nonpsychotic_mental_disorders_following_brain_damage.short, 
Depressive_disorder_not_elsewhere_classified.short, 
Mental_disorders_childhood.short, 
Disturbance_of_conduct_not_elsewhere_classified.short, 
Disturbance_of_emotions_specific_to_childhood_and_adolescence.short, 
Hyperkinetic_syndrome_of_childhood.short, 
Specific_delays_in_development.short, 
Psychic_factors.short, 
Mental_retardation.short) 


for(i in 1:length(ICD_9_code_groups_psychiatric.short)){
print(levels(as.factor(ICD_9_code_groups_psychiatric.short[[i]]$SEX)))
}

121633