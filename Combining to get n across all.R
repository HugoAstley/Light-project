view(primary.diagnosis.ICD_9_code_groups[[1]])


test <- do.call("rbind", primary.diagnosis.ICD_9_code_groups)

4327242
3199147

duplicated(primary.diagnosis.ICD_9_code_groups)

difference <- (primary.diagnosis.ICD_9_code_groups[duplicated(primary.diagnosis.ICD_9_code_groups)][[1]])
