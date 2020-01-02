######Make them procedure groups######

save(xy, file="xy.RData")


xy <- as.matrix(data2.short[,5:11])

for(i in 1:7){
  xy[,i] <- as.character(xy[,i])
}


xy[is.na(xy)] <- 0



for(i in 1:7){
  xy[,i] <- as.numeric(xy[,i])
}  


for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 000 & xy[j,i] < 140),
      xy[j,i] <- "Infectious",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}


for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 139 & xy[j,i] < 240),
      xy[j,i] <- "neoplasms",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 239 & xy[j,i] < 280),
      xy[j,i] <- "endocrine",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 279 & xy[j,i] < 290),
      xy[j,i] <- "blood",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 289 & xy[j,i] < 320),
      xy[j,i] <- "psychiatric",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}


for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 319 & xy[j,i] < 390),
      xy[j,i] <- "nervous",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 389 & xy[j,i] < 460),
      xy[j,i] <- "circulatory",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 459 & xy[j,i] < 520),
      xy[j,i] <- "respiratory",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 519 & xy[j,i] < 580),
      xy[j,i] <- "digestive",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 579 & xy[j,i] < 630),
      xy[j,i] <- "genitourinary",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 629 & xy[j,i] < 680),
      xy[j,i] <- "pregnancy",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 679 & xy[j,i] < 710),
      xy[j,i] <- "skin",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 709 & xy[j,i] < 740),
      xy[j,i] <- "musculoskeletal",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 739 & xy[j,i] < 760),
      xy[j,i] <- "congenital",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 759 & xy[j,i] < 780),
      xy[j,i] <- "perinatal",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 779 & xy[j,i] < 800),
      xy[j,i] <- "illdefined",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
for(i in 1:7){
  for(j in 1:nrow(xy)){
    ifelse(
      (xy[j,i] > 799 & xy[j,i] <= 999),
      xy[j,i] <- "injury",
      xy[j,i]
    )
    tryCatch({
      print(j)
      if (j==7) stop("Error!")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

