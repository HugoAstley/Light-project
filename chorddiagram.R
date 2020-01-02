##Could potentialyl do it with the original before numeric removed the V diagnoses etc

load("ICD_9_code_groups_psychiatric.RData")
save(xy, file="xy.RData")

save(out, file="out.RData")
load("out.RData")
####WARNING###
###THIS IS NOT THE WHOLE DATASET!!!####

####Remove lines where they have the same thing over and over because it confuses the function#######
xy[is.na(xy)] <- 0
xy <- xy[!(xy[,1] == "0" & xy[,2] == "0" & xy[,3] == "0" & xy[,4] == "0" & xy[,5] == "0" & xy[,6] == "0" & xy[,7] == "0") ,]

##Better version:
#marked
for(i in 1:nrow(xy)){
  ifelse(
    (length(unique(xy[i, ])) == 1),
    xy[i,1] <- "marked",
    xy
    )
}


#removed
xy <- xy[!(xy[,1] == "marked"),]




################################

######Adjacency like matrix#######




for(i in 1:7){
  xy[,i] <- as.character(xy[,i])
}




# Find all unique diagnoses.
all.diagnoses <- unique(as.vector(xy))
all.diagnoses <- sort(as.character(all.diagnoses))

# This is a way of creating an empty matrix.
out <- matrix(rep(NA, length(all.diagnoses)^2), nrow = length(all.diagnoses),
              dimnames = list(all.diagnoses, all.diagnoses))





  
##The function: make the matrix##
for (i in 1:nrow(xy)) {
 combinations <- combn(unique(xy[i, ]), m = 2, simplify = FALSE)
 tryCatch({
   print(i)
   if (i==7) stop("Error!")
 }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
for (j in 1:length(combinations)) {
    # Add occurrence of each combination to the corresponding combination.
    com <- sort(combinations[[j]])
    out[com[1], com[2]]  <- sum(out[com[1], com[2]], 1, na.rm = TRUE)

  }
}







# Load the circlize library
library(circlize)

# Make the circular plot
chordDiagram(out, transparency = 0.5)


##as character
##failing that : numbers (dataframe then number it but this will be harder turning it into groups)
##couple have like 8 numbers to overcome abvoe problem but then you have to deal with the fact that there are doubles, maybe that's ok















#######################Trash##################
combinations[[1]]

n <- combn(unique(xy[1, ]), m = 2, simplify = FALSE)

length(combinations[[1]])

 if(length(combn(unique(xy[i, ])< 2))) next
while (test_expression)
{
  statement
}

i <- 1
while (i < 6) {
  print(i)
  i = i+1
}

as.character(com[2])
out["0","733"]
com[1]
lapply(xy, function(x)
  if(length(x)>1) {
    combn(x, 2, simplify=FALSE)
  }
  else x)


for(i in 1:length(xy)){
  if(rowsum(xy[i,] == 0){
    xy <- xy[-i,]
  }
}







#in each diagnoses group: what's the probabiility of the other diagnoses groups being there
#They could be columns: search through the diagnoses coloumns then paste into that one column
#temporary true fulse
#count true or false by row like you did with the diagnoses number and divide by the length



#making the boolean matrix   
library(reshape2)
n <- melt(data2)
w <- dcast(n, Items~TrxID)
x <- as.matrix(w[,-1])
x[is.na(x)] <- 0
x <- apply(x, 2,  function(x) as.numeric(x > 0))  #recode as 0/1
v <- x %*% t(x)                                   #the magic matrix 
diag(v) <- 0                                      #repalce diagonal
dimnames(v) <- list(w[, 1], w[,1])                #name the dimensions
v
#


n <- rowSums(data2[,4:10] == "nc")


for(i in 1:7){
  data2.binary[,i+3] <- data2[,i+3] !="NA"
}

#Find rowwise count values of the number of true and false values
data2$DIAGNOSn <- rowSums(data2.binary[,4:10], na.rm = T)



###Creating the boolean matrix
#Give them all a label 1
#Then left join into data2

#Conditional replacement e.g. "psychiatric" then rowsum based on that



###A table that, for each diagnoses, shows the breakdown of that diagnoses across the other diagnoses (as in how many are in common with this...and this...and this)
n <- ddply(data2.short, .(data2.short$DIAGNOS1, data2.short$DIAGNOS2, data2.short$DIAGNOS3), nrow)

n








##Make a circualtarised plot
library(circlize)



#Exclude NA from the ciruclar plot
out <- out[2:18, 2:18]

#Remove tiny ones: congenital, infectious,  pregancy, perinatal, #3,8,13,14
out <- out[-c(3,8,13,14), -c(3,8,13,14)]


# Make the circular plot


circos.par(gap.after = 5, start.degree = -34)



####Overall 
out[is.na(out)] <- 0
col_fun = colorRamp2(range(out), c("white", "black"), transparency = 0.5)
chordDiagram(out,
             grid.col = "lightgrey",
             transparency = 0.5,
             col= col_fun,
             order = c("respiratory", "skin","psychiatric",  "blood", "circulatory", "digestive", 
                       "endocrine", "genitourinary", 
                       "illdefined", "injury", "musculoskeletal", "neoplasms",
                       "nervous"))



###Emphasis on psychaitric

##Colour table##
col_df = data.frame(c("psychiatric", "psychiatric","psychiatric", "blood", "circulatory", "digestive", 
                      "endocrine", "genitourinary", 
                      "illdefined", "injury", "musculoskeletal", "neoplasms",
                      "nervous",
                      "skin","respiratory"),
  c("respiratory", "skin", c(rep("psychiatric", times=(nrow(out))))),
                    c(rep("lightblue", times=(2+nrow(out)))))


##Draw it##
chordDiagram(out,
             row.col = c("#FF000080", "#00FF0010", "#0000FF10"),
             transparency = 0.5,
             grid.col = "lightgrey",
             col= col_df,
             order = c("respiratory", "skin","psychiatric",  "blood", "circulatory", "digestive", 
                       "endocrine", "genitourinary", 
                       "illdefined", "injury", "musculoskeletal", "neoplasms",
                       "nervous")
)





            
            

            
##Not used##
#take away weak lines
col_mat[out < 12] = "#00000000"
chordDiagram(mat, grid.col = grid.col, col = col_mat, link.border = border_mat2) 

"#FFEEEE", "#FF0000"

##Link border
link.border = border_mat2

border_mat2 = matrix("black", nrow = 1, ncol = ncol(out))
rownames(border_mat2) = rownames(out)[15]
colnames(border_mat2) = colnames(out)


chordDiagram(mat, grid.col = grid.col, link.lwd = 2, )


"congenital", 
"Infectious",
"perinatal",
"pregnancy"




chordDiagram(mat, grid.col = grid.col, )

            
                      
              grid.col = c(S1 = "red", S2 = "green", S3 = "blue",
                                   E1 = "grey", E2 = "grey", E3 = "grey", E4 = "grey", E5 = "grey", E6 = "grey")        
            
####Trash###






##get an adjacency matrix
library(igraph)


n <- get.adjacency(graph.data.frame(as.matrix(data2.short[,5:11]), directed=FALSE))

graph.data.frame(as.matrix(data2.short[,5:11]), directed=FALSE)

n <- as.data.frame(n)