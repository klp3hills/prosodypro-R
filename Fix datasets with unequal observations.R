######################################################################################
# Fix datasets with unequal data points in rows
# 
# This script reads in a tab delimited dataset which has an unequal number of 
# data points in each row (even when the first row does not contain the max number
# of columns) and outputs a dataframe (df). It reads the data in as characters, so 
# if some of the data are numbers, precision will be lost in converting from characters 
# back to numeric (it truncates the data at 4 decimal points). NAs are filed in at the END 
# of rows with less than the maximum number of columns.
#
######################################################################################
x <- scan(file=file.choose(), what="char", sep="\n", skip=1)

num.col <- max(count.fields(file=file.choose(), sep = "\t"))
x <- read.csv(file=file.choose(), col.names=1:num.col, sep="\t", fill=T, skip=1, encoding="UTF8")

# initialize variables to store length and item # of longest item
longest.length <- 0
longest.item <- 0
y <- list(length=length(x))

for (i in 1:length(x)) {
  
  y[[i]] <- unlist(strsplit(x[i], "\t")) # Separate observations in x[i] to y[i]
  
  if (length(y[[i]]) > longest.length) {
    longest.length <- length(y[[i]])
    longest.item <- i
  }
}

for (j in 1:length(y)) {
  
  # Get the difference between length of current item & length of longest item
  d <- longest.length - length(y[[j]]) 
  cat("y[",j,"]: ",longest.length,"- ",length(y[[j]])," = ",d,"\n")
  
  if (d > 0) { # if length of current item less than longest, add NAs
    y[[j]] <- c(y[[j]],rep("NA",d))
  }
}

# Convert the list to a dataframe (skipping the first list item which is the column names)
df <- data.frame(matrix(unlist(y), nrow=length(y), ncol=longest.length, byrow=T),stringsAsFactors=FALSE)

# Make the column names--"item", "X1", "X2",...
c.names <- c("item",paste0("X",2:longest.length))
colnames(df) <- c.names

