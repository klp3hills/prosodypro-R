create.f0 <- function(d, index) {

#-----------------------------------------------------------
# SETUP FUNCTION (all root structures)
#-----------------------------------------------------------
  
  # Extract the data
  dat1 <- d[index,]
  
  # Set up an f0 vector with NAs for onsets of each syllable and f0 data for the TBU
  #   taking into account that some items have NAs filling the last 10 columns because
  #   they were shorter than the longest items in the experiment
  x <- vector(length=0) # Initialize f0 vector
  
  # Get the # of none NA columns (not including non-numeric columns)
  n <- sum(!is.na(dat1[,sapply(dat1,is.numeric)]))
  
  # for loop through all syllables but the final syllable adding NAs for onsets 
  for (i in seq(1,n-10,10)) { 
    x <- as.numeric(c(x,na.s,dat1[,seq(i+1,i+10,1)]))
  } # for loop through syllables
  
  if (dat1$cv=="CVCV") {
    x <- as.numeric(c(x,na.s,dat1[,seq(i+11,i+20,1)]))
  } else {
    # Add f0 data for the final syllable (i.e. the 2nd half of the final CVV)
    x <- as.numeric(c(x,dat1[,seq(i+11,i+20,1)]))
  } # else
  
  return(x)
  
} # function create.f0