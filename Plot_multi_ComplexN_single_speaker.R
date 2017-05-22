#####################################################
# PLOT MULTIPLE COMPLEX N ITEMS FROM SINGLE SPEAKER ON ONE PLOT
#
# This plots chosen items from one speaker from the Complex N tone production experiment
# (May 28-29, 2015) on a single plot for comparison.
# 
# Assumptions:
# * The dataset has been imported to R using the script ```get_ensemble_files.R``` and is stored
#   in the list ```dataset.list```
# * Before plotting, the data to be plotted is stored in the dataframe ```dat``` as described in
#   "Complex N data processing.Rmd"
#
# Other plotting scripts are offered (see "Complex N data processing.Rmd"). This script can
# * MULTIPLE WORDLIST ITEMS FROM A SINGLE SPEAKER TO THE SCREEN
# * MULTIPLE WORDLIST ITEMS FROM A SINGLE SPEAKER TO PDF
# 
# TO MAKE PLOTS WITH THIS SCRIPT:
# 1. Excecute the code in the first section, "SET THESE PARAMETERS BEFORE ATTEMPTING ANY OF THE PLOTS!"
# 2. Select code in the PLOT section and hit ⌘ ENTER
#
# The same plot setup is used for both CVCV and CVV/CVˀV structures.
#
#####################################################

#----------------------------------------------------
# SET THESE PARAMETERS BEFORE ATTEMPTING ANY OF THE PLOTS!
#----------------------------------------------------

# Load the data as described in "Complex N data processing.Rmd"

# Create NAs to be inserted in plots for consonants (onsets)
na.s = rep(NA, 10)

# CHOOSE the Y-AXIS range
yrange = c(100,220)

# CHOOSE output to "screen" or "pdf"
#output <- "screen"
output <- "pdf"

# CHOOSE the SPEAKER
# View the list speakers in the data if you want to
speakers <- unique(dat$sp)

# CHOOSE the speaker:
speaker <- "MQM"

# CHOOSE the wordlist items to be plotted together on a single plot
# 1. Execute any of the next lines to view wordlist items 
dat[order(dat$cv, dat$mel, decreasing = T),][c(47,45,46,43)] # sorted by CV structure and melody
dat[order(dat$mel, decreasing = T),][c(47,45,46,43)] # sorted only by melody
#for (i in 1:nrow(dat)) { cat(i,": ",dat[i,]$noun," ",dat[i,]$mel,"\t",sep=""); if (i %in% seq(2,nrow(dat),2)) { cat("\n", sep="") } }

# 2. Specify the item numbers to be plotted separated by commas:
#items <- c(37,32,31) # CVV plots L_L, H_H
#items <- c(19,20,21) # CVV plots Isolation
#items <- c(37,32,31,36,38,12) # more CVV plots L_L, H_H
#items <- c(19,20,21,22,26,28) # CVV plots Isolation
#items <- c(6,11,18) # CVCV plots L_L, H_H
#items <- c(1,38,3) # CVCV plots Isolation
#items <- c(6,11,18,7,29,9) # more CVCV plots L_L, H_H
items <- c(1,38,3,4,7,10) # CVCV plots Isolation
itemsID <- "CVCV" # Give an identifier for the group of items for filename & plot title
#itemsID <- "CVV" # Give an identifier for the group of items for filename & plot title

# 3. Save specified items to sp.dat
sp.dat <- dat[items,] 
sp.dat$X1 # Check to see which items are selected

# SPECIFY the experiment context: "isolation", "L_L", "H_H" to be used in filname & plot title
#context <- "isolation"
#context <- "H_H"
#context <- "L_L"
context <- "isolation"

# OPTION: CHOOSE PLOTTING CHARACTERS for for each plot (default is to use line styles so 
#   uncomment the line, pch=pc..., in the plotting section below to use plotting charcters
#pc = c(16, 0, 1, 2)

#xmax <- (ncol(dat)-7)*2 # the number of columns containing f0 data (minus 7 char columns)
xmax <- (ncol(dat)-17)*2 # Use this code if all items are only 3 moras

#-----------------------------------------------------------
# PLOT
#-----------------------------------------------------------

#PLOT SETUP

if (output == "pdf") {
  # Turn on Cairo pdf device for special characters anywhere in plot
  library(Cairo)
  Cairo(file=paste0("ComplexN-",speaker,"-",context,"(",itemsID,")-",paste0(sp.dat$mel, collapse=","),".pdf"),
        type="pdf",
        family="ArialUnicodeMS", 
        width=7, 
        height=5, 
        units="in")
} # if turn on pdf device if pdf output was chosen

# set up the plot area
par(mar=c(5, 4, 4, 2) + 0.1) # make space for the legend at the right margin

# Set up the colours for up to FOUR plots to be plotted in each plot area
colour = c("black", "red", "green", "blue", "brown", "orange")

# Make a blank plot
plot(1:xmax, 
     ylim=yrange, 
     type="n", 
     lwd=2, 
     ylab="F0 (Herz)",
     xlab="Normalized Time", 
     main=paste0(speaker," ",context," (",itemsID,"): ", paste0(sp.dat$mel, collapse=", ")),
     sub="")

# MAKE PLOTS

# for loop through wordlist items for the speaker
for (j in 1:nrow(sp.dat)) {
  
  # Extract item[j] data to be plotted
  dat1 <- sp.dat[j,]
  
  # Set up an f0 vector with NAs for onsets of each syllable and f0 data for the TBU
  #   taking into account that some items have NAs filling the last 10 columns because
  #   they were shorter than the longest items in the experiment
  f0 <- vector(length=0) # Initialize f0 vector
  
  # Get the # of none NA columns (not including non-numeric columns)
  n <- sum(!is.na(dat1[,sapply(dat1,is.numeric)]))
  
  # for loop through all syllables but the final syllable adding NAs for onsets 
  for (i in seq(1,n-10,10)) { 
    f0 <- as.numeric(c(f0,na.s,dat1[,seq(i+1,i+10,1)]))
  } # for loop through syllables
  
  if (dat1$cv=="CVCV") {
    f0 <- as.numeric(c(f0,na.s,dat1[,seq(i+11,i+20,1)]))
  } else {
    # Add f0 data for the final syllable (i.e. the 2nd half of the final CVV)
    f0 <- as.numeric(c(f0,dat1[,seq(i+11,i+20,1)]))
  } # else
  
  
# f0 <- create.f0(d=sp.dat[i,], index=i)
  lines(1:length(f0), f0, 
       ylim=yrange, 
       type="l", 
       lwd=2, 
       lty=j, # line type, 1=solid, 2=dashed, 3=dotted, 4=dotdash,...6
       col=colour[j],
     # pch=pc[m], # plot character
       ylab="F0 (Herz)",
       xlab="Normalized Time")

  print(paste0(speaker,", item ",j,": ", sp.dat[j,]$noun))
  
  } # for loop through wordlist

# PRINT THE LEGEND

# USE WHEN LINE TYPES IN LEGEND ARE SEQUENTIAL beginning with 1
legend("topleft", 
       legend=paste0(sp.dat$ipa," '",sp.dat$noun,"' ",sp.dat$mel), 
       col=colour, 
       lty=1:nrow(sp.dat), 
       bty="n",
       cex=0.8,
       lwd=2)

if (output == "pdf") {
  # reset plotting parameters
  dev.off()
} # shut off pdf device if pdf output was chosen


