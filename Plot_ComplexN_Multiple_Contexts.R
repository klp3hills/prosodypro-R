#####################################################
# PLOT COMPLEX N MULTIPLE CONTEXTS
#
# This plots all contexts (isolation, L_L, H_H) of each item of the Complex N tone production
# on a single plot for a single speaker.
# 
# PREPARING THE DATA TO MAKE PLOTS:
# * The data for the SINGLE speaker to be plotted is stored in the following dataframes: 
#       * Isolation context: datI
#         datI <- dat
#       * L_L context: datL
#         datL <- dat
#       * H_H context: datH
#         datH <- dat
#   This can be done by 
#       1. clearing the workspace 
#       2. loading a saved workspace for a particular context for a speaker
#       3. changing ```dat``` to the appropriate name from the above list
#       4. clearing all items from the workspace EXCEPT the new variable(s) you have created
#          (In the Environment pane in RStudio, change to Grid view, check all boxes except the
#          one you want to keep, click the broom icon)
#       5. loading the workspace for the next context
#       6. repeat
#
# * THE DATAFRAMES ARE SORTED THE SAME! Each item in each of the dataframes must correspond!
#   If each dataframe has all the data, they can be sorted by the Mixtec for each item like this:
#       x <- datI[order(datI$mix),]
#       y <- datL[order(datL$mix),]
#       z <- datH[order(datH$mix),]
#       cbind(x$mix, y$mix, z$mix) # check to see items line up
#   ***If dataframes are missing items then empty rows for those items will have to be inserted.
#
# * THE NUMBER OF SEGMENTS has been added to each dataframe.
#   This can be done using this vector I created by hand with the # of segs for each item:
#     segs <- c(8,5,5,7,5,5,5,7,8,6,5,6,6,5,5,5,6,8,5,6,6,5,5,6,6,5,6,5,7,8,5,6,6,5,7,8,5,5)
#     x$segs <- segs # add the segs column to the dataframe
#     cbind(x$mix, x$segs) # check the correspondence between the Mixtec and # of segs
#     y$segs <- segs
#     z$segs <- segs
#
# * ALL CONTEXT DATAFRAMES ARE IN A LIST
#   Put them in a list like this:
#       all.dat <- list(x,y,z) # put the data.frames into a list so we can loop through them
#       cbind(all.dat[[1]]$mix, all.dat[[1]]$segs, all.dat[[2]]$mix, all.dat[[2]]$segs, all.dat[[3]]$mix, all.dat[[3]]$segs) # check
#
# AFTER TO MAKE PLOTS WITH THIS SCRIPT CLIKE THE SOURCE BUTTON in RSTUDIO 
# (upper right of this pane)
#
# The same plot setup is used for both CVCV and CVV/CVˀV structures.
#
#####################################################


#----------------------------------------------------
# DO THIS BEFORE ATTEMPTING ANY OF THE PLOTS!
#----------------------------------------------------

# Load the data as described in "Complex N data processing.Rmd"

# Create NAs to be inserted in plots for consonants (onsets)
na.s = rep(NA, 10)

#----------------------------------------------------

#####################################################
# **ALL WORDLIST ITEMS** FROM A SINGLE SPEAKER TO PDF
#####################################################

# This plots each wordlist item for a single speaker to a separate page in a PDF file. It uses 
# the Cairo library so it can plot UTF-8 characters (e.g. in vernacular labels).

#-----------------------------------------------------------
# GENERAL SETUP
#-----------------------------------------------------------

# CHOOSE the speaker:
speaker <- "MQM"
       
# CHOOSE the Y-AXIS range
yrange = c(100,220)

# SPECIFY the experiment context: "isolation", "L_L", "H_H" (This is only for labeling)
#context <- "isolation"
#context <- "H_H"
#context <- "L_L"
#context <- "All"
context <- c("isolation", "L_L", "H_H")


#-----------------------------------------------------------
# PLOT SETUP
#-----------------------------------------------------------

# Turn on Cairo pdf device if there are special characters anywhere in plot
library(Cairo)
Cairo(file=paste0("ComplexN-",speaker,"-",paste(context, collapse = ","),".pdf"),
      type="pdf",
      family="ArialUnicodeMS", 
      width=7, 
      height=5, 
      units="in")

# set up the plot area
#par(mar=c(5, 4, 4, 2) + 0.1) # Use when legend inside plotting area (or no legend)
par(mar=c(5, 4, 4, 7) + 0.1) # Use when legend outside the plotting area

# Set up the colours for the contexts to be plotted in each plot area
colour = c("black", "red", "green", "blue", "brown", "orange")

#-----------------------------------------------------------
# MAKE PLOTS
#-----------------------------------------------------------

# loop through wordlist items for the speaker
for (j in 1:nrow(x)) { # each data.frame has the same # of rows, so I'll just use x
  
  # create a blank plot with labels
  plot(1:(x[j,]$segs*10), 
       ylim=yrange, 
       type="n", 
       lwd=2, 
       ylab="F0 (Herz)",
       xlab="Normalized Time", 
       main=paste0(speaker,": ",x[j,]$ipa," '",x[j,]$noun,"'"," ",x[j,]$mel),
       sub="")
  
  for (k in 1:length(all.dat)) { # loop through each context data.frame
    
    # Extract the data item
    dat <- all.dat[[k]][j,]
  
    # Set up an f0 vector with NAs for onsets of each syllable and f0 data for the TBU
    #   taking into account that some items have NAs filling the last 10 columns because
    #   they were shorter than the longest items in the experiment
    f0 <- vector(length=0) # Initialize f0 vector
  
    # Get the # of numeric columns in current row with numeric data (i.e. those that aren't "NA" 
    # not including non-numeric columns) MINUS 1 because dat$segs is numeric, but not f0 data
    n <- sum(!is.na(dat[,sapply(dat,is.numeric)]))-1
  
    # loop through all syllables but the final syllable adding NAs for onsets 
    for (i in seq(1,n-10,10)) { 
      f0 <- as.numeric(c(f0,na.s,dat[,seq(i+1,i+10,1)]))
    } # for loop through syllables
  
    if (dat$cv=="CVCV") {
      f0 <- as.numeric(c(f0,na.s,dat[,seq(i+11,i+20,1)]))
    } else { # Add f0 data for the final syllable (i.e. the 2nd half of the final CVV)
      f0 <- as.numeric(c(f0,dat[,seq(i+11,i+20,1)]))
    } # else
  
  
    # add context to plot
    lines(1:length(f0), f0, 
         #ylim=yrange, 
         type="l", 
         lwd=2, 
         lty=k, # line type, 1=solid, 2=dashed, 3=dotted, 4=dotdash,...6
         col=colour[k]
         )
  
    print(paste0(speaker,", item ",j,": ", dat$X1,", context: ",k)) # trace progress
  } # loop through contexts
  
  # OPTION IF LEGEND IS OUTSIDE THE PLOT (as defined by the par(mar=c(bottom, left, top, right)) settings)
  par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot
  
  # USE WHEN LINE TYPES IN LEGEND ARE SEQUENTIAL beginning with 1
  legend("topright", 
         inset=c(-0.3, 0), 
         legend=context, 
         col=colour, 
         lty=1:length(context), 
         bty="n", 
         lwd=2)
  
} # for loop through wordlist

# reset plotting parameters
dev.off()