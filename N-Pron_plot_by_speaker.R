#####################################################
# PLOT N-PRON BY SPEAKER
#
# This script makes a 7 x 4 plot of each of the 7 nouns 
# with each of the 4 pronouns, for each speaker.
# 
# Assumptions:
# * The data to be ploted is in the variable dat.
# * The yrange for the plot can be set below.
#####################################################

# CHOOSE the y-axis range
#yrange = c(100,220) #F0 range for males
yrange = c(130,250) #F0 range for FQM

#yrange = c(175,325) #F0 range for females
#yrange = c(150,300) #F0 range for CQM

# Get the speaker list
speakers <- unique(dat$sp)

# Create NAs to be inserted in plots for Cs (onsets)
x = rep(NA, 10)

#-----------------------------------------------------------
# BEGIN PLOT

for (i in unique(dat$sp)) { # loop through speakers
  
  # Trace
  print(paste("Iteration through speaker list", i, sep=" "))
  
  # Turn on Cairo pdf device for special characters anywhere in plot
  library(Cairo)
  Cairo(file=paste0("N-Pron f0-",i,".pdf"),
        type="pdf",
        family="ArialUnicodeMS", 
        width=7, 
        height=5, 
        units="in")
        
  # set up the plot area
  par(mar=c(5, 4, 4, 7) + 0.1) 
        
  
  # Can use this device if you don't need special characters:
  # pdf(paste("N-Pron f0-",i,".pdf",sep=""), width=7, height=5)
  
  # Set up plotting characters for the four N-Pron plots to be plotted in each plot area
  pc = c(0, 1, 2, 15, 16, 17)
  
  # Set up the colours for the four N-Pron plots to be plotted in each plot area
  colour = c("black", "red", "green", "blue", "brown", "orange")
  
  xmax <- (ncol(dat)-7)*2 # the number of columns containing f0 data (minus 7 char columns)
  
  for (j in unique(dat[dat$sp==i,]$g)) { # loop through nouns for speaker i
    
    # Extract the data for the current speaker, i, for the current noun, j
    sp.noun.list <- dat[dat$sp==i & dat$g==j,]
    
    # Create a blank plot for the noun; each of the 4 N-Pron combinations will be added below
    plot(1:xmax, type="n", 
         main=paste0("Noun-Pron: ",i," - ",unique(dat[dat$sp==i & dat$g==j,]$ipa)," '",j,"'"), 
         sub="", 
         xlab="Normalized Time", 
         ylab="Frequency (Hz)", 
         ylim=yrange)
    
    # Trace
    print("###############################################################################")
    print(paste("speaker =", i, "noun =", j, sep=" "))
    
    for (m in 1:nrow(sp.noun.list)) { # loop plotting each pronoun for a noun
      
        # Trace
        print(dat[dat$sp==i & dat$g==j,]$X1)

        # Initialize f0 vector
        f0 = vector(length=0)
        
        # set up an f0 vector with NAs for onsets of each syllable
        for (k in seq(1,ncol(dat)-10,10)) { # for loop through syllables adding NAs for onsets
          f0 <- as.numeric(c(f0,x,(sp.noun.list[m,seq(k+1,k+10,1)])))
        } # for loop through syllables

        # Trace
        print("---------------------------------------------------------------------------")
        #print(sp.noun.list[m,1:31])
        print(sp.noun.list[m,]$X1)
        
        lines(1:length(f0), f0, 
              type="l", # l=lines, p=points, b=both,...
              lwd=2,
              lty=m, # line type, 1=solid, 2=dashed, 3=dotted, 4=dotdash,...6
              col=colour[m],
            # pch=pc[m], # plot character
              ylim=yrange
              ) 
        
        } # loop plotting each pronoun for a noun
    
    # OPTION IF LEGEND IS OUTSIDE THE PLOT (as defined by the par(mar=c(bottom, left, top, right)) settings)
    par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot
    
    # USE WHEN LINE TYPES IN LEGEND ARE SEQUENTIAL beginning with 1
    legend("topright", 
           inset=c(-0.3, 0), 
           legend=sp.noun.list[]$pron, 
           col=colour, 
           lty=1:length(sp.noun.list), 
           bty="n", 
           lwd=2)
    
    } # loop through nouns
  
    # reset plotting parameters
    par(xpd=FALSE)  # Turn clipping back on
    dev.off()
  
  } # loop through speakers
