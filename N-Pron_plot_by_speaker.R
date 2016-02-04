#####################################################
# PLOT N-PRON BY SPEAKER
#
# This script makes a 7 x 4 plot for each speaker, 
# of each of the 7 nouns with each of the 4 pronouns.
# 
#####################################################

# CHOOSE the number of syllables to be plotted:
numsyl <- 3

# CHOOSE the y-axis range
yrange = c(100,220)

# Get the speaker list
speakers <- unique(dat$sp)

# Create NAs to be inserted in plots for Cs (onsets)
x = rep(NA, 10)

#-----------------------------------------------------------
# BEGIN PLOT

for (i in unique(dat$sp)) { # loop through speakers
  
  print(paste("Iteration through speaker list", i, sep=" "))
  
  # Turn on pdf() device
  pdf(paste("N-Pron f0-",i,".pdf",sep=""), width=7, height=5)
  
  s.title <- ""
  
  pc = c(16, 0, 1, 2)
  
  colour = c("black", "red", "green", "blue")
  
  # set up the plot area
  par(mar=c(5, 4, 4, 8) + 0.1) # make space for the legend at the right margin
  
  xmax <- (ncol(dat)-4)*2 # the number of columns containing f0 data
  
  for (j in unique(dat[dat$sp==i,]$noun)) { # loop through nouns for speaker i
    
    # Create a blank plot for the noun; each of the 4 N-Pron combinations will be added below
    plot(1:xmax, type="n", 
         main=paste0(" Noun-Pron ",i), 
         sub=s.title, 
         xlab="Normalized Time", 
         ylab="Frequency (Hz)", 
         ylim=yrange)
    
    print(paste("speaker =", i, "noun =", j, sep=" "))
    
    sp.noun.list <- dat[dat$sp==i & dat$noun==j,]
    
    for (m in 1:nrow(sp.noun.list)) { # loop plotting each pronoun for a noun
      
        print(dat[dat$sp==i & dat$noun==j,]$Normtime)

        # Initialize f0 vector
        f0 = vector(length=0)
        
        # set up an f0 vector with NAs for onsets of each syllable
        for (k in seq(1,ncol(dat)-10,10)) { # for loop through syllables adding NAs for onsets
          f0 <- as.numeric(c(f0,x,(sp.noun.list[m,seq(k+1,k+10,1)])))
        } # for loop through syllables

        lines(1:length(f0), f0, 
              type="b", 
              lwd=1.5,
              lty=1,
              col=colour[m],
              pch=pc[m],
              ylim=yrange, 
              cex.main=.8) 
        
        } # loop plotting each pronoun for a noun
    
    } # loop through nouns
  
    # reset plotting parameters
    dev.off()
  
  } # loop through speakers
