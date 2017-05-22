#********************************************************************************
# Plot selected N-Pron frames
#
# This plot allows you to plot selected N-Pron items from a single speaker to a single plot
# Assumptions:
# * The speakers N-Pron data is stored in the variable `dat`.
# Setup:
# * Specify which N-Pron combinations to plot
# * Specify the title for the file/plot
# * In PLOT SETUP:
#   - Set the y-range for the plots
#   - Can change the colours for the plot lines 
#
#********************************************************************************

#----------------------------------------
# SETUP

# Select the items to be plotted by creating a vector of names of the nouns
#   and a corresponding vector of names of the pronouns
#   and putting them together into a data frame

#noun <- c('camisa', 'puño', 'puño', 'camisa', 'puño') # Comparing LM & L
#pro <- c('el', 'el', 'mi', 'nosotros', 'nosotros') # Comparing LM & L
#noun <- c('camisa', 'puño') # Comparing LM & L
#pro <- c('nosotros', 'nosotros') # Comparing LM & L
#noun <- c('camisa', 'camisa', 'puerco', 'puerco') # Comparing LM & LH
#pro <- c('el', 'nosotros', 'el', 'nosotros') # Comparing LM & LH
#noun <- c('semilla', 'rodilla') # Comparing HHa-1pe ~ HHb-3m
#pro <- c('NA', 'nosotros', 'el') # Comparing HHa-1pe ~ HHb-3m
noun <- c('semilla', 'cama') # Comparing HHa-1pe ~ HHb-3m
pro <- c('mi', 'mi') # Comparing HHa-1pe ~ HHb-3m

selected <- data.frame(noun, pro)

# change the structue from factor to character
selected$noun <- as.character(selected$noun)
selected$pro <- as.character(selected$pro)

# Specify the name for the file/plot
#fname <- "Compare L & LM"
#fname <- "Compare LM & LH"
#fname <- "Compare HH & HH(L)"
fname <- "Compare HH-1sg HL-1sg"

#----------------------------------------
# PLOT SETUP
# CHOOSE the y-axis range
#yrange = c(100,220) #F0 range for males
#yrange = c(130,250) #F0 range for FQM
#yrange = c(175,325) #F0 range for females
#yrange = c(150,300) #F0 range for CQM
yrange = c(150,250) #F0 range for All Speakers

# Set up the colours for the four N-Pron plots to be plotted in each plot area
colour = c("black", "red", "green", "blue", "brown", "orange")

# Create NAs to be inserted in plots for Cs (onsets)
x = rep(NA, 10)

# Turn on Cairo pdf device for special characters anywhere in plot
library(Cairo)
Cairo(file=paste0("N-Pron - ",unique(dat$sp)," - ",fname,".pdf"),
      type="pdf",
      family="ArialUnicodeMS", 
      width=7, 
      height=5, 
      units="in")

# set up the plot area
par(mar=c(5, 4, 4, 7) + 0.1) 

xmax <- (ncol(dat)-7)*2 # the number of columns containing f0 data (minus 7 char columns)

# Create a blank plot for the noun; each of the N-Pron combinations will be added below
plot(1:xmax, type="n", 
     main=paste0("Noun-Pron: ",unique(dat$sp)," - ", fname), 
     sub="", 
     xlab="Normalized Time", 
     ylab="Frequency (Hz)", 
     ylim=yrange)

#----------------------------------------
# BEGIN PLOT

for (i in 1:nrow(selected)) { # loop through selected N-Pron items
  
  item <- dat[dat$g==selected$noun[i] & dat$pron==selected$pro[i],]
    
  # Initialize f0 vector (which will have data + NAs for onsets)
  f0 = vector(length=0)
  
  # set up an f0 vector with NAs for onsets of each syllable
  for (j in seq(1,ncol(dat)-10,10)) { # for loop through syllables adding NAs for onsets
    f0 <- as.numeric(c(f0,x,(item[1,seq(j+1,j+10,1)])))
  } # for loop through syllables
  
  lines(1:length(f0), f0, 
        type="l", # l=lines, p=points, b=both,...
        lwd=2,
        lty=i, # line type, 1=solid, 2=dashed, 3=dotted, 4=dotdash,...6
        col=colour[i],
        ylim=yrange
        )

} # lopp through seleted N-Pron items

# OPTION IF LEGEND IS OUTSIDE THE PLOT (as defined by the par(mar=c(bottom, left, top, right)) settings)
par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot

# USE WHEN LINE TYPES IN LEGEND ARE SEQUENTIAL beginning with 1
legend("topright", 
       inset=c(-0.3, 0), 
       legend=paste(selected[]$noun, selected[]$pro, sep="-"), 
       col=colour, 
       lty=1:nrow(selected), 
       bty="n", 
       lwd=2)

# reset plotting parameters
par(xpd=FALSE)  # Turn clipping back on
dev.off()
  