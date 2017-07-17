#********************************************************************************
# Plot words selected by glosses
#
# This plot allows you to average & plot selected items from all speakers to a single plot
#
# Assumptions:
# * The ProsodyPro data for each item for each speaker is stored in the variable `dat`.
#
# Setup:
# * Specify which items to plot
# * Specify the title for the file/plot
# * In PLOT SETUP:
#   - Set the y-range for the plots
#   - Can change the colours for the plot lines 
#
#********************************************************************************

#----------------------------------------
# SETUP

# SPECIFY the items to be plotted by creating a vector of names of the item glosses
# Comparing HH(L)-1pe & HH-3m
gloss.list <- c('cochino', 'camisa', 'ardilla') 

# SPECIFY the title for the file/plot
title <- "LH, L!H, (L)HL"

# INDICATE which columns contain the f0 data
data.cols <- 2:21

# CHOOSE output to "screen" or "pdf"
output <- "screen"
#output <- "pdf"

#----------------------------------------
# PLOT SETUP
# CHOOSE the y-axis range
yrange = c(100,220) #F0 range for All Speakers

# Set up the colours for the four N-Pron plots to be plotted in each plot area
colour = c("black", "red", "green", "blue", "brown", "orange")

# Create NAs to be inserted in plots for Cs (onsets)
x = rep(NA, 10)

if (output == "pdf") {
  # Turn on Cairo pdf device for special characters anywhere in plot
  library(Cairo)
  Cairo(file=paste0(title,"-",s.title,".pdf"),
        type="pdf",
        family="ArialUnicodeMS", 
        width=7, 
        height=5, 
        units="in")
} # if turn on pdf device if pdf output was chosen

# set up the plot area
#par(mar=c(5, 4, 4, 7) + 0.1) # use to make space for the legend at the right margin
par(mar=c(5, 4, 4, 2) + 0.1) # use to put legend inside plotting area

xmax <- (ncol(dat)-4)*2 # the number of columns containing f0 data (minus 4 non-f0 data columns)

# Extract the data to be plotted and average it
#   calculcate the aggregate mean of the data with the glosses in `gloss.list` 
#   grouped on each gloss in `$g` (column 24 = the gloss variable)
selected <- aggregate(.~g, data=dat[dat$g %in% gloss.list,c(data.cols,24)], mean, na.rm=T)
  
# Create a blank plot for the noun; each of the N-Pron combinations will be added below
plot(1:xmax, type="n", 
     main=paste0(title), 
     sub="", 
     xlab="Normalized Time", 
     ylab="Frequency (Hz)", 
     ylim=yrange)

#----------------------------------------
# BEGIN PLOT

for (i in 1:length(gloss.list)) { # loop through selected items
  
  item <- selected[i,]
  
  # Initialize f0 vector (which will have data + NAs for onsets)
  f0 = vector(length=0)
  
  # set up an f0 vector with NAs for onsets of each syllable
  for (j in seq(1,ncol(item)-10,10)) { # for loop through syllables adding NAs for onsets
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

# PRINT THE LEGEND

# First, get new melody list and wordlist with phonological representation and Eng. glosses, 
# Second, replace old melody labels with new ones & add variables for phonological rep & Eng gloss

# Read in list of new melody labels
new.melody.list <- read.csv(file="/Users/Kevin/Fieldwork-vmj/Tone - experiments/new_melody_list.csv", header=T, sep="\t", encoding="UTF-8")

# Read in a new wordlist to use as a lookup table
new.wordlist <- read.csv(file = "/Users/Kevin/Fieldwork-vmj/Tone - experiments/2011 single words (108)/2011 wordlist (updated phonetic).csv", header = T, sep = "\t", encoding = "UTF8")

m <- gloss.list # Create vector for the melody
ph <- gloss.list # Create vector for phonological representation
en <- gloss.list # Create vector for English gloss

# Using lapply, loop over items in new$ph and match values to the look up table. store in new$ph.
#   lapply creates a list of factors so I had to unlist it and turn it into characters

# Replace the Spanish glosses with melodies
m <- as.character(unlist(lapply(m, function(x) new.wordlist$Melody[match(x, new.wordlist$Spanish)])))

# Replace the melody labels in `melody.list` with new ones from `new.melody.list`
m <- as.character(unlist(lapply(m, function(x) new.melody.list$new_m[match(x, new.melody.list$old_m)])))

# Replace Spanish glosses in `ph` with phonological representation from `new.wordlist`
ph <- as.character(unlist(lapply(ph, function(x) new.wordlist$Phonetic[match(x, new.wordlist$Spanish)])))

# Replace Spanish glosses in `en` with English glosses from `new.wordlist`
en <- as.character(unlist(lapply(en, function(x) new.wordlist$English[match(x, new.wordlist$Spanish)])))


# OPTION IF LEGEND IS OUTSIDE THE PLOT (as defined by the par(mar=c(bottom, left, top, right)) settings)
#par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot

# USE WHEN LINE TYPES IN LEGEND ARE SEQUENTIAL beginning with 1
legend("bottomleft", 
       #inset=c(-0.3, 0), # use if legend outside the plot
       legend=paste0(m,": ",ph," ‘",en,"’"), 
       col=colour, 
       lty=1:length(gloss.list), 
       bty="n", 
       lwd=2)

if (output == "pdf") {
  # reset plotting parameters
  dev.off()
} # shut off pdf device if pdf output was chosen
