#********************************************************************************
# Plot selected N-Pron frames
#
# This plot allows you to plot selected N-Pron items from a single speaker to a single plot
#
# Assumptions:
# * The averaged N-Pron data for each item for each speaker is stored in the variable `dat`.
#
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

# SPECIFY if the plots are from data of all speaker averages or individual speaker's data
#   (The "all speaker" data stores the filenames used in picking items in the `Normtime`
#    variable, but the individual data stores them in `X1`)
is.all.sp = "F"

# SPECIFY the items to be plotted by creating a vector of GLOSSES
#   use underline "_" for spaces; e.g. tlacuache_de_mi # by gloss
gloss.list <- c("_puerco_de_nosotros_","_puerco_de_el_") # by filename
#gloss.list <- dat[dat$pron=="NA",]$g # use for bare nouns (no pron)

# Nothing to do here. Since the aggregate (averaged) data & individual data store the filename in
#   a different variable, we need this statement to choose the right one:
if (is.all.sp == "T") { # all speaker averaged data
  selected <- dat[dat$Normtime %in% gloss.list,] 
} else { # individual speaker's data
  selected <- dat[dat$X1 %in% gloss.list,] 
} # else

# Other ways to select the data... 
#selected <- dat[grep(paste(gloss.list, collapse="|"), dat$Normtime),] # Use to find gloss in filename
#selected <- dat[dat$pro=="NA",] # use for bare nouns (no pron)

# SPECIFY the name for the file/plot
title <- "LH-1pe, LH-3m (L_L Context)"

# SPECIFY "T" if ALL items are BIMORAIC (i.e. only two TBUs) or NOT
bimoraic = "F"

# CHOOSE output to "screen" or "pdf"
output <- "screen"
#output <- "pdf"

#----------------------------------------
# PLOT SETUP
# CHOOSE the y-axis range
#yrange = c(100,220) #F0 range for males
yrange = c(130,240) #F0 range for FQM
#yrange = c(180,300) #F0 range for AHM
#yrange = c(160,260) #F0 range for CQM
#yrange = c(140,260) #F0 range for AHM

# Set up the colours for the four N-Pron plots to be plotted in each plot area
colour = c("black", "red", "green", "blue", "brown", "orange", "green3")

# Create NAs to be inserted in plots for Cs (onsets)
x = rep(NA, 10)

if (output == "pdf") {
  # Turn on Cairo pdf device for special characters anywhere in plot
  library(Cairo)
  Cairo(file=paste0("N-Pron - ",unique(dat$sp)," - ",title,".pdf"),
        type="pdf",
        family="ArialUnicodeMS", 
        width=7, 
        height=5, 
        units="in")
} # if turn on pdf device if pdf output was chosen

# set up the plot area
#par(mar=c(5, 4, 4, 7) + 0.1) # use for legend outside plot
par(mar=c(5, 4, 4, 2) + 0.1) # use for legend inside plot

if (bimoraic=="F") { # if data is not all bimoraic
  xmax <- (ncol(dat)-7)*2 # the number of columns containing f0 data (minus 7 char columns)
} else { # else if data is all bimoraic
  xmax <- 40
}

# Create a blank plot for the noun; each of the N-Pron combinations will be added below
plot(1:xmax, 
     type="n", 
     main=paste0(unique(dat$sp)," - ", title), 
     sub="", 
     xlab="Normalized Time", 
     ylab="Frequency (Hz)", 
     ylim=yrange)

#----------------------------------------
# BEGIN PLOT

for (i in 1:nrow(selected)) { # loop through selected N-Pron items
  
  item <- selected[i,]
  
  if (is.all.sp == "T") { # all speaker averaged data
    gloss.list[i] <- selected[i,]$Normtime # Revise `gloss.list` so it has same order as plots
  } else { # individual speaker's data
    gloss.list[i] <- selected[i,]$X1 # Revise `gloss.list` so it has same order as plots
  } # else
  
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

} # loop through seleted N-Pron items

# PRINT THE LEGEND

# First, get new melody list and wordlist with phonological representation and Eng. glosses, 
# Second, replace old melody labels with new ones & add variables for phonological rep & Eng gloss

# Read in a new wordlist to use as a lookup table
wordlist <- read.csv(file = "/Users/Kevin/Dropbox/_exp_data/NPron/N-Pron wordlist.csv", header = T, sep = "\t", encoding = "UTF8")

m <- gsub("_(.+)_","\\1",gloss.list, perl=T) # Create vector for the melody (sript off "_")
ph <- gsub("_(.+)_","\\1",gloss.list, perl=T) # Create vector for phonological representation
en <- gsub("_(.+)_","\\1",gloss.list, perl=T) # Create vector for English gloss

# Using lapply, loop over items in new$ph and match values to the look up table. store in new$ph.
#   lapply creates a list of factors so I had to unlist it and turn it into characters

# Replace the Spanish glosses with melodies
m <- as.character(unlist(lapply(m, function(x) wordlist$mel[match(x, wordlist$g)])))

# Replace Spanish glosses in `ph` with phonological representation from `wordlist`
ph <- as.character(unlist(lapply(ph, function(x) wordlist$ipa[match(x, wordlist$g)])))

# Replace Spanish glosses in `en` with English glosses from `wordlist`
en <- as.character(unlist(lapply(en, function(x) wordlist$en[match(x, wordlist$g)])))

# OPTION IF LEGEND IS OUTSIDE THE PLOT (as defined by the par(mar=c(bottom, left, top, right)) settings)
#par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot

# USE WHEN LINE TYPES IN LEGEND ARE SEQUENTIAL beginning with 1
legend("bottomleft", 
       #inset=c(-0.3, 0), # use for legend outside plot
       legend=paste0(m,": ",ph," ‘",en,"’"), 
       col=colour, 
       lty=1:nrow(selected), 
       bty="n", 
       lwd=2)

if (output == "pdf") {
  # reset plotting parameters
  dev.off()
} # shut off pdf device if pdf output was chosen
  