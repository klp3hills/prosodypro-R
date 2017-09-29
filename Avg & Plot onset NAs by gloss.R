#################################################################################################
# Average & plot f0 data with NAs for onsets
# 
# This script will average and plot f0 data (e.g. output from ProsodyPro) for selected glosses.
# Plots for up to 6 words will have a different line colour and line type. 
# Additional plots will repeat line colours and types.
# 
# ASSUMPTIONS:
# * The raw, unaveraged data (for all speakers) is stored in variable `dat`
# * NAs have been inserted for each onset (there are other scripts to plot data without onset NAs)
# * `dat` contains a `g` variable containing the gloss of each item
# * The list of new melody labels is stored in `/Users/Kevin/Fieldwork-vmj/Tone - experiments/new_melody_list.csv`
# 
# INSTRUCTIONS:
# * Set the parameters/options in the USER SETUP section: 
#   - The glosses of the words to be plotted
#   - The columns containing the f0 data
#   - The main title and subtitle
#   - The y-range for the plot
#   - The output: screen or pdf
# * Click the `Source` button (upper right of window) or use the source() command to run the script
# 
#################################################################################################

#------------------------------------------------------------------------------------------------
# USER SETUP
#------------------------------------------------------------------------------------------------

# CHOOSE THE MELODIES TO BE PLOTED:
gloss.list <- c("mi_chapulin", "chapulin", "milpa")

# INDICATE THE COLUMNS IN THE DATAFRAME CONTAINING F0 DATA:
data.cols <- 2:41

# INDICATE THE PLOT TITLE & SUBTITLE (become part of filename, as well)
title <- "LH, L!H, (L)HL (citation)"
s.title <- "Mean f0 - Males"

# CHOOSE the Y-AXIS range
yrange = c(100,220)

# CHOOSE output to "screen" or "pdf"
#output <- "screen"
output <- "pdf"


#------------------------------------------------------------------------------------------------
# PLOT SETUP
#------------------------------------------------------------------------------------------------

# Store data for the selected melodies
items <- dat[dat$g %in% gloss.list,]

# Initialize `melody.list`
melody.list <- vector(length = 0)

xmax <- length(data.cols)

#------------------------------------------------------------------------------------------------
# PLOT SETUP
#------------------------------------------------------------------------------------------------

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
par(mar=c(5, 4, 4, 2) + 0.1) 

# Set up the colours for up to FOUR plots to be plotted in each plot area
colour = c("black", "red", "green", "blue", "brown", "orange")


#------------------------------------------------------------------------------------------------
# BEGIN PLOTTING
#------------------------------------------------------------------------------------------------

# Make a blank plot
plot(1:xmax, 
     ylim=yrange, 
     type="n", 
     lwd=2, 
     ylab="F0 (Herz)",
     xlab="Normalized Time", 
     main=paste0(title),
     #sub=s.title
     )

for (i in 1:length(gloss.list)) { # begin loop through melodies to be plotted
  
  # Average the data for word gloss i
  plot.dat <- sapply(items[items$g==gloss.list[i], data.cols], mean, na.rm=T)
  
  # Get the melody of this item and add it to `melody.list`
  melody.list <- c(melody.list, unique(items[items$g==gloss.list[i],]$m))
  
  lines(1:xmax, plot.dat, 
        ylim=yrange, 
        type="l", 
        lwd=2, 
        lty=i, # line type, 1=solid, 2=dashed, 3=dotted, 4=dotdash,...6
        col=colour[i],
        ylab="F0 (Herz)",
        xlab="Normalized Time")
  
} # for loop through melody.list

# PRINT THE LEGEND

# First, get new melody list and wordlist with phonological representation and Eng. glosses, 
# Second, replace old melody labels with new ones & add variables for phonological rep & Eng gloss

# Read in list of new melody labels
new.melody.list <- read.csv(file="/Users/Kevin/Fieldwork-vmj/Tone - experiments/new_melody_list.csv", header=T, sep="\t", encoding="UTF-8")

# Read in a new wordlist to use as a lookup table
new.wordlist <- read.csv(file = "/Users/Kevin/Fieldwork-vmj/Tone - experiments/2011 single words (108)/2011 wordlist (updated phonetic).csv", header = T, sep = "\t", encoding = "UTF8")

ph <- gloss.list # Create vector for phonological representation
en <- gloss.list # Create vector for English gloss

# Using lapply, loop over items in new$ph and match values to the look up table. store in new$ph.
#   lapply creates a list of factors so I had to unlist it and turn it into characters

# Replace the melody labels in `melody.list` with new ones from `new.melody.list`
melody.list <- as.character(unlist(lapply(melody.list, function(x) new.melody.list$new_m[match(x, new.melody.list$old_m)])))

# Replace Spanish glosses in `ph` with phonological representation from `new.wordlist`
ph <- as.character(unlist(lapply(ph, function(x) new.wordlist$Phonetic[match(x, new.wordlist$Spanish)])))

# Replace Spanish glosses in `en` with English glosses from `new.wordlist`
en <- as.character(unlist(lapply(en, function(x) new.wordlist$English[match(x, new.wordlist$Spanish)])))


#------------------------------------------------------------------------------------------------
# USE WHEN LINE TYPES IN LEGEND ARE SEQUENTIAL beginning with 1
legend("bottomleft", 
       legend=paste0(melody.list,": ",ph," ‘",en,"’"), 
       col=colour, 
       lty=1:length(melody.list), 
       bty="n",
       cex=0.8,
       lwd=2)
#------------------------------------------------------------------------------------------------

if (output == "pdf") {
  # reset plotting parameters
  dev.off()
} # shut off pdf device if pdf output was chosen

# List the item glosses and melodies that were plotted
print(unique(items[,c("m","g")]))
