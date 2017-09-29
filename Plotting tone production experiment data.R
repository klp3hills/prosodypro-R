##################################################################################
# PLOTTING TONE PRODUCTION EXPERIMENTAL DATA
##################################################################################

##################################################################################
PLOT SINGLE SPEAKER DATA (single N or N-N) FROM PROSODY PRO OUTPUT DIRECTORY
##################################################################################
# This is the procedure to make a plot for a single experiment item for a single
# speaker, using the ProsodyPro setup that has each token for each list item for
# each speaker in a separate folder and where it also puts the output files.
# 1. Copy the directory FULL PATH to clipboard
# 2. Paste the full path as the FIRST ARGUMENT to F0PLOT function B/W QUOTES
# 3. Add the min and max y-axis values as minfreq and maxfreq parameters
# 4. Other parameters to adjust the plot may also be set. See f0plot.R.

f0plot("/Users/Kevin/U of A/Summer 2011 fieldwork/Summer 2011 fieldwork audio/2011 Wordlist/1 Echi wordlist 2011/7 mi_rodilla/", minfreq=100, maxfreq=250)

##################################################################################
N-N PHRASE PLOTS
##################################################################################

# ***LOAD LATEST N-N R WORKSPACE*** (updated 2017-06-16)
rm(list = ls()) # Clear the workspace first

# UPDATED June 2017
load("~/Fieldwork-vmj/Summer 2012 audio/2012 NVNV genitive/ProsodyPro output- NVNV males 2 2013-09/N-N NVNV - ALL ProsodyPro data, revised lz normalization = male.lz2 2013-10-16.RData")

# PLOT A SINGLE PHRASE MELODY FROM sorted.normf0: Substitute the list item number for the number in the double sq. brackets, [[i]]

plot(1:80, as.numeric(sapply(as.list(sorted.normf0[[1]][2:81]), mean)), type="l", lwd=3, xlab="Normalized Time", ylab="F0 (Hz)", sub="All Speakers", main=paste("Normalized F0", sorted.normf0[[1]]$Melody[1], sep=": "), ylim=c(120,200), col="black")

abline(v=c(1,10,20,30,40,50,60,70,80)) # add segment divisions
abline(v=40, lwd = 2) # add a heavey word divider

# ADD ANOTHER PLOT TO THE EXISTING PLOT
lines(1:80, as.numeric(sapply(as.list(sorted.normf0[[2]][2:81]), mean)), type="l", lwd=3, col="red")


################# 
# MAKE SINGLE PLOTS WITH N1 FIXED AND ALL OTHER MELODIES IN N2 or vice versa
#################

# Plot characters and colours
pc = c(16, 0, 1, 2, 15, 4, 5, 6)
colour = c("black", "red", "green", "blue", "cornflowerblue", "darkmagenta", "tan4")

# create vector of N1 frame numbers for each melody
N1.frames = cbind(
HHaN1.frames = 1:7,
HHbN1.frames = 8:14,
HLaN1.frames = 15:21,
HLbN1.frames = 22:28,
LHN1.frames = 29:35,
LMN1.frames = 36:42,
MLN1.frames = 43:49)

# create vector of N2 frame numbers for each melody
N2.frames = cbind(
HHaN2.frames = seq(1,43,7),
HHbN2.frames = seq(2,44,7),
HLaN2.frames = seq(3,45,7),
HLbN2.frames = seq(4,46,7),
MLN2.frames = seq(5,47,7),
LHN2.frames = seq(6,48,7),
LMN2.frames = seq(7,49,7))

# create a list with the N1 [[1]] and N2 [[2]] frame numbers for ea. melody
frames = list(N1.frames, N2.frames)

# IMPORTANT: FIRST SET THESE PARAMETERS

# Set the data here:
plot.list = lz.normf0.list

# Set the yrange here: use 120, 200 for Hz (males), and -2, 2 for z-scores
yrange = c(-2, 2)

# Set the melody column number (83 for Hz data, 84 for normalized data)
melcol = 84

# Set fixed phrase position here:
pos = c("N1","N2")

# Set fixed phrase melody here:
N1mel = c("HHa", "HHb", "HLa", "HLb", "LH", "LM", "ML")
N2mel = c("HHa", "HHb", "HLa", "HLb", "ML", "LH", "LM")
mels = cbind(N1mel, N2mel)
title = "Log Z-score Normalized -"
s.title = "NVNV N-N (Males)"
ylabel = "Frequency (log10 z-scores)"

#---------------------
# Plot all frames for EACH MELODY in N1 and N2 position
# BEGIN PLOT: select from here to "END PLOT" and press ⌘  ENTER

for (j in 1:2) {  # loop through all melodies in N1 and in N2

	for (k in 1:7) { # loop through the 7 melodies in position N[j] (i.e. N1 or N2)

		melody = frames[[j]][,k]  # Get the frame numbers for a melody in pos N
		# create melody list for each melody in the target frames
		melody.list = vector(length=7)
		for (l in 1:7) { melody.list[l] = plot.list[[melody[l]]][1,melcol] }		
		# Turn on the pdf device
		pdf(paste(title,pos[j],"=",mels[k,j],".pdf",sep=" "),width=7, height=5)

		# Create a blank plot
		par(mar=c(5, 4, 4, 8) + 0.1) # make space for the legend at the right margin

		# Set up the blank plot
		plot(1:80, type="n", main=paste(title,pos[j],"=",mels[k,j],sep=" "), sub=s.title, xlab="Normalized Time", ylab=ylabel, ylim=yrange)

		# Add the F0 plots for all 7 frames of a particular melody in N position
		for (i in 1:7) {
			lines(1:80, sapply(plot.list[[melody[i]]][,2:81],mean, na.rm=T), type="b", lwd=1, lty=1, col=colour[i], pch=pc[i], cex=0.5)
			}
	
		# Add segment and word boundaries
		abline(v=c(10,30,50,70), lty=2, lwd=0.75) # make dotted line between C & V within syll.
		abline(v=c(1,20,40,60,80)) # make solid lines at syllable boundaries
		abline(v=40, lwd=2) # make a thick line at the word boundary

		# create the legend	
		par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot
	    legend("topright", inset=c(-0.35, 0), legend=paste(melody," ",melody.list,sep=""), title="Frames", col=colour, pch=pc, bg="light grey", lwd=2, cex=1)
    	par(xpd=FALSE)  # Turn clipping back on
    
		dev.off() # Turn off the pdf device
		
		} # k loop through the 7 melodies in position N
		
	} # j loop through position N1 and N2
    
# END PLOT

#---------------------

#********************************************************************************
# PLOT ALL FRAMES for ONE FIXED MELODY in ONE PHRASE POSITION
# OR, MAKE YOUR OWN COMBINATIONS by changing the frame numbers in "melody" below
# Then select from "BEGIN PLOT" to "END PLOT" and press ⌘-ENTER
# **IMPORTANT** This code is for plotting from lists, not data.frames!
#********************************************************************************

# ***LOAD LATEST N-N R WORKSPACE*** (updated 2017-06-16)
rm(list = ls()) # Clear the workspace first

# UPDATED June 2017
load("~/Fieldwork-vmj/Summer 2012 audio/2012 NVNV genitive/ProsodyPro output- NVNV males 2 2013-09/N-N NVNV - ALL ProsodyPro data, revised lz normalization = male.lz2 2013-10-16.RData")

# create melody list for each melody in the target frames
# REMEMBER to choose the same N position number and fixed melody number here as in pdf() & legend() below

# SELECT THE DATA: plot.list = to the LIST from which the data will be taken
#   e.g. sorted.normf0, lz.normf0.list
plot.list = lz.normf0.list # male data; list storing log z-score normf0 data for ea. melody separately
plot.list = sorted.normf0 # this is only male data

# SET the yrange here: use 120, 200 for Hz (males), and -2, 2 for z-scores
yrange = c(-1.5, 1.5) # for LZ data
yrange = c(120,200) # for Hz data

# SET the melody column number (83 for Hz data, 84 for normalized data)
melcol = 84 # for LZ data
melcol = 83 # for Hz data

# SET the title, subtitle and axis labels here
title = "N-N: H-HL, H-L"
s.title = paste("N-N, N",pos," = ",mel,sep="")
s.title = "male (Hz)"

ylabel = "Frequency (log10 z-scores)"
ylabel = "Frequency (Hz)"
xlabel = "Normalized Time"

# Set the position of the fixed melody = 1 or 2
pos = 1

# Set the fixed melody label
mel = "HHa"
mel = "LH"

# Set the unique part of the filename (it will be combined with pos and mel + .pdf in creating the full filename)
fn = "Log Z-score Normalized NVNV N-N Males - "
fn = "NVNV N-N Males (Hz) - "

# CHANGE THIS...
# Get the frame numbers for a melody in pos N[k]
melody = frames[[1]][,1]  
melody.list = vector(length=7)

# OR MAKE UP YOUR OWN COMBINATION:
# melody = numbers of the frames (from the plot with all 49+2 melody combinations = the number of the list containing the data for that melody combination from the sorted.normf0 or lz.normf0.list) in the order they should appear
melody = c(8,9,13) # HHb-HHa, HHb-HHb, HHb-LH
melody = c(1,2) # HHa-HHa, HHa-HHb
melody = c(10,11,13) #HHb-HLa, HHb-HLb, HHb-LH
melody = c(12,5,33) #HHb-ML, HHa-ML, LH-ML
melody = c(50,9,12) # HHb-HHb alternate
melody = 8:14 # HHb-N (all combos)
melody = c(45,46,39) # ML-HLa, HLb, LM-HLb
melody = c(45,46,17,18) # ML-HLa, HLb, HLa-HLa, HLb
melody = 47 # ML-ML
melody = c(47,40,33,26,19,12,5) # all N-ML
melody = c(3,45) # HHa-HLa, ML-HLa
melody = c(6,7,48,49) # HHa-LH, HHa-LM, ML-LH, ML-LM
melody = 33 # LH-ML
melody = 45 # ML-HLa
melody = c(3,4,6) # HHa-HLa, HHa-HLb, HHa-LH
melody = c(8,9,13,6) # HHb-HHa, HHb-HHb, HHb-LH, HHa-LH
melody = c(8,13,6) # HHb-HHa, HHb-LH, HHa-LH
melody = c(12,14) # HHb-ML, HHb-LM
melody = c(42,49) # LM-LM, ML-LM
melody = c(42,49,38,33,45) # LM-LM, ML-LM, LM-HLa, LH-ML, ML-HLa
melody = c(42,38,45,49) # LM-LM, ML-LM, ML-HLa, ML-LM
melody = c(42,38,45,33) # LM-LM, ML-LM, ML-HLa, LH-ML
melody = c(5,12,33) # H(L)-L, H-L, LH-L
melody = 10 # HHb-HL
melody = 43 # ML-HHa
melody = 45 # ML-HLa
melody <- 29 # LH-HHa
melody <- c(30,31) # LH-HHb, LH-HLa
melody <- c(30,29,31) # LH-HHb, LH-HLa
melody <- c(5,12) # HHa-ML, HHb-ML
melody <- c(7,14) # HHa-LM, HHb-LM
melody <- c(2,3) # HHa-HHb, HHa-HLa
melody <- c(15,16,17,20) # HLa-HHa, HLa-HHb, HLa-HLa, HLa-LH
melody <- c(17,18) # HLa-HLa, HLa-HLb
melody <- c(6,13,34) # HHa-LH, HHb-LH, LH-LH

# RE-RUN THIS!
# melody.list = the melody labels for the frames in melody
# Get the data for each frame in melody and store it in melody.list
melody.list = vector(length=0)
for (i in 1:length(melody)) { melody.list[i] = plot.list[[melody[i]]][1,melcol] }

# UPDATE: convert old melody labels to new ones
new.mel <- c("H", "H(L)", "HL", "L", "L!H", "LH", "(L)HL", "x_H(L)", "x_L")
old.mel <- c("HHa", "HHb", "HLa", "ML", "LM", "LH", "HLb", "x_HHb", "x_ML")
mel.table <- data.frame(old.mel, new.mel)
for (j in 1:nrow(mel.table)) {
  melody.list <- gsub(as.character(mel.table[j,"old.mel"]), as.character(mel.table[j,"new.mel"]), melody.list, perl=T)
}


#------------------------------------------------------------------------------------
# CHOOSE screen or pdf output
output = "pdf"
#output = "screen"

# BEGIN PLOT HERE:
# Turn on the pdf device
# If you need special characters in the plot uncomment cairo_pdf below, otherwise use pdf() because it will have a smaller size
if (output == "pdf") {
  # Turn on Cairo pdf device for special characters anywhere in plot
  library(Cairo)
  Cairo(file=paste0(title,s.title,".pdf"),
        type="pdf",
        family="ArialUnicodeMS", 
        width=7, 
        height=5, 
        units="in")
} # if turn on pdf device if pdf output was chosen

# Create a blank plot
#par(mar=c(2, 4, 3, 8) + 0.1) # make space for the legend at the right margin
par(mar=c(5, 4, 4, 2) + 0.1) # use for legend inside plot

plot(1:80, 
     main=title, 
     #sub=s.title, 
     type="n", 
     #xaxt="n", # remove the box around the plot
     ylab=ylabel, 
     xlab=xlabel,
     ylim=yrange)
#title(main=title)
#title(xlab=xlabel, line=1)
#title(sub=s.title, line=2.5)

# Add the F0 plots for all frames in melody
for (i in 1:length(melody)) {
	lines(1:80, sapply(plot.list[[melody[i]]][,2:81],mean), 
	      type="l", 
	      lwd=2, 
	      lty=i, 
	      col=colour[i], 
	      #pch=pc[i], # to use special plotting chararacters
	      cex=0.8)
	}
	
#abline(v=c(10,30,50,70), lty=2, lwd=0.75) # make dotted line between C & V within syll.
#abline(v=c(1,20,40,60,80)) # make solid lines at syllable boundaries
abline(v=c(1,20,40,60,80), lty=2, lwd=0.75) # make dotted lines at syllable boundaries
#abline(v=40, lwd=2) # make a thick line at the word boundary
abline(v=40, lwd=1) # make a narrow solid line at the word boundary

# create the legend	
par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot
    legend("bottomleft", 
           #inset=c(-0.35, 0), # use to put legend outside right edge of plotting area
           #inset=c(.05,0), # this places the plot inside the last syllable
           inset=c(.025,0), # this places the plot inside the first syllable for bottomeleft
           legend=melody.list, # to get frame #s use: paste(melody," ", melody.list,sep="")
           title="Melodies", 
           col=colour, 
           #pch=pc, 
           lty=1:length(melody.list),
           lwd=2, 
           cex=1, 
           bty="n")
    par(xpd=FALSE)  # Turn clipping back on
    
    
    if (output == "pdf") {
      # reset plotting parameters
      dev.off()
    } # shut off pdf device if pdf output was chosen
    
# END PLOT
#------------------------------------------------------------------------------------

#################
# Plot a N-N contour with the maximum mean for each TBU
# (can change from max to other functions)

# Get the data
dat = read.csv(file.choose(), header=T, sep="\t", encoding="UTF-8")
dat <- lmv.hhb.ml

# Get the maximum mean for each TBM
max(as.numeric(sapply(lmv.hhb.ml[,2:21], mean))) -x
c(max(as.numeric(sapply(lmv.hhb.ml[,22:41], mean)))) -x
c(max(as.numeric(sapply(lmv.hhb.ml[,42:61], mean)))) -x
c(max(as.numeric(sapply(lmv.hhb.ml[,62:81], mean)))) -x

fn = "Speaker LMV - N-N: H(L)-L"
melody.list = "HHb-ML"

# UPDATE: convert old melody labels to new ones
new.mel <- c("H", "H(L)", "HL", "L", "L!H", "LH", "(L)HL", "x_H(L)", "x_L")
old.mel <- c("HHa", "HHb", "HLa", "ML", "LM", "LH", "HLb", "x_HHb", "x_ML")
mel.table <- data.frame(old.mel, new.mel)
for (j in 1:nrow(mel.table)) {
  melody.list <- gsub(as.character(mel.table[j,"old.mel"]), as.character(mel.table[j,"new.mel"]), melody.list, perl=T)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# CHOOSE screen or pdf output
output = "pdf"
#output = "screen"

# BEGIN PLOT HERE:
# Turn on the pdf device
# If you need special characters in the plot uncomment cairo_pdf below, otherwise use pdf() because it will have a smaller size
if (output == "pdf") {
  # Turn on Cairo pdf device for special characters anywhere in plot
  library(Cairo)
  Cairo(file=paste0(title,s.title,".pdf"),
        type="pdf",
        family="ArialUnicodeMS", 
        width=7, 
        height=5, 
        units="in")
} # if turn on pdf device if pdf output was chosen

# Create a blank plot
#par(mar=c(2, 4, 3, 8) + 0.1) # make space for the legend at the right margin
par(mar=c(5, 4, 4, 2) + 0.1) # use for legend inside plot

# Make the plot
plot(1:80, as.numeric(sapply(lmv.hhb.ml[,2:81], mean)), 
     ylim=c(180,300), typ="l", 
     xlab="Normalized Time", 
     ylab="F0 (Herz)", 
     #xaxt="n",
     main=fn)


# Add the max pitch labels
#text(seq(15,75,20),282,round(x), cex=0.8)

# Add the segment, syllable, word boundaries
#abline(v=seq(10,70,20),lty=3) # dotted onset boundary
abline(v=seq(0,80,20),lty=3, lwd=0.75) # syllable boundary
abline(v=40, lwd=1)

# Add the segment labels
#par(family="ArialUnicodeMS") # Set unicode font to display IPA; DON'T USE FOR PDF (but can for cairo_pdf)
#text(seq(5,75,10),295,c("n","ɨ","n","ʲɨ","n","ɨ","n","ʲɨ"), cex=1.5)

# create the legend	
par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot
legend("topright", 
       #inset=c(-0.35, 0), # use to put legend outside right edge of plotting area
       inset=c(.05,0), # this places the plot inside the last syllable
       legend=melody.list, # to get frame #s use: paste(melody," ", melody.list,sep="")
       title="Melodies", 
       col=colour, 
       #pch=pc, 
       lty=1:length(melody.list),
       lwd=2, 
       cex=1, 
       bty="n")
par(xpd=FALSE)  # Turn clipping back on


if (output == "pdf") {
  # reset plotting parameters
  dev.off()
} # shut off pdf device if pdf output was chosen

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#################

# PLOT ALL 49 MELODY COMBINATIONS (if there are "extra" melodies for multiple realizations of the same melody combination, there will be more than 49 plots (e.g. 51 if 2 melody combinations each have two realizations), but this will happen automatically because the number of plots is determined by the length of sorted.normf0.

# IMPORTANT! First set these parameters:
plot.list = lz.normf0.list
xmax = 80
xmin = 1
xrange = c(xmin, xmax)
yrange = c(-2, 2)

# setup numbering for the plots
x = c("I", "II", "III", "IV", "V", "VI", "VII")
y = letters[1:7]
z = NULL
for (i in 1:7) {
	for (j in 1:7) {
		z = c(z, (paste(x[i], ".", y[j], ". ", sep="")))
		}
}
z = c(z, "II.b (alt) ", "VII.b (alt) ") # numbering for alternate plots

# frames to print extra values (min or max)
min55 = c(8,27)
min65 = c(7,14,35,42)
min45 = c(18,25,46,48)
max15 = 43:49
max25 = c(1,2,3,5,6,7,8,9,10,11,13,14,17,22,23,24,35,26,27,28,50)
max35 = 4
max45 = c(5,29,33,36,40)
max55 = c(24,31,38,44)
max65 = c(23,37)
max75 = c(22,43,51)

# Turn on pdf() device
pdf("NVNV N-N males LZ normalized.pdf", width=13, height=7.85)

# set up a 7 x 7 plot, with .25" margins
par(mfrow=c(7,7), mai=c(.25, .35, .25, .25))

# Create the plots
for (i in 1:length(plot.list)) { 
	plot(1:xmax, sapply(plot.list[[i]][2:81], mean, na.rm=T), type="l", ylim=yrange, xlim=xrange,cex.main=.8, main=paste(i, plot.list[[i]]$Melody[1], sep=" "))
	abline(v=c(10,30,50,70), lty=3, lwd=0.5) # make dotted line between C & V within syll.
	abline(v=c(1,20,40,60,80), lwd=0.75) # make solid lines at syllable boundaries
	abline(v=40, lwd=1.5) # make a thicker line at the word boundary
	text(13,yrange[2]*0.9, round(mean(plot.list[[i]][,21], na.rm=T), 3), cex=.8) # add offset Hz at syllable 1 boundary
	text(33,yrange[2]*0.9, round(mean(plot.list[[i]][,41], na.rm=T), 3), cex=.8) # add offset Hz at syllable 2 (word 1) boundary
	text(53,yrange[2]*0.9, round(mean(plot.list[[i]][,61], na.rm=T), 3), cex=.8) # add offset Hz at syllable 3 boundary
	text(73,yrange[2]*0.9, round(mean(plot.list[[i]][,81], na.rm=T), 3), cex=.8) # add offset Hz at syllable 4 (word 2) boundary
	if (i %in% min55) { text(53,yrange[1]*0.9, round(min(sapply(plot.list[[i]][,51:60],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	if (i %in% min65) { text(63,yrange[1]*0.9, round(min(sapply(plot.list[[i]][,61:70],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	if (i %in% min45) { text(43,yrange[1]*0.9, round(min(sapply(plot.list[[i]][,41:50],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	if (i %in% max15) { text(13,yrange[1]*0.9, round(max(sapply(plot.list[[i]][,11:20],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	if (i %in% max25) { text(23,yrange[1]*0.9, round(max(sapply(plot.list[[i]][,21:30],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	if (i %in% max35) { text(33,yrange[1]*0.9, round(max(sapply(plot.list[[i]][,31:40],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	if (i %in% max45) { text(43,yrange[1]*0.9, round(max(sapply(plot.list[[i]][,41:50],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	if (i %in% max55) { text(53,yrange[1]*0.9, round(max(sapply(plot.list[[i]][,51:60],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	if (i %in% max65) { text(63,yrange[1]*0.9, round(max(sapply(plot.list[[i]][,61:70],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	if (i %in% max75) { text(73,yrange[1]*0.9, round(max(sapply(plot.list[[i]][,71:80],mean, na.rm=T), na.rm=T), 3), cex=.8) }
	
	}
	
# reset plotting parameters
dev.off()


#################################################################################
PLOT SINGLE N MELODIES
#################################################################################

# PLOT ALL SINGLE N MELODIES FROM MALE DATAFRAMES STORED IN A LIST

# FIRST CHANGE THESE PARAMETERS!
# Change this to the correct list with the plotting data, y-range, x-range:
plot.list = lz.normf0.list
plot.list = dat
yrange = c(-1.5,1.5)
xmin = 1; xmax = 40
xrange = c(xmin,xmax)
title = "Mean Z-score Normalized Log F0"
s.title = "Male Single N (NVNV)"

# Plot characters and colours
pc = c(16, 0, 1, 2, 15, 4, 5, 6)
pc = c(16, 17)
colour = c("black", "red", "green", "blue", "cornflowerblue", "darkmagenta", "tan4", "darkorange")
colour = c("black","red")

# create melody list
melody.list = vector(length=length(plot.list))
for (i in 1:length(plot.list)) { melody.list[i] = plot.list[[i]]$Melody[1] }

# BEGIN PLOT HERE
# Create a blank plot
par(mar=c(5, 4, 4, 8) + 0.1) # make space for the legend at the right margin

plot(1:xmax, type="n", main=title, sub=s.title, xlab="Normalized Time", ylab="Frequency (Hz)", ylim=yrange, xlim=xrange)

for (i in 1:length(plot.list)) {
	lines(1:xmax, sapply(plot.list[[i]][,2:(xmax+1)],mean, na.rm=T), type="b", lwd=1.5, lty=1, col=colour[i], pch=pc[i], cex=0.8)
	}

abline(v=seq(10, xmax, 20), lty=3, lwd=0.5) # make dotted line between C & V within syll.
abline(v=seq(20, xmax, 20)) # make solid lines at syllable boundaries
if (xmax > 40) { abline(v=40, lwd=2) } # make a thick line at the word boundary if N-N

# create the legend	
par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot
    legend("topright", inset=c(-0.25, 0), legend=melody.list, title="Frames", col=colour, pch=pc, bg="light grey", lwd=2, cex=1)
    par(xpd=FALSE)  # Turn clipping back on
    
# END PLOT

# HERE'S THE OLD WAY OF PLOTTING ALL SINGLE N MELODIES...
# Get list order
for (i in 1:length(male.dat)) { cat(male.dat[[i]][1,1], male.dat[[i]][1,42], male.dat[[i]][1,43], "\n") }

# Extract the data for each melody and store in a separate variable for ea. melody
HHa = male.dat[[1]]
HHb = male.dat[[2]]
HLa = male.dat[[3]]
HLb = male.dat[[4]]
LHa = male.dat[[5]]
LHb = male.dat[[6]]
LM = male.dat[[7]]
ML = male.dat[[8]]

# Plot the first (ML) melody in black
plot(1:40, as.numeric(sapply(as.list(ML[,2:41]),mean)), type="l", lwd=3, lty=1, main="Average f0 Normalized Time", sub="Single N Melodies (Males)", xlab="Normalized Time", ylab="Frequency (Hz)", ylim=c(120, 200))

# Add HHa melody in cornflowerblue
lines(1:40, as.numeric(sapply(as.list(HHa[,2:41]),mean)), type="l", lwd=3, lty=6, col="cornflowerblue")

# Add HHb melody in blue
lines(1:40, as.numeric(sapply(as.list(HHb[,2:41]),mean)), type="l", lwd=3, lty=5, col="blue")

# Add LM melody in green
lines(1:40, as.numeric(sapply(as.list(LM[,2:41]),mean)), type="l", lwd=3, lty=4, col="green")

# Add HLa melody in brown
lines(1:40, as.numeric(sapply(as.list(HLa[,2:41]),mean)), type="l", lwd=3, lty=3, col="brown")

# Add HLb melody in tan4
lines(1:40, as.numeric(sapply(as.list(HLb[,2:41]),mean)), type="l", lwd=3, lty=3, col="tan4")

# Add LHa melody in blueviolet
#lines(1:40, as.numeric(sapply(as.list(LHa[,2:41]),mean)), type="l", lwd=3, lty=2, col="blueviolet")

# Add LHb melody in red
lines(1:40, as.numeric(sapply(as.list(LHb[,2:41]),mean)), type="l", lwd=3, lty=2, col="red")

# Add the segment boundaries
abline(v=c(1, seq(from=10, to=40, by=10)), lwd=.8)
abline(v=20,lwd=2)

# Add a legend in the bottom left corner (where there's enough space)
legend("topleft", c("ML", "HHa", "HHb", "LM", "HLa", "HLb", "LH"), cex=0.8, col=c("black","cornflowerblue", "blue", "green", "brown", "tan4", "red"), lty=c(1, 6, 5, 4, 3, 3, 2), lwd=2, bg="light grey")

----------------------------------------------------------------------------------
# COMPARING TWO (OR MORE) MELODIES FROM DATAFRAMES
# Note: See below for plotting melodies when dataframes are stored in a giant list.


# SPECIFY THE DATA...
# first...
dat = male.df
dat = male.lz2
unique(dat$m)
unique(dat$g)

# then to plot, CHOOSE WHICH DATA TO DISPLAY...

# 5 basic disyllabic pitch contours
dat1 = dat[grep("-punyo",dat$Normtime),]	# ML
dat2 = dat[grep("-chapulin",dat$Normtime),]	# HLa
dat3= dat[grep("-semilla",dat$Normtime),]	# HHa
dat4 = dat[grep("-camisa",dat$Normtime),]	# LM
dat5 = dat[grep("-cochino",dat$Normtime),]	# LH

melody.list = c("ML tʃiki 'fist'", "HL tɨka 'grasshopper'", "HH tʃɨtɨ 'seed'", "LM kotõ 'shirt'", "LH kutʃi 'pig'") #5

title = "Disyllabic Noun Melodies in Isolation"

# all 7 underlying melodies
dat1 = dat[grep("-punyo",dat$Normtime),] 	# ML
dat2 = dat[grep("-chapulin",dat$Normtime),] # HLa
dat3= dat[grep("-semilla",dat$Normtime),] 	# HHa
dat4 = dat[grep("-camisa",dat$Normtime),]	# LM
dat5 = dat[grep("-mango",dat$Normtime),]	# LH
dat6 = dat[grep("-rodilla",dat$Normtime),]	# HHb
dat7 = dat[grep("-ardilla",dat$Normtime),]	# HLb

# all 7 NVNV underlying melodies
dat1 = dat[dat$g=="mazorca",]	# ML
nrow(dat1)
dat2 = dat[dat$g=="charca",]	# HLa
nrow(dat2)
dat3 = dat[dat$g=="hermano",]	# HHa
nrow(dat3)
dat4 = dat[dat$g=="mole",]		# LM
nrow(dat4)
dat5 = dat[dat$g=="mango",]		# LH
nrow(dat5)
dat6 = dat[dat$g=="sangre",]	# HHb
nrow(dat6)
dat7 = dat[dat$g=="murcielago",] # HLb
nrow(dat7)

melody.list = c("ML nɨnʲɨ 'ear of corn'", "HLa minʲi 'pool'", "HHa nʲanʲi 'brother'", "LM muli 'chili sauce'", "LH maŋgu 'mango'", "HHb nɨnʲɨ 'blood'", "HLb lome 'bat'")

title = "Disyllabic NVNV Noun Melodies in Isolation"

# HHa vs HHb (semilla ~ rodilla)
dat1 = dat[grep("-semilla",dat$Normtime),]
dat2 = dat[grep("-rodilla",dat$Normtime),]
title = "HHa vs. HHb - Mean F0"
melody.list = c("HHa 'seed'", "HHb 'knee'") # 2

# HLa vs HLb (cana ~ ardilla)
dat1 = dat[grep("-cana",dat$Normtime),]
dat2 = dat[grep("-ardilla",dat$Normtime),]
title = "HLa vs. HLb - Mean F0"
melody.list = c("HLa 'grey hair'", "HLb 'squirrel'") # 2


# N ~ N-1s comparisons
# HHa
dat1 = dat[grep("-milpa",dat$Normtime),]
dat2 = dat[grep("-mi_milpa",dat$Normtime),]
dat3 = dat[grep("-rodilla",dat$Normtime),]
title = "HHa+1s, HHa, HHb"
melody.list = c("HHa itu 'cornplant'", "HHa-1s iti 'my cornplant'", "HHb tʃɨtɨ 'knee'") # 3

# HHb
dat1 = dat[grep("-rodilla",dat$Normtime),]
dat2 = dat[grep("-mi_rodilla",dat$Normtime),]
dat3 = dat[grep("-chapulin",dat$Normtime),]
title = "HHb+1s, HHb, HLa"
melody.list = c("HHb tʃɨtɨ 'knee'", "HHb-1s tʃɨti 'my knee' ", "HLa tɨka 'grasshopper'") # 3

# HHb, HHb-1s, HHa, HHa-1s, HLa
dat1 = dat[grep("-rodilla",dat$Normtime),]
dat2 = dat[grep("-mi_rodilla",dat$Normtime),]
dat3 = dat[grep("-milpa",dat$Normtime),]
dat4 = dat[grep("-mi_milpa",dat$Normtime),]
dat5 = dat[grep("-chapulin",dat$Normtime),]
title = "HHb+1s, HHb, HHa, HHa-1s, HLa"
melody.list = c("HHb tʃɨtɨ 'knee'", "HHb-1s tʃɨti 'my knee' ", "HHa itu 'cornplant'", "HHa-1s iti 'my cornplant'", "HLa tɨka 'grasshopper'") # 3

# ML
dat1 = dat[grep("-elotillo",dat$Normtime),]
dat2 = dat[grep("-mi_elotillo",dat$Normtime),]
dat3 = dat[grep("-camisa",dat$Normtime),]
title = "ML+1s, ML, LM"
melody.list = c("ML isa 'green ears of corn'", "ML-1s ise 'my green ears of corn'", "LM kotõ 'shirt'") # 3


# HLa
dat1 = dat[grep("-chapulin",dat$Normtime),]
dat2 = dat[grep("-mi_chapulin",dat$Normtime),]
dat3 = dat[grep("-rodilla",dat$Normtime),]
title = "N vs N+1s (HHb)"
melody.list = c("HLa 'grasshopper'", "HLa-1s 'my grasshopper' ", "HHb 'knee'") # 3

# LH

# LM

# ML

# Choose a colour list with same number of plots...
col.list = c("black","red") # 2
col.list = c("black","red", "green") # 3
col.list = c("black","red", "green", "blue") # 4
col.list = c("black","red", "green", "blue", "purple") # 5
col.list = c("black","red", "green", "blue", "purple", "brown") #6
col.list = c("black","red", "green", "blue", "purple", "brown", "orange") # 7

# Set the sub-title (at bottom of plot)
s.title = "Mean f0 - Males"

# Set the y-range
yrange = c(120,210)
yrange = c(-2,1.5)

# If you want to make a pdf use these commands...
library(extrafont) # use this if you need to use a particular font
loadfonts() # use this if you need to use a particular font
# If you want to output to .ps files instead of .pdf, use:
# loadfonts(device="postscript")

cairo_pdf(paste(title," - no Cs", ".pdf",sep=""),family="ArialUnicodeMS", width=7, height=5)

# don't use if you need special characters
pdf(paste(title," - no Cs", ".pdf",sep=""),width=7, height=5)

png(paste(title," - no Cs", ".png",sep=""),width=504, height=360)

# Create a plot region with a normal right margin
par(mar=c(5, 4, 4, 2) + 0.1)

# Plot the first melody using normf0 data for the representative word
plot(1:40, sapply(dat1[,2:41],mean,na.rm=T), type="l", lwd=3, main=title, sub=s.title, xlab="Normalized Time", ylab="F0 (log z-score normalized)", ylim=yrange)

# Plot the second word over top of the first using lines() and different colour
lines(1:40, sapply(dat2[,2:41],mean,na.rm=T), type="l", lwd=3, col="red",lty=2)

# Add a third plot
lines(1:40, sapply(dat3[,2:41],mean,na.rm=T), type="l", lwd=4, col="green", lty=3)

# Add a fourth plot
lines(1:40, sapply(dat4[,2:41],mean,na.rm=T), type="l", lwd=3, col="blue", lty=4)

# Add a fifth plot
lines(1:40, sapply(dat5[,2:41],mean,na.rm=T), type="l", lwd=3, col="purple", lty=5)

# Add a sixth plot
lines(1:40, sapply(dat6[,2:41],mean,na.rm=T), type="l", lwd=3, col="brown", lty=6)

# Add a seventh plot
lines(1:40, sapply(dat7[,2:41],mean,na.rm=T), type="l", lwd=2, col="orange", lty=1)

# Add syllable boundary vertical lines to the plot
abline(v=c(1,20,40))

# Add dashed boundary between onset and vowel
abline(v=c(10,30), lty=2)

# create the legend	
par(xpd=TRUE)  # Turn off clipping to allow legend to be outside plot

par(family="ArialUnicodeMS") # Set unicode font to display IPA; DON'T USE FOR PDF (but can for cairo_pdf)

legend("bottomleft", inset=c(0, 0), legend=melody.list, col=col.list, lty=1:length(col.list), bty="n", lwd=2, cex=0.8)

par(xpd=FALSE)  # Turn clipping back on

dev.off() # Turn off the pdf device
  
# Other legends...  
# Add a legend in the corner
legend("bottomright", c("HLa ('chapulin')", "HLa+1s ('mi.chapulin')", "HHb+1s ('mi.rodilla')", "HHb ('rodilla')", "HHa ('milpa')"), cex=0.8, col=c("black","red", "green", "blue", "purple"), lty=c(1,1), lwd=3, title="Plots", bg="light grey")

# A legend with fewer items
legend("bottomleft", c("LM ('gallina')", "LM+1s ('mi.gallina')", "LH ('mango')", "LH+1s ('mi.mango')"), cex=0.8, col=c("black","red", "blue", "green"), lty=1:length(col.list), lwd=3, title="Plots", bg="light grey")

-----------------------------------------------------------------------------------
# COMPARING TWO (OR MORE) MELODIES FROM DATAFRAMES STORED IN A LIST

# Extract the data for each melody and store in a variable
# Look up the item number from the wordlist then extract that list item.
HLaHLa = sorted.normf0[[17]]
HLaHLb = sorted.normf0[[18]]
HLaML = sorted.normf0[[19]]
HLaLM = sorted.normf0[[21]]

# Plot the first (base) melody in black
plot(1:80, as.numeric(sapply(as.list(HHbLH[,2:81]),mean)), type="l", lwd=3, lty=1, main="LZ Normalized F0", sub="Comparison of HHb+{LH, HHa, HHb}", xlab="Normalized Time", ylab="Frequency (Log10 Normalized)", ylim=c(-2, 2))

# Add another melody in red
lines(1:80, as.numeric(sapply(as.list(HHbHHa[,2:81]),mean)), type="l", lwd=3, lty=4, col="red")

# Add another melody in green
lines(1:80, as.numeric(sapply(as.list(HHbHHb[,2:81]),mean)), type="l", lwd=3, lty=3, col="green")

# Add another melody in blue
lines(1:80, as.numeric(sapply(as.list(HLaLM[,2:81]),mean)), type="l", lwd=3, lty=2, col="blue")

# Add the syllable boundaries
abline(v=c(1, seq(from=20, to=80, by=20)), lwd=.8)

# Add the segment boundaries
abline(v=c(seq(from=10, to=70, by=20)), lty=3, lwd=.8)

# Add a heavy line between words in the phrase
abline(v=40, lwd=2)

# Add a legend in the bottom left corner (where there's enough space)
legend("topright", c("HHb-LH", "HHb-HHa", "HHb-HHb"), cex=0.8, col=c("black","red", "green"), lty=c(1, 4, 3), lwd=2, bg="light grey")

----------------------------------------------------------------------------------
# PLOT N-1S DATA

# Load workspace: Single N no Cs (caña-ardilla, elotillo-alto, puño-camisa, rodilla-semilla, milpa, gallina, mango, pegar, chapulin, N+1s) 2013-04-30.RData

# Get data for each speaker
aqq.1s = n1sf0.noCs.males[n1sf0.noCs.males$sp=="AQQ",] # Andres
fqm.1s = n1sf0.noCs.males[n1sf0.noCs.males$sp=="FQM",] # Chiku
imq.1s = n1sf0.noCs.males[n1sf0.noCs.males$sp=="IMQ",] # Ines
mmv.1s = n1sf0.noCs.males[n1sf0.noCs.males$sp=="MMV",] # Marka
mqm.1s = n1sf0.noCs.males[n1sf0.noCs.males$sp=="MQM",] # Miguel

# Set parameters for plotting
item = "mi_elotillo"
dat = aqq.1s[aqq.1s$g==item,]
dat = fqm.1s[fqm.1s$g==item,]
dat = imq.1s[imq.1s$g==item,]
dat = mmv.1s[mmv.1s$g==item,]
dat = mqm.1s[mqm.1s$g==item,]

plot.items = vector(length=nrow(dat))
plot(1:40,seq(100,220,length.out=40), xlim = c(1,40), ylim = c(100,240), main=c(unique(dat$sp),unique(dat$g)), col=colours[i], type = "n", xlab = "Normalized Time", ylab = "F0")

for (i in 1:nrow(dat)) { 
	lines(1:40, dat[i, 2:41], ylim = c(100,220), col=colours[i], type = "b", pch=i, xlab = "Normalized Time", ylab = "F0")
	plot.items[i] = as.character(dat[i,1])
	}
legend("bottomleft", plot.items, cex=0.8, col=colours[1:nrow(dat)], pch=1:nrow(dat), lwd=2, bg="light grey")


----------------------------------------------------------------------------------
# HOW TO PLOT DATA WITH VOICELESS Cs (NO F0) SO THE PLOT HAS BLANK SPACE FOR THEM

# Create a matrix of "NA" with as many columns as F0 samples for one segment and as many rows as there are tokens
C1 = matrix("NA", nrow=54, ncol=10)
y = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "Y10")
C1 = data.frame(C1)
colnames(C1) = y
C2 = matrix("NA", nrow=54, ncol=10)
w = c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10")
C2 = data.frame(C2)
colnames(C2) = w

# Insert the one-segment dataframes of all "NA" where the voice less segments are and the f0 values from the vowels in where the vowels should go
rodilla.dat = cbind(rodillaf0[,1], c, rodillaf0[,2:11], b, rodillaf0[,12:21])
semilla.dat = cbind(semillaf0[,1], c, semillaf0[,2:11], b, semillaf0[,12:21])

# Plot rodilla.dat
plot(1:40, as.numeric(sapply(as.list(dat[2:length(dat)]),mean)), type="l", lwd=3, main="Average f0 Normalized Time", xlab="Normalized Time", ylab="Frequency (Hz)", ylim=c(100,250))

# Plot semilla.dat over top
lines(1:40, as.numeric(sapply(as.list(dat2[2:length(dat2)]),mean)), type="l", lwd=3, main="Average f0 Normalized Time", xlab="Normalized Time", ylab="Frequency (Hz)", col="red", ylim=c(100, 250))

abline(v=c(1,10,20,30,40))
legend("bottomright", c("HHb ('rodilla')", "HHa ('semilla')"), cex=0.8, col=c("black","red"), lty=c(1,1), lwd=3, title="Plots", bg="light grey")

---------------------------------------------------------------------------------
# Plotting token <<filename>actualtimenormf0 files:
plot(dat[,2], dat[,3], type="l", lwd=3)
abline(v=(c(dat[10,2], dat[20,2], dat[30,2], dat[40,2]))) # put vertical lines at segment boundaries

---------------------------------------------------------------------------------
# Plot all f0 contours for a wordlist item by actual f0times instead of normalized (equal for each segment) f0times:

# Load a library for making tick marks on the plot
library(Hmisc)

# 1) Read in the normf0.txt data (10 f0 readings for each segment
normf0 = read.table(file.choose(), header=T, sep="\t", encoding="UTF-8")

# 2) Read in the times for the readings from normactualtime.txt
f0times = read.table(file.choose(), header=T, sep="\t", encoding="UTF-8")

# 3) Read in the meanf0.txt file which has the segment labels
meanf0 = read.table(file.choose(), header=T, sep="\t", encoding="UTF-8")

# 3) Plot f0 at actual f0times of an individual (the first) token
plot(as.numeric(f0times[1,2:ncol(f0times)]), as.numeric(normf0[1,2:ncol(normf0)]), type="l", lwd=3) # Get x value from f0times and y value (f0) from normf0
abline(v=(c(f0times[1,2], f0times[1,11], f0times[1,21], f0times[1,31], f0times[1,41]))) # put vertical lines at segment boundaries

# 4) Plot f0 at actual f0times for all tokens on same plot
minf0 <- as.numeric(readline("Min y-axis value (f0)? "))
maxf0 <- as.numeric(readline("Max y-axis value (f0)? "))
xrange = c(0, max(f0times[,length(f0times)]))
yrange = c(minf0, maxf0)
colors <- rainbow(nrow(normf0))
linetype <- 1:nrow(normf0)
plotchar <- seq(0, 0+nrow(normf0), 1)
labels <- as.character(normf0[,1])

plot(xrange, yrange, type="n", main="f0 Actual f0times", xlab="f0times", ylab="Frequency (Hz)", xlim=xrange, ylim=yrange)

for (i in 1:nrow(normf0))  {
	tokentime <- as.numeric(f0times[i,2:length(f0times)])
	tokenf0 <- as.numeric(normf0[i,2:length(normf0)])
	lines(tokentime, tokenf0, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i])
}
legend("topright", labels, cex=0.8, col=colors, pch=plotchar, lty=linetype, bg="light grey", title="Plots")

# 5) Plot average f0 and average f0times over all tokens
avgtime = as.numeric(sapply(as.list(f0times[,2:length(f0times)]),mean))
avgf0 = as.numeric(sapply(as.list(normf0[,2:length(f0times)]),mean))
plot(avgtime, avgf0, type="l", lwd=3, main="Average f0 Actual f0times", xlab="f0times", ylab="Frequency (Hz)", xlim=xrange, ylim=yrange) # Get x value from f0times and y value (f0) from normf0
minor.tick(ny=4, tick.ratio=par("tck")/2)
abline(v=(c(avgtime[1], avgtime[10], avgtime[20], avgtime[30], avgtime[40]))) # put vertical lines at segment boundaries
labels = colnames(meanf0[2:ncol(meanf0)])  # Get labels from the column names of meanf0
labels[length(labels)] = substr(labels[length(labels)], 1, 1) # just take the first character of the last label because if both Vs in the word are the same, the 2nd V (the last label) will have a ".1" appended to it
labelx = c((avgtime[5]+avgtime[6])/2, (avgtime[15]+avgtime[16])/2, (avgtime[25]+avgtime[26])/2, (avgtime[35]+avgtime[36])/2)
labely = rep(maxf0-10, 4)
par(family="ArialUnicodeMS")
text(labelx, labely, labels, cex=2)
# dev.off() # To turn off the font family parameter change

--------------------------------------------------------------------------------------
# ADDING ADDITIONAL MELODIES TO AN EXISTING PLOT
  
# USING SPECIAL PLOT CHARACTERS AND COLOURS:
# Add HHa in blue diamonds
  lines(sapply(as.list(HHaf0[,2:length(HHaf0)]),mean), col="blue", lwd=2, type="b", pch=18)

# Add HHb in cornflowerblue solid circles
lines(sapply(as.list(HHbf0[,2:length(HHbf0)]),mean), col="cornflowerblue", lwd=2, type="b", pch=19)

# Add LM in red upside down greyed filled triangles
lines(sapply(as.list(LMf0[,2:length(LMf0)]),mean), col="red", lwd=2, type="b", pch=25)

# Add LH in blueviolet solid squares
lines(sapply(as.list(LHf0[,2:length(LHf0)]),mean), col="blueviolet", lwd=2, type="b", pch=15)

# Add HLa in green solid triangles
lines(sapply(as.list(HLaf0[,2:length(HLaf0)]),mean), col="green", lwd=2, type="b", pch=17)

# Add HLb in tan4 asterisks
lines(sapply(as.list(HLbf0[,2:length(HLbf0)]),mean), col="tan4", lwd=2, type="b", pch=8)
  
########## START HERE TO ADD THE OTHER MELODY PLOTS ############
#
# USING LINE STYLES AND COLOURS:
# Add HHa in blue long dash short dash line
lines(sapply(as.list(HHaf0[,2:length(HHaf0)]),mean), col="blue", lwd=3, lty=6)

# Add HHb in cornflowerblue dotted line
lines(sapply(as.list(HHbf0[,2:length(HHbf0)]),mean), col="cornflowerblue", lwd=3, lty=3)

# Add LM in red dash dot line
lines(sapply(as.list(LMf0[,2:length(LMf0)]),mean), col="red", lwd=3, lty=4)

# Add LH in blueviolet long dash line
lines(sapply(as.list(LHf0[,2:length(LHf0)]),mean), col="blueviolet", lwd=3, lty=5)

# Add HLa in green short dash line
lines(sapply(as.list(HLaf0[,2:length(HLaf0)]),mean), col="green", lwd=3, lty=2)

# Add HLb in tan4 short dash line
lines(sapply(as.list(HLbf0[,2:length(HLbf0)]),mean), col="tan4", lwd=3, lty=2)

# Add a legend
labels = c("HHa", "HHb", "LM", "LH", "HLa", "HLb", "ML")
linetype = c(6,3,4,5,2, 2, 1)
colours = c("blue", "cornflowerblue", "red", "blueviolet", "green", "tan4", "black")
legend("topleft", labels, cex=0.8, col=colours, lty=linetype, bg="light grey", title="Melodies")

# Put the legend outside the plot at the topright
# But you have to change the the margins like this before plotting to leave room at the right edge for the legend. xpd=TRUE will make ablines run off plot so make sure to do par(xpd=FALSE) before doing ablines:
# par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
par(xpd=T)
legend("topright", inset=c(-0.2,0.038), legend=labels, title="Melodies", col=colours, lty=linetype, lwd=3)
par(xpd=F)

-----------------------------------------------------------------------------------
  # PLOT A 3 SEGMENT (VCV) WORD CONTOUR OVER A 4 SEGMENT (CVCV) WORD CONTOUR 
  #   starting at the beginning of the second segment (i.e. skipping over the 
  #   missing initial C)
  
  # Step 1: plot the average over all segments in normalized time using f0plot(type=2)
  
  # Step 2: read in the normf0.txt for the VCV word
  dat = read.table(file.choose(), header=T, sep="\t", encoding="UTF-8")

# Step 3: compute the average of all tokens and store as a numeric vector
f0 = as.numeric(sapply(as.list(dat[,2:length(dat)]),mean))

# Step 4: create a vector for the x-axis (normalized time--assumes 10 plots per segment), and increase each value by 10 to skip the missing initial C
normtime = (1:length(f0))+10

#Step 5: add the line to the existing CVCV plot
lines(normtime, f0, type="l", lwd=1.5, col="red")

# OR...
lines(normtime, f0, type="b", lwd=2, col="magenta", pch=15)

# OR...
lines(normtime, f0, lwd=3, col="magenta", lty=5)
