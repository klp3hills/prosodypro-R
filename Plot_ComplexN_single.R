#####################################################
# PLOT COMPLEX N SINGLE ITEM
#
# This plots a single item from one speaker from the Complex N tone production experiment
# (May 28-29, 2015).
# 
# Assumptions:
# * The dataset has been imported to R using the script ```get_ensemble_files.R``` and is stored
#   in the list ```dataset.list```
# * Before plotting, the data to be plotted is stored in the dataframe ```dat``` as described in
#   "Complex N data processing.Rmd"
#
# Other plotting scripts are offered (see "Complex N data processing.Rmd"). This script can
# **SINGLE WORDLIST ITEM** FROM A SINGLE SPEAKER TO THE SCREEN
# **ALL WORDLIST ITEMS** FROM A SINGLE SPEAKER TO PDF
# 
# TO MAKE PLOTS WITH THIS SCRIPT:
# 1. Excecute the code in the first section, "DO THIS FIRST BEFORE ATTEMPTING ANY OF THE PLOTS!"
# 2. Jump to the section for the kind of plots you want to make and execute the code:
#    a. **SINGLE WORDLIST ITEM** FROM A SINGLE SPEAKER TO THE SCREEN
#    b. **ALL WORDLIST ITEMS** FROM A SINGLE SPEAKER TO PDF
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
# **SINGLE WORDLIST ITEM** FROM A SINGLE SPEAKER TO THE SCREEN
#####################################################

#-----------------------------------------------------------
# GENERAL SETUP
#-----------------------------------------------------------

# CHOOSE the Y-AXIS range
yrange = c(100,220)

# CHOOSE the SPEAKER
# View the list speakers in the data if you want to
speakers <- unique(dat$sp)

# CHOOSE the speaker:
speaker <- "MQM"

# CHOOSE which word to plot by it's Spanish gloss, which it will look up in dat$noun:
# Wordlist glosses for reference: unique(dat$noun):
# [1] "arbol_de_tololote" "garza"             "frutales"          "baras_de_otate"    "caballete"        
# [6] "jitomate"          "polilla"           "hombro"            "Oaxaca"            "bolita"           
# [11] "coco"              "esposa"            "baño"              "niños"             "gemelos"          
# [16] "sufrimiento"       "tu_axila"          "dueño"             "zopilote"          "rifle"            
# [21] "sandia"            "varita"            "naranja"           "costal"            "hombre"           
# [26] "zorro"             "piedra_de_filar"   "chicharra"         "mar"               "refresco"         
# [31] "tequio"            "animal"            "Las_Trojes"        "arbol_de_limon"    "arbol_de_guava"   
# [36] "estrella"          "lluvia_con_viento" "ceñidor" 

item <- "ceñidor"

# Extract the data
dat1 <- dat[dat$sp==speaker & dat$noun==item,]

#-----------------------------------------------------------
# SPECIFIC SETUP (all root structures)
#-----------------------------------------------------------

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
}
  
#-----------------------------------------------------------
# BEGIN PLOT

plot(1:length(f0), f0, 
     ylim=yrange, 
     type="l", 
     lwd=2, 
     ylab="F0 (Herz)",
     xlab="Normalized Time", 
     main=paste0(dat[dat$noun==item,]$sp,": ",dat[dat$noun==item,]$ipa," '",item,"'"))

# END PLOT
#-----------------------------------------------------------

#####################################################
# **ALL WORDLIST ITEMS** FROM A SINGLE SPEAKER TO PDF
#####################################################

# This plots each wordlist item for a single speaker to a separate page in a PDF file. It uses 
# the Cairo library so it can plot UTF-8 characters (e.g. in vernacular labels).

#-----------------------------------------------------------
# GENERAL SETUP
#-----------------------------------------------------------

# CHOOSE the Y-AXIS range
yrange = c(100,220)

# CHOOSE the SPEAKER
# View the list speakers in the data if you want to
speakers <- unique(dat$sp)

# CHOOSE the speaker:
speaker <- "MQM"

# SPECIFY the experiment context: "isolation", "L_L", "H_H"
#context <- "isolation"
context <- "H_H"


# Extract the speaker data
sp.dat <- dat[dat$sp==speaker,]


#-----------------------------------------------------------
# PLOT SETUP
#-----------------------------------------------------------

# Turn on Cairo pdf device if there are special characters anywhere in plot
library(Cairo)
Cairo(file=paste0("ComplexN-",speaker,"-",context,".pdf"),
      type="pdf",
      family="ArialUnicodeMS", 
      width=7, 
      height=5, 
      units="in")

# set up the plot area
par(mar=c(5, 4, 4, 2) + 0.1)

#-----------------------------------------------------------
# MAKE PLOTS
#-----------------------------------------------------------

# for loop through wordlist items for the speaker
for (j in 1:nrow(sp.dat)) {
  
  # Extract the data
  dat1 <- sp.dat[j,]
  
  # Set up an f0 vector with NAs for onsets of each syllable and f0 data for the TBU
  #   taking into account that some items have NAs filling the last 10 columns because
  #   they were shorter than the longest items in the experiment
  f0 <- vector(length=0) # Initialize f0 vector
  
  # Get the # of numeric columns in current row with numeric data, i.e. those that aren't "NA" 
  # (not including non-numeric columns)
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
  plot(1:length(f0), f0, 
       ylim=yrange, 
       type="l", 
       lwd=2, 
       ylab="F0 (Herz)",
       xlab="Normalized Time", 
       main=paste0(speaker,": ",sp.dat[j,]$ipa," '",sp.dat[j,]$noun,"'"),
       sub="")

  print(paste0(speaker,", item ",j,": ", sp.dat[j,]$noun))
  
  } # for loop through wordlist

# reset plotting parameters
dev.off()


