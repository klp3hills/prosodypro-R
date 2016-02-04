# N-Pron data processing
#
# ---PLOTTING---

# CHOOSE the item to plot
item <- "_semilla_de_mi_"

# SET the y-axis range
yrange = c(100,220)

# Create NAs to be inserted in plots for Cs (onsets)
x = rep(NA, 10)

# Initialize f0 vector
f0 = vector(length=0)

# set up an f0 vector with NAs for onsets of each syllable
for (i in seq(1,ncol(dat)-10,10)) { # for loop through syllables
f0 <- as.vector(c(f0, x, dat[dat$Normtime==item,seq(i+1,i+10,1)]))
} # for loop through syllables

# BEGIN PLOT
plot(1:length(f0),f0, ylim=yrange)

