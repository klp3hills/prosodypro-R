# N-Pron data processing
#
# ***********************************************************************************************
# To process N-Pron experiment data BEGIN with the instructions in "N-Pron_data_processing.html".
# ***********************************************************************************************
# 
# -------ADDING MIXTEC & MELODY TO DATASET----------
# Read in list of nouns with melodies
npron.melody.list <- read.table(file.choose(), header=T, sep="\t", encoding="UTF-8") # N-Pron noun melody list.csv
npron.melody.list
str(npron.melody.list)

# Convert gloss, mixtec and melody to character strings
for (i in 2:5) {
  npron.melody.list[,i] <- as.character(npron.melody.list[,i])
}

# Look up the noun, assign it the Mixtec & Melody that corresponds to the noun in npron.melody.list

# Copy dat$noun to dat$mix, dat$mel and dat$ipa
dat$mix <- dat$noun
dat$mel <- dat$noun
dat$ipa <- dat$noun

# Substitue the proper Mixtec, melody and ipa from npron.melody.list for each noun
for (i in 1:nrow(npron.melody.list)) { 
  dat[dat$mix==npron.melody.list[i,]$g,]$mix <- npron.melody.list[i,]$mix
  dat[dat$mel==npron.melody.list[i,]$g,]$mel <- npron.melody.list[i,]$mel
  dat[dat$ipa==npron.melody.list[i,]$g,]$ipa <- npron.melody.list[i,]$ipa
}

# Check the melodies & glosses
names(dat)
dat[,c(1,32:37)]
