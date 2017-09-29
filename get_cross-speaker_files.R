################################################################################################
# Using R to get cross-speaker ("ensemble") output files from ProsodyPro 5.6.1
#
# This R script imports cross-speaker averages of the aggregate ("ensemble") calculation output 
# files from ProsodyPro (http://www.homepages.ucl.ac.uk/~uclyyix/ProsodyPro/) Praat script 
# developed by Yi Xu.
# 
# BACKGROUND -- MAKE SURE YOU'VE DONE THIS BEFORE RUNNING THE SCRIPT!
# * RUN ProsodyPro to output individual ensemble files -- "Get ensemble files" 
# * RUN ProsodyPro to get files with cross-speaker averages
# * This will produce four .txt files: 
#   1. mean_normactutime_cross_speaker.txt
#   2. mean_normf0_cross_speaker.txt
#   3. mean_normtime_f0velocity_cross_speaker.txt
#   4. mean_normtime_semitonef0_cross_speaker.txt
# * PLACE these files in a separate folder by themselves
# * ASSIGN the full path of this folder to the `sp.dir` variable in the script below.
# 
# The script will...
# 1. Create a list of dataframes (`dataset.list`)--one dataframe for each text file 
# 2. Read in all the .txt files
#    rows with unequal number of columns (data points) are correctly handled even 
#    if the first row has less than the max number of columns; missing values in short rows are 
#    assigned 'NA' at the END of the row
# 3. Adds a column for the speaker (= "All_speakers") to each dataframe
# 4. Append each type of file to the appropriate dataframe in the list (i.e. the 
#     mean_normf0_cross_speaker.txt file gets stored in the list item `dataset.list$mean_normf0`, etc.)
# 
# The scrip will NOT add colums for gloss, pronoun, mixtec, ipa & melody. If you want these columns,
# which are needed for making f0 plot, see "N-Pron data processing.Rmd".
#
################################################################################################

# save the current working directory
cur_dir <- getwd()

# ***THIS MIGHT NEED TO BE CHANGED*** set the path where the speaker folders are
sp.dir <- "/Users/Kevin/Dropbox/_exp_data/NPron/To_process"

# set working directory
setwd(sp.dir)

# initialize the speaker files data.frame (this is just for tracking the files processed)
all.sp.files <- data.frame(num=character(), sp=character(), files=character(), stringsAsFactors = F)

list.names <- c("mean_normactutime", "mean_normf0", "mean_normtime_f0velocity", "mean_normtime_semitonef0")

# initialize the list to store the "ensemble" files of all types
dataset.list <- vector("list", length=length(list.names))
names(dataset.list) <- list.names

# get the list of all text files in the directory
files <- list.files(path=sp.dir, pattern="*.txt", full.names=T, recursive=FALSE)

# append the filenames in this speaker's folder to the dataframe
all.sp.files = rbind(all.sp.files, cbind(rep(basename(sp.dir), length(files)), files))

for (j in 1:length(files)) { # loop through files in ea. speaker folder
  
  # get the bare filename minus the extension--this is the name of the list item it will be stored in
  fn <- gsub("(.*)(\\.txt)","\\1", basename(files[j]))
  fn <- gsub("(.*)(_cross_speaker)","\\1",fn)
  
  # read the file into the appropriate list item; 
  num.col <- max(count.fields(files[j], sep = "\t")) # Find max # of columns in the file
  cat("Max # columnes in",fn," = ",num.col,"\n") # Report
  tmp <- read.csv(files[j], header=T, col.names=1:num.col, sep="\t", encoding="UTF-8", fill=T, as.is = T)
  # Make column names for the max # of columns
  # Fill empty cells with NA
  # Strings read "as.is", not converted to factors
  
  # add a column for the speaker so we know who's folder the data came from
  tmp$sp <- rep("All_speakers", nrow(tmp))
  
  # append the new dataframe to the appropriate one in dataset.list
  rbind(dataset.list[[fn]], tmp) -> dataset.list[[fn]]
  
} # loop through files in each speaker folder

# reset the working directory to what it was
setwd(cur_dir)
