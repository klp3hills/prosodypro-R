# Using R to get aggregate ("ensemble") output files from ProsodyPro 6.0 beta
#
# This R script imports aggregate ("ensemble") calculation output files from the ProsodyPro 
# (http://www.homepages.ucl.ac.uk/~uclyyix/ProsodyPro/) Praat script developed by Yi Xu.
# 
# BACKGROUND -- MAKE SURE YOU'VE DONE THIS BEFORE RUNNING THE SCRIPT!
# * RUN ProsodyPro 6.0 beta2 script with the "Keep files together" and task #4 
#   "Get ensemble files" boxes checked
# * All ProsodyPro "ensemble" output files are .txt files (e.g. mean_normf0.txt, maxf0.txt, minf0.txt, etc.)
# * Each speaker's files MUST be in a separate folder (directory) named with their 3 initials 
#   (which will be added to the dataframes created as the sp(eaker) variable
# * All speaker folders (directories) MUST be in the same enclosing folder (directory) 
#   ASSIGN the full path must to this folder to to sp.dir in the script below.
# 
# The script will...
# 1. Create a list of dataframes (dataset.list)--one dataframe for each type of "ensemble" file 
# (e.g. mean_normf0, maxf0, minf0, etc.)
# 2. Get the list of speaker folders (directories) from the directory assigned to sp.dir
# 3. Get the list of .txt files in each speakers folder
# 4. Read in all the .txt files for a speaker and store in a temp data.frame 
#    New 2016-02-26: rows with unequal number of columns (data points) are correctly handled even 
#    if the first row has less than the max number of columns; missing values in short rows are 
#    assigned 'NA' at the END of the row
# 5. Append each type of file to the appropriate dataframe in the list (i.e. all mean_f0 files get 
#    stored in the list item dataset.list$mean_f0, etc.)
# 6. Saves a file listing all the ProsodyPro files processed to the directory assigned to sp.dir
#
# See "Using R to Get ProsodyPro Ensemble Output.Rmd" for a more in depth explanation of the script

#_________________FUNCTIONS_________________

# none!

#_________________BEGIN SCRIPT_________________

# save the current working directory
cur_dir <- getwd()

# ***THIS MIGHT NEED TO BE CHANGED*** set the path where the speaker folders are
#sp.dir <- "/Users/Kevin/Fieldwork-vmj/_exp_data/NPron/Processed"
#sp.dir <- "/Users/Kevin/Fieldwork-vmj/_exp_data/ComplexN/CompN-isolation"
#sp.dir <- "/Users/Kevin/Fieldwork-vmj/_exp_data/ComplexN/CompN-H_H"
sp.dir <- "/Users/Kevin/Fieldwork-vmj/_exp_data/ComplexN/CompN-L_L"

# set working directory
setwd(sp.dir)

# get the list of folders (directories) in the working directory
dir_list <- list.dirs(full.names=T, recursive=F)

# initialize the speaker files data.frame (this is just for tracking the files processed)
all.sp.files <- data.frame(num=character(), sp=character(), files=character(), stringsAsFactors = F)

# make list of variables into which each "ensemble" data file type will be read
list.names <- c("duration", "excursionsize", "finalf0", "finalvelocity", "maxf0", "maxvelocity", "mean_duration", "mean_excursionsize", "mean_finalf0", "mean_finalvelocity", "mean_maxf0", "mean_maxvelocity", "mean_meanf0", "mean_meanintensity", "mean_minf0", "mean_normactutime", "mean_normf0", "mean_normtime_f0velocity", "mean_normtime_semitonef0", "mean_normtimeIntensity", "meanf0", "meanintensity", "minf0", "normactutime", "normf0", "normtime_f0velocity", "normtime_semitonef0", "normtimeIntensity")

# initialize the list to store the "ensemble" files of all types
dataset.list <- vector("list", length=length(list.names))
names(dataset.list) <- list.names

for (i in 1:length(dir_list)) { # loop through speaker folders
  
  # get the list of all text files in the directory
  files <- list.files(path=dir_list[i], pattern="*.txt", full.names=T, recursive=FALSE)
 
  # remove the files with uneven numbers of observations per row (can't be put in a dataframe & aren't needed anyway)
  files <- files[-grep("/f0velocity.txt", files)] # remove f0velocity.txt from the file list
  files <- files[-grep("/samplef0.txt", files)] # remove samplef0.txt from the file list
  
  # append the filenames in this speaker's folder to the dataframe
  all.sp.files = rbind(all.sp.files, cbind(rep(basename(dir_list[i]), length(files)), files))
  
  for (j in 1:length(files)) { # loop through files in ea. speaker folder
    
    # get the bare filename minus the extension--this is the name of the list item it will be stored in
    fn <- gsub("(.*)(\\.txt)","\\1", basename(files[j]))
    
    # read the file into the appropriate list item; 
    num.col <- max(count.fields(files[j], sep = "\t")) # Find max # of columns in the file
    cat("Max # columnes in",fn," = ",num.col,"\n") # Report
    tmp <- read.csv(files[j], header=T, col.names=1:num.col, sep="\t", encoding="UTF-8", fill=T, as.is = T)
                    # Make column names for the max # of columns
                    # Fill empty cells with NA
                    # Strings read "as.is", not converted to factors
    
    # add a column for the speaker so we know who's folder the data came from
    tmp$sp <- rep(basename(dir_list[i]), nrow(tmp))
    
    # append the new dataframe to the appropriate one in dataset.list
    rbind(dataset.list[[fn]], tmp) -> dataset.list[[fn]]
    
  } # loop through files in each speaker folder
} # loop through speaker folders

# write the paths of all speaker files processed to a text file in the folder enclosing the speaker folders
write.table(all.sp.files, file = paste0(sp.dir, "/ProsodyPro_files_processed.txt"), sep = "\t", quote = F, fileEncoding = "UTF-8")

# reset the working directory to what it was
setwd(cur_dir)

