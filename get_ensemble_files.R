# Using R to get aggregate ("ensemble") output files from ProsodyPro 6.0 beta
#
# This R script imports aggregate ("ensemble") calculation output files from the ProsodyPro 
# (http://www.homepages.ucl.ac.uk/~uclyyix/ProsodyPro/) Praat script developed by Yi Xu.
# 
# Background
# * The ProsodyPro 6.0 beta2 script must be run with task #4 "Get ensemble files" checked
# * All ProsodyPro aggregate ("ensemble") output files are .txt files (e.g. mean_normf0.txt, maxf0.txt, minf0.txt, etc.)
# * Each speaker's files must be in a separate folder (directory) named with their initials (which will be added
#   to the dataframes created as the sp(eaker) variable
# * All speaker folders (directories) must be in the same enclosing folder (directory)
# 
# The script will...
# 1. Create a list of dataframes (dataset.list)--one dataframe for each type of "ensemble" file 
# (e.g. mean_normf0, maxf0, minf0, etc.)
# 2. Get the list of speaker folders (directories) from the directory assigned to sp.dir
# 3. Get the list of .txt files in each speakers folder
# 4. Read in the files for a speaker and store in a temp data.frame (missing values are 
#    automatically assigned 'NA')
# 5. Append each type of file to the appropriate dataframe in the list (i.e. all mean_f0 files get 
#    stored in the list item dataset.list$mean_f0, etc.)
#
# See "Using R to Get ProsodyPro Ensemble Output.Rmd" for a more in depth explanation of the script

#_________________FUNCTIONS_________________



#_________________BEGIN SCRIPT_________________

# save the current working directory
cur_dir <- getwd()

# set the path where the speaker folders are
sp.dir <- "/Users/Kevin/Fieldwork-vmj/_exp_data/NPron"

# set working directory
setwd(sp.dir)

# get the list of folders (directories) in the working directory
dir_list <- list.dirs(full.names=T, recursive=F)

# initialize the speaker files data.frame (this is just for tracking the files processed)
all.sp.files <- data.frame(sp=character(), files=character(), stringsAsFactors = F)

# make list of variables into which each "ensemble" data file type will be read
list.names <- c("duration", "excursionsize", "finalf0", "finalvelocity", "maxf0", "maxvelocity", "mean_duration", "mean_excursionsize", "mean_finalf0", "mean_finalvelocity", "mean_maxf0", "mean_maxvelocity", "mean_meanf0", "mean_meanintensity", "mean_minf0", "mean_normactutime", "mean_normf0", "mean_normtime_f0velocity", "mean_normtime_semitonef0", "mean_normtimeIntensity", "meanf0", "meanintensity", "minf0", "normactutime", "normf0", "normtime_f0velocity", "normtime_semitonef0", "normtimeIntensity")

# initialize the list to store the "ensemble" files of all types
dataset.list <- vector("list", length=length(list.names))
names(dataset.list) <- list.names

for (i in 1:length(dir_list)) { # loop through speaker folders
  
  # get the list of all text files in the directory
  files <- list.files(path=dir_list[i], pattern="*.txt", full.names=T, recursive=FALSE)
 
  # remove files with uneven numbers of observations per row (not needed, & can't be put in data.frame)
  files <- files[-grep("/f0velocity.txt", files)] # remove f0velocity.txt from the file list
  files <- files[-grep("/samplef0.txt", files)] # remove samplef0.txt from the file list
  
  # append the filenames in this speaker's folder to the dataframe
  all.sp.files = rbind(all.sp.files, cbind(rep(basename(dir_list[i]), length(files)), files))
  
  for (j in 1:length(files)) { # loop through files in ea. speaker folder
    
    # get the bare filename minus the extension--this is the name of the list item it will be stored in
    fn <- gsub("(.*)(\\.txt)","\\1", basename(files[j]))
    
    # read the file into the appropriate list item; strings read "as.is", not converted to factors
    tmp <- read.csv(files[j], header=T, sep="\t", encoding="UTF-8", as.is = T)
    
    # add a column for the speaker so we know who's folder the data came from
    tmp$sp <- rep(basename(dir_list[i]), nrow(tmp))
    
    # append the new dataframe to the appropriate one in dataset.list
    rbind(dataset.list[[fn]], tmp) -> dataset.list[[fn]]
    
  } # loop through files in each speaker folder
} # loop through speaker folders

# reset the working directory to what it was
setwd(cur_dir)

