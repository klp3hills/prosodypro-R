# Using R to get aggregate ("ensemble") output files from ProsodyPro 6.0 beta

#_________________FUNCTIONS_________________



#_________________BEGIN SCRIPT_________________

# save the current working directory
cur_dir = getwd()

# get the path where the speaker folders are
#sp.dir <- readline(prompt = "Enter the pathway to the folder containing the speaker data folders: ")
sp.dir <- "/Users/Kevin/Fieldwork-vmj/_exp_data/NPron"

# set working directory
setwd(sp.dir)

# get the list of folders (directories) in the working directory
dir_list <- list.dirs(full.names=T, recursive=F)

# initialize the speaker files data.frame (this is just for tracking)
all.sp.files <- data.frame(sp=character(), files=character(), stringsAsFactors = F)

# make list of ensemble files into which each data file type will be read
list.names <- c("duration", "excursionsize", "finalf0", "finalvelocity", "maxf0", "maxvelocity", "mean_duration", "mean_excursionsize", "mean_finalf0", "mean_finalvelocity", "mean_maxf0", "mean_maxvelocity", "mean_meanf0", "mean_meanintensity", "mean_minf0", "mean_normactutime", "mean_normf0", "mean_normtime_f0velocity", "mean_normtime_semitonef0", "mean_normtimeIntensity", "meanf0", "meanintensity", "minf0", "normactutime", "normf0", "normtime_f0velocity", "normtime_semitonef0", "normtimeIntensity")

# initialize the list to store the ensemble files of all types
dataset.list <- vector("list", length=length(list.names))
names(dataset.list) <- list.names

for (i in 1:length(dir_list)) { # loop through speaker folders
  
  # get the list of all text files in the directory
  files <- list.files(path=dir_list[i], pattern="*.txt", full.names=T, recursive=FALSE)
 
  # remove files with uneven numbers of observation per row
  files <- files[-grep("/f0velocity.txt", files)]
  files <- files[-grep("/samplef0.txt", files)]
  
  # append the files to the dataframe
  all.sp.files = rbind(all.sp.files, cbind(rep(basename(dir_list[i]), length(files)), files))
  
  for (j in 1:length(files)) { # loop through files in ea. speaker folder
    
    # get the bare filename minus the extension
    fn = gsub("(.*)(\\.txt)","\\1", basename(files[j]))
    
    # read the file into the appropriate list item; strings read "as.is", not converted to factors
    tmp <- read.csv(files[j], header=T, sep="\t", encoding="UTF-8", as.is = T)
    
    # add a column for the speaker so we know who's folder the data came from
    tmp$sp <- rep(basename(dir_list[i]), nrow(tmp))
    
    rbind(dataset.list[[fn]], tmp) -> dataset.list[[fn]]
    
  } # loop through files in ea. speaker folder
} # loop through speaker folders


