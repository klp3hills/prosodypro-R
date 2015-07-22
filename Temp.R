list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE, full.names=FALSE, ignore.case=FALSE) { 
  # Use this function to get a list of folders (directories) in
  # a specified folder (directory)
  # use full.names=TRUE to pass to file.info 
  all <- list.files(path, pattern, all.dirs, full.names=TRUE, recursive=FALSE, ignore.case) 
  dirs <- all[file.info(all)$isdir] # determine whether to return full names or just dir names 
  if(isTRUE(full.names)) 
    return(dirs) 
  else 
    return(basename(dirs)) 
}


/Users/Kevin/Fieldwork-vmj/_exp_data/NPron

NPron.list[names(NPron.list)==fn]