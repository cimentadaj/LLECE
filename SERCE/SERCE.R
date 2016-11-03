library(tidyverse)
library(downloader)
library(haven)

# SET WORKING DIRECTORY HERE WHERE FILES WILL BE (IN CASE THEY MUST BE DOWNLOADED)
# AND WHERE FILES ARE (IN CASE NO DOWNLOAD IS NECESSARY)
setwd("/Users/cimentadaj/Downloads/serce")

funs <- c("https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/downloader_file.R",
          "https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/unrar.R")
# The unrar functions relies heavily on have UnRAR installed in your OS.
# So have a look at the link above. This script has been only tested in mac OSx.

for(i in funs) source_url(i, sha = sha_url(i))

downloader_file("bcf362e6.zip",
                "http://www.unesco.org/new/fileadmin/MULTIMEDIA/FIELD/Santiago/zip/",
                "SERCE",
                getwd())

data_dir <- paste0(getwd(), "/SERCE/")

# Is the unrared file nam in the folder?
unrar_name <- grepl("Bases pa", list.files(data_dir))

# If the files is not there, then unrar the file.
# After that, for all folders inside the new path
# unrar everything inside those folders.
if (!(any(unrar_name))) {
  # Exact path to the rar files
  rar_path <- paste0(data_dir, grep("SERCE.rar", list.files(data_dir), value = T))
  
  # Unrar it and extract it in the same folder
  unrar(rar_path, data_dir)

  # New path for the unrared folder  
  new_path <- paste0(data_dir, grep("Bases pa", list.files(data_dir), value = T), "/")

  # All files within that folder
  fls_path <- list.files(new_path)
  # Obtain only the file names without a '.' in their names, so only the folders
  folder_names <- fls_path[!grepl("\\.", fls_path)]
  
  # Create a path for each of these new folders
  folder_path <- paste0(new_path, folder_names, "/")
  
  # This function lists all the .rar files within the argument 
  # 'folder' and unrars them in the 'exdir' argument. The password
  # argument should be used when the rar files have the same password
  extracter <- function(folder, exdir, password = NULL) {
    
    # Remember to end both paths with /
    rar_files <- list.files(folder, "*.rar")
    for (i in rar_files) unrar(paste0(folder, i), exdir, password)
  }
  
  # Loop through each folder path and extract all rar files within each path
  for (i in folder_path) extracter(i, i, "serce2007")
}

# if the first file was unrared manually, the program assumes that
# all other files within that file have been unrared as well.

all_data <- vector("list", )
read_spss()

