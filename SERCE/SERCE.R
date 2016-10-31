library(tidyverse)
library(downloader)


# SET WORKING DIRECTORY HERE WHERE FILES WILL BE (IN CASE THEY MUST BE DOWNLOADED)
# AND WHERE FILES ARE (IN CASE NO DOWNLOAD IS NECESSARY)

setwd("/Users/cimentadaj/Downloads/serce")

# If the file hasn't been unzipped, then download and unzip it.
if (!("SERCE" %in% list.files(getwd()))) {
  # Names of zip file and download path
  zip_file <- "bcf362e6.zip"
  download_url <- "http://www.unesco.org/new/fileadmin/MULTIMEDIA/FIELD/Santiago/zip/"
  
  # Create temporary directory and temporary file path
  temp <- tempdir()
  dir_file <- paste0(temp, "/", zip_file)

  # Download file and save in temporary file
  download(paste0(download_url, zip_file), dir_file)
  
  # unzip in working directory
  unzip(dir_file)
  rm(temp)
}

data_dir <- paste0(getwd(), "/SERCE/")

# Is the unrared file nam in the folder?
unrar_name <- grepl("Bases pa", list.files(data_dir))

# If the files is not there, then unrar it
if (!(any(unrar_name))) {
  # Unrar it
}
  
