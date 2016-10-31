library(tidyverse)
library(downloader)


# SET WORKING DIRECTORY HERE WHERE FILES WILL BE (IN CASE THEY MUST BE DOWNLOADED)
# AND WHERE FILES ARE (IN CASE NO DOWNLOAD IS NECESSARY)

setwd("/Users/cimentadaj/Downloads/perce/")

# If the file hasn't been unzipped, then download and unzip it.
if (!("PERCE" %in% list.files(getwd()))) {
  # Names of zip file and download path
  zip_file <- "e6e641d8.zip"
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

data_dir <- paste0(getwd(), "/PERCE")

if (!("PERCE" %in% list.files(data_dir))) {
  # Unrar file
}

data_dir_upd <- paste0(data_dir, "/PERCE/")
data_files <- list.files(data_dir_upd)

