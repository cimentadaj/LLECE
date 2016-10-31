# This function checks if a folder exists in the supplied directory
# if not, it downloads the zip file from the supplied url and unzips
# in the provided directory

# zipfile = zip file
# url = URL path where the zip file is located
# string = folder name that should/shouldn't be in the dir argument
# dir = directory where the file is searched for and the file is unzipped

downloader_file <- function(zipfile, url, string, dir) {
  require(downloader)
  if (!(zipfile %in% list.files(dir))) {
    # Names of zip file and download path
    zip_file <- zipfile
    download_url <- url
    
    # Create temporary directory and temporary file path
    temp <- tempdir()
    dir_file <- paste0(temp, "/", zip_file)
    
    # Download file and save in temporary file
    download(paste0(download_url, zip_file), dir_file)
    
    # unzip in working directory
    unzip(dir_file)
    rm(temp)
  }
}
