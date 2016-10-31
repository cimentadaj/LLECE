library(tidyverse)



# SET WORKING DIRECTORY HERE WHERE FILES WILL BE (IN CASE THEY MUST BE DOWNLOADED)
# AND WHERE FILES ARE (IN CASE NO DOWNLOAD IS NECESSARY)

setwd("/Users/cimentadaj/Downloads/serce")

github_fun <- "https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/downloader_file.R"
source_url(github_fun,
           sha = sha_url(github_fun))

downloader_file("bcf362e6.zip",
                "http://www.unesco.org/new/fileadmin/MULTIMEDIA/FIELD/Santiago/zip/",
                "SERCE",
                getwd())

data_dir <- paste0(getwd(), "/SERCE/")

# Is the unrared file nam in the folder?
unrar_name <- grepl("Bases pa", list.files(data_dir))

# If the files is not there, then unrar it
if (!(any(unrar_name))) {
  # Unrar it
}
  
