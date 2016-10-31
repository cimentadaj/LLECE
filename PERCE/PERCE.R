library(tidyverse)
library(downloader)
library(haven)

# SET WORKING DIRECTORY HERE WHERE FILES WILL BE (IN CASE THEY MUST BE DOWNLOADED)
# AND WHERE FILES ARE (IN CASE NO DOWNLOAD IS NECESSARY)

setwd("/Users/cimentadaj/Downloads/perce/")

github_fun <- "https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/downloader_file.R"
source_url(github_fun, sha = sha_url(github_fun))

downloader_file("e6e641d8.zip",
                "http://www.unesco.org/new/fileadmin/MULTIMEDIA/FIELD/Santiago/zip/",
                "PERCE",
                getwd())

data_dir <- paste0(getwd(), "/PERCE")

# if (!("PERCE" %in% list.files(data_dir))) {
#  # Unrar file
# }

data_dir_upd <- paste0(data_dir, "/PERCE/")
data_files <- list.files(data_dir_upd)

paths <- paste0(data_dir_upd, data_files)

# For a detailed description of what each data base is, please refer to the only document in the perce
# download file.
data_names <- c("lstudents_parents",
                "lteacher",
                "ldirector",
                "mstudents_parents",
                "mteacher",
                "mdirector")

all_data <- Map(function(x, y) assign(x, read_spss(y)), data_names, paths)

all_data <- lapply(all_data, function(p) {
  names(p) <- tolower(names(p))
  p <- as.data.frame(p)
  p
})

perce <- Reduce(function(x, y) full_join(x, y), all_data)
rm(list = ls()[!(ls() %in% c("perce"))])

