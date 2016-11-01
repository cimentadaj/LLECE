library(tidyverse)
library(downloader)
library(haven)

# SET WORKING DIRECTORY HERE WHERE FILES WILL BE (IN CASE THEY MUST BE DOWNLOADED)
# AND WHERE FILES ARE (IN CASE NO DOWNLOAD IS NECESSARY)

setwd("/Users/cimentadaj/Downloads/perce/")

funs <- c("https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/downloader_file.R",
          "https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/unrar.R")

# NOTE: The unrar function depends havily on unrar from OSx. Please
# have a look at the function file and install or update the necessary
# programs

for (i in funs) source_url(i, sha = sha_url(i))


downloader_file("e6e641d8.zip",
                "http://www.unesco.org/new/fileadmin/MULTIMEDIA/FIELD/Santiago/zip/",
                "PERCE",
                getwd())

data_dir <- paste0(getwd(), "/PERCE/")

if (!("PERCE" %in% list.files(data_dir))) {
  rar_path <- paste0(data_dir, grep("Bases_paises", list.files(data_dir), value = T))
  unrar(rar_path, data_dir)
}

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