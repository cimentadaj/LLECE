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


# New path for the unrared folder  
new_path <- paste0(data_dir, grep("Bases pa", list.files(data_dir), value = T), "/")

# All files within that folder
fls_path <- list.files(new_path)
# Obtain only the file names without a '.' in their names, so only the folders
folder_names <- fls_path[!grepl("\\.", fls_path)]

# Create a path for each of these new folders
folder_path <- paste0(new_path, folder_names, "/")

# Create empty list where data sets paths will be stored
all_data <- vector("list", 3) # why 3? because we'll group third and 6th graders
all_paths <- vector("list", 3)# and director data separately. This is HIGHLY
                              # unlikely to change in the future as they won't 
                              # add anymore 'surveys'.

# All paths
paths <- lapply(folder_path, function(x) paste0(x, list.files(x, "*.sav")))
all_paths_vec <- do.call(`c`, paths)

all_paths[[1]] <- grep("[[:alpha:]]{1,}3", all_paths_vec, value = T)
all_paths[[2]] <- grep("[[:alpha:]]{1,}6", all_paths_vec, value = T)
all_paths[[3]] <- setdiff(all_paths_vec, c(all_paths[[1]], all_paths[[2]]))

all_data <- lapply(all_paths, function(x) lapply(x, read_spss))

joiner <- function(x, y, key_vars, ...) {
  
  # Loop through all key variables and whenever
  # a key variable is present in both data frames,
  # stop the loop and assign the key variable to vector 'key'.
  # If a key variable is in neither
  # data frames, continue to the next key variable
  for (i in key_vars) {
    if (i %in% names(x) && i %in% names(y)) { 
      key <- i; break
    } else {
      next
    }
  }
  
  # Merge data frame with the common key
  df <- full_join(x, y, key, ...)
  
  # identify the keys that didn't match and change their names
  # from the .x suffix to the original name. This is done
  # for recursive merging which depends on the other 
  # original names
  remain <- setdiff(key_vars, key)
  for (i in remain) names(df) <- gsub(paste0(i, "\\.x"), i, names(df))
  
  df
}

key_variables <- c("id_alumno", "id_gradoaula", "LlavePaisCentro")

## EVerything below this line is experimental
p <- lapply(all_data, function(x) {
  Reduce(function(x, y) joiner(x, y, key_variables), x)
  })

p[[1]]


serce <- lapply(all_data, function(x) {
  Reduce(function(x, y) full_join(x, y), x)
})

serce2 <- Reduce(function(x, y) full_join(x, y), serce)


# Here's the problem up to now:
# We join the student and family questionnaire:
AF <- full_join(all_data[[2]][[1]], all_data[[2]][[2]], c("id_alumno", "id_gradoaula"))
# EVerything is fine:
dim(AF)

# We then join the math, language and science datasets (in that order):
notas <- full_join(all_data[[2]][[4]], all_data[[2]][[5]], c("id_alumno", "id_gradoaula"))
notas <- full_join(notas, all_data[[2]][[3]], c("id_alumno", "id_gradoaula"))
# Everything is fine:
dim(notas)

# We merge them together:
notas_AF <- full_join(AF, notas, c("id_alumno", "id_gradoaula"))
dim(notas_AF)

# Everything is fine up to now. Because we have the same respondents across all
# data sets. However, when we start merging the teacher data, observations
# start to grow exponentially to little over 500,000. This is
# obviously because some students are being duplicated due to more than 1 teacher
# within a classroom.

# For example, we have student A in Class 1. Class 1 has three teachers.
# When we merge student A with the three rows from Class 1, student 1
# gets duplicated 3 times. This happens because the children data doesn't
# have the unique id for teachers, but only for class rooms. If we could
# match students to teachers, then the info from the teachers would
# simply be added as columns instead of duplication.


prof1 <- full_join(notas_AF, all_data[[2]][[6]], "id_gradoaula")
dim(prof1)
prof1 <- full_join(prof1, all_data[[2]][[7]], "id_gradoaula")
dim(prof1)
prof1 <- full_join(prof1, all_data[[2]][[8]], "id_gradoaula")
dim(prof1)
prof1 <- full_join(prof1, all_data[[2]][[9]], "id_gradoaula")
dim(prof1)
