library(tidyverse)
library(haven)
library(readr)
library(downloader)
unrar_fun <- "https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/unrar.R"

# One big mistake-prone part of the code is assigning a different database name
# to a different database.

# returns the df with lowercase names
# returns the country codes changed as country names

# SET WORKING DIRECTORY HERE WHERE FILES WILL BE (IN CASE THEY MUST BE DOWNLOADED)
# AND WHERE FILES ARE (IN CASE NO DOWNLOAD IS NECESSARY)

setwd("/Users/cimentadaj/Downloads/terce")

# File names and url
database <- c("Logro-de-aprendizaje.zip", "Factores-asociados.zip")
download_url <- "http://www.unesco.org/new/fileadmin/MULTIMEDIA/FIELD/Santiago/zip/"

# Make folder names
correct_name <- gsub(".zip", "", database)
correct_name <- gsub("-", " ", correct_name)

# Which folders are not in the working directory?
index <- which(!(correct_name %in% list.files(getwd())))

# For the files which are not in the working directory, download and unzip in the wd
if (length(index) != 0) {
  # Files which haven't been downloaded
  fl <- database[index]
  temp <- tempdir()
  for (i in fl) {
    # Create directory to download
    file_name <- paste0(temp, "/", i)
    
    # Download files and unzip
    download(paste0(download_url, i), file_name)
    unzip(file_name, exdir = getwd())
  }
  rm(temp)
}

# save as different files
dir <- "/Users/cimentadaj/Downloads/terce/"

terce <- function(directory) {
  stopifnot(dir.exists(directory))
  
# Checks which files are unzipped:
database <- c("Logro-de-aprendizaje.zip", "Factores-asociados.zip")
correct_name <- gsub(".zip", "", database)
correct_name <- gsub("-", " ", correct_name)
index <- which(!(correct_name %in% list.files(directory)))

# If some files haven't been unzipped, then unzip them:
if (length(index) != 0) {
  # Files which haven't been unzipped
  fl <- database[index]
  for (i in fl) unzip(paste0(directory , "/", i), exdir = directory)
}

# Create a directory with the with unzipped folders from the directory provided
direc <- paste0(directory, "/", list.files(directory, pattern = " "), "/")

# Let's access the folders where the data is:
dir_bases <- paste0(direc[2], "Bases de datos texto", "/")
dir_texto <- paste0(direc[1], "Texto", "/")

# If the file is not unzipped, unzip it.
if ( !("Bases de datos texto" %in% list.files(direc[2])) ) {
  fl_zip <- direc[2]
  # Find the 'text' zip file and create zip file path
  unzp_file <- grep("texto", list.files(fl_zip), value = T)
  unzp_file <- paste0(fl_zip, unzp_file)
  
  # unzip to its directory
  unzip(unzp_file, exdir = paste0(direc[2]))
}

# If 'Texto' hasn't been unrared, unrar it.
if ( !("Texto" %in% list.files(direc[1])) ) {
  fl_zip2 <- direc[1]
  unrar_fun <- "https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/unrar.R"
  source_url(unrar_fun, sha = sha_url(unrar_fun))
  
  # Create exact path to the .rar file
  rar_path <- paste0(fl_zip2, grep("texto", list.files(fl_zip2), value = T))
  unrar(rar_path, fl_zip2)
  
  # All csv files inside the "Texto" folder in Factores asociados
  csv_files <- list.files(dir_texto, pattern = "*.csv")
  
  # Read all csv files with that are ';' separated and save as csv.
  for (i in csv_files) {
      write_csv(empty <- read_delim(paste0(dir_texto, i), delim = ";", col_names = T),
                path = paste0(dir_texto, i))
  }
  rm(empty)
}

# In case you unrared "Texto" manually, then the script assumes
# that each csv file is comma separated and not separated by ';'.
# In case it's not, run the loop above which reads each csv as ;
# delimited and saves as comma separated.

# Files for only third graders
files3 <- c(list.files(dir_texto, pattern = "3"), list.files(dir_bases, pattern = "3"))

# Only 3rd grade files from the texto folder
all3 <- paste0(dir_texto, files3[1:(length(files3) - 2)])

# Now combined with the files from the bases de texto folder
all3 <- c(all3, paste0(dir_bases, files3[(length(files3) - 1):length(files3)]))

# Repeat exactly the same from above to 6th grade (which has the additional science class)
files6 <- c(list.files(dir_texto, pattern = "6"), list.files(dir_bases, pattern = "6"))

# Only the 6th grades from texto folder
all6 <- paste0(dir_texto, files6[1:(length(files6) - 3)])
# Now combined with the files from bases de texto folder
all6 <- c(all6, paste0(dir_bases, files6[(length(files6) - 2):length(files6)]))

# Names of each data base to be assigned

vecname3 <- paste0(c("student",
                     "director",
                     "family",
                     "lteacher",
                     "mteacher",
                     "language",
                     "math"),
                   "3")

vecname6 <- paste0(c("student",
                     "director",
                     "family",
                     "lteacher",
                     "mteacher",
                     "steacher",
                     "language",
                     "math",
                     "science"),
                   "6")


# List that will contain the third and sixth data base.
data_compiled <- list(third = list(), sixth = list())

# Read each data from third and sixth grade and assign the database name from the above vectors
data_compiled[[1]] <- Map(function(x, y) assign(x, read_csv(y, col_names = T)), vecname3, all3)
data_compiled[[2]] <- Map(function(x, y) assign(x, read_csv(y, col_names = T)), vecname6, all6)
           
# Lower case column names and transform to data frame
data_compiled <- lapply(data_compiled, function(x) lapply(x, function(p) {
  names(p) <- tolower(names(p))
  p <- as.data.frame(p)
  p
}))

finder <- c("32" = "ARG",
            "76" = "BRA",
            "152" = "CHL",
            "170" = "COL",
            "188" = "CRI",
            "214" = "REP",
            "218" = "ECU",
            "320" = "GTM",
            "340" = "HON",
            "484" = "MEX",
            "558" = "NIC",
            "591" = "PAN",
            "600" = "PAR",
            "604" = "PER",
            "858" = "URU",
            "4841" = "NLE")
           
data_compiled2 <- lapply(data_compiled, function(x) lapply(x, function(p) {
  p$country <- finder[as.character(p$idcntry)]
  p$sID <- paste0(p$country, p$idstud)
  p$oID <- paste0(p$country, p$idschool)
  p
}))

# Function updates the names of vector nam with the suffix char, excluding the elements
# 'excp'
namer <- function(nam, char, excp) {
  charstr <- nam[!(nam %in% excp)]
  nam[!(nam %in% excp)] <- paste0(charstr, char)
  nam
}

# This function matches two patterns and if actual = T
# subsets only the indexes of those two patterns. If actual
# == F, subsets everything but those two patterns.
grep2_pattern <- function(p1, p2, vec, actual = T) {
  first <- grepl(p1, vec)
  second <- grepl(p2, vec)
  if (actual) vec[as.logical(first + second)]
  else vec[!as.logical(first + second)]
}
           
merger <- function(dat, suffix) {
             
  df <- Map(function(x, y) setNames(x, namer(names(x), y, c("oID", "sID"))), dat, suffix)
  # Function merges every element of the list
  teach_dir <- df[grep2_pattern("director","teacher", names(df))]
  teach_dir_merge <- Reduce(function(x, y) full_join(x, y, by = c("oID")), teach_dir)
             
  # Function merges every element of the list
  student2 <- df[grep2_pattern("director","teacher", names(df), actual = F)]
  student2_merge <- Reduce(function(x, y) full_join(x, y, by = c("sID")), student2)
             
  all <- full_join(student2_merge, teach_dir_merge, by = c("oID.x" = "oID"))
             
  all
}
           
three <- merger(data_compiled2[[1]], suffix = c("_student", "_director", "_family",
                                                "_lteacher", "_mteacher", "_language",
                                                "_math"))
           
six <- merger(data_compiled2[[2]], suffix = c("_student", "_director", "_family",
                                              "_lteacher", "_mteacher", "_steacher",
                                              "_language", "_math", "_science"))
           
all_data <- full_join(three, six)
           
setmove <- function(df, columns) {
  df[, c(columns, setdiff(names(df), columns))]
}
           
all_data$sID <- all_data$idst_student
all_data$oID <- all_data$idsc_student
all_data$country <- all_data$country_student
all_data$dependencia <- all_data$dependencia_student
all_data$ruralidad <- all_data$ruralidad_student
all_data$genero <- all_data$genero_student
all_data$idgrade <- all_data$idgrade_student
           
           
all_data2 <- setmove(all_data, c("sID",
                             "oID",
                             "country",
                             "dependencia",
                             "ruralidad",
                             "genero",
                             "idgrade"))
           
rm(list = ls()[!(ls() %in% c("all_data2"))])
all_data2
}

all <- terce(dir)


suffix <- switch(format, "csv" = ".csv",
                 "Stata" = ".dat",
                 "SPSS" = ".sav",
                 "SAS" = ".sas")

output_path <- paste0(output_path, "terce", suffix)

all_data2 <- all
if (format == "csv") write_csv(all_data2, output_path)
if (format == "Stata") write_dta(all_data2, output_path)
if (format == "SPSS") write_sav(all_data2, output_path)
if (format == "SAS") write_sas(all_data2, output_path)
