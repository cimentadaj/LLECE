library(tidyverse)
library(haven)
library(readr)
library(downloader)

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


###### Reading and merging third grade data #######

direc <- paste0(getwd(), "/", correct_name, "/")

dir_bases <- paste0(direc[1], "Bases de datos texto", "/")
dir_texto <- paste0(direc[2], "Texto", "/")

# If the file is not unzipped, unzip it.
if ( !("Bases de datos texto" %in% list.files(direc[1])) ) {
  # Find the 'text' zip file and create zip file path
  unzp_file <- grep("texto", list.files(direc[1]), value = T)
  unzp_file <- paste0(direc[1], unzp_file)
  
  # unzip to its directory
  unzip(unzp_file, exdir = paste0(direc[1]))
}

if ( !("Texto" %in% list.files(direc[2])) ) {
  # Stil to do: unrar "Texto"
  
  # All csv files inside the "Texto" folder in Factores asociados
  csv_files <- list.files(dir_texto, pattern = "*.csv")
  
  # Read all csv files with ; separator and save as csv files
  for (i in csv_files) {
      write_csv(empty <- read_delim(paste0(dir_texto, i), delim = ";", col_names = T),
                path = paste0(dir_texto, i))
  }
  rm(empty)
}

# In case you unrared "Texto" manually, then it assumes that each csv file is comma
# separated and not separated by ';'. In case it's not, run the loop above which
# reads each csv as ; delimited and saves as comma separated.

files3 <- c("QA3.csv",
            "QD3.csv",
            "QF3.csv",
            "QP3L.csv",
            "QP3M.csv",
            "PL3_all_TERCE.csv",
            "PM3_all_TERCE.csv")

# Only 3rd grade files from the texto folder
all3 <- paste0(dir_texto, files3[1:(length(files3) - 2)])
# Now combined with the files from the bases de texto folder
all3 <- c(all3, paste0(dir_bases, files3[(length(files3) - 1):length(files3)]))

# Repeat exactly the same from above to 6th grade (which has the additional science class)
files6 <- c("QA6.csv",
            "QD6.csv",
            "QF6.csv",
            "QP6L.csv",
            "QP6M.csv",
            "QP6C.csv",
            "PL6_all_TERCE.csv",
            "PM6_all_TERCE.csv",
            "PC6_all_TERCE.csv")

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


data_compiled <- list(third = list(), sixth = list())
# Read each data from third and 6th grade and assign the database name from the above vectors
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
