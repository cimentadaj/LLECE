# Add checking for a backslash as the last character of directory
# The Stata file that is returned does not have any labels
# and the SERCE data file does not have any codebook

# The problem is that the labels are pretty lengthy
# and write_dta throws an error saying Stata does not accept
# such long labels.

directory <- "/Users/cimentadaj/Downloads/serce/SERCE"

serce <- function(main_dir, return_df = T, save = F, output_path = directory, save_format = c("csv", "Stata", "SPSS"), stata_version = 13) {
  require(tidyverse)
  require(haven)
  require(readr)
  require(downloader)
  require(stringi)
  
  # If the last character is a backslash, return the same directory,
  # if not, the paste the backslash
  directory <- ifelse(strsplit(main_dir, NULL)[[1]][nchar(directory)] != "/",
                      paste0(main_dir, "/"),
                      main_dir)
  stopifnot(dir.exists(directory))
  
  
  unrar_fun <- "https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/unrar.R"
  source_url(unrar_fun, sha = sha_url(unrar_fun))
  
  # Checks which files are unzipped:
  database <- list.files(directory, pattern = ".rar")
  correct_name <- trimws(gsub("_|.rar|SERCE", " ", database), "right")
  
  dir_files <- stri_trans_general(list.files(directory), "Latin-ASCII")
  
  index <- which(!(correct_name %in% dir_files))
  
  # If some files haven't been unzipped, then unzip them:
  if (length(index) != 0) {
    # Files which haven't been unzipped
    fl <- database[index]
    for (i in fl) unrar(paste0(directory , "/", i), exdir = directory)
    
    folders <- grep(".zip|.rar", list.files(directory), inv = T, value = T)
    new_path <- paste0(directory, folders)
    # All files within that folder
    fls_path <- list.files(new_path)
    
    # Obtain only the file names without a '.' in their names, so only the folders
    folder_names <- fls_path[!grepl("\\.", fls_path)]
    
    # Create a path for each of these new folders
    folder_path <- paste0(new_path,"/", folder_names, "/")
    
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
    
    rm(database, correct_name, folder_path, fls_path, new_path, index, dir_files)
  }
  
  # if the first file was unrared manually, the program assumes that
  # all other files within that file have been unrared as well.
  
  folder <- paste0(directory, grep("_|.csv|.sav|.dta", list.files(directory), inv = T, value = T), "/")
  folder_names <- list.files(folder)[grep("\\.", list.files(folder), inv = T)]
  folder_path <- paste0(folder, folder_names, "/")
  
  # Create empty list where data sets paths will be stored
  all_data <- vector("list", 3) # why 3? because we'll group third and 6th graders
  all_paths <- vector("list", 3)# and director data separately. This is HIGHLY
  # unlikely to change in the future as they won't 
  # add anymore 'surveys'.
  
  # All paths
  paths <- lapply(folder_path, function(x) paste0(x, list.files(x, "*.sav")))
  all_paths_vec <- do.call(`c`, paths)
  
  all_paths[[1]] <- sort(grep("[[:alpha:]]{1,}3", all_paths_vec, value = T))
  all_paths[[2]] <- sort(grep("[[:alpha:]]{1,}6", all_paths_vec, value = T))
  all_paths[[3]] <- sort(setdiff(all_paths_vec, c(all_paths[[1]], all_paths[[2]])))
  
  # Names of each data base to be assigned
  
  third <- paste0(c("student",
                    "family",
                    "lscore",
                    "mscore",
                    "language",
                    "math",
                    "teacher"),
                  "3")
  
  sixth <- paste0(c("student",
                    "family",
                    "sscore",
                    "lscore",
                    "mscore",
                    "science",
                    "language",
                    "math",
                    "teacher"),
                  "6")
  
  school <- c("school", "director")
  
  all_data <- list(third = NULL, sixth = NULL, school = NULL)
  
  # Read the third and sixth grade data into the lists and the school/director data
  all_data[[1]] <- Map(function(x, y) assign(x, read_spss(y)), third, all_paths[[1]])
  all_data[[2]] <- Map(function(x, y) assign(x, read_spss(y)), sixth, all_paths[[2]])
  all_data[[3]] <- Map(function(x, y) assign(x, read_spss(y)), school, all_paths[[3]])
  
  # Function updates the names of vector nam with the suffix char, excluding the elements
  # 'excp'
  namer <- function(nam, char, excp) {
    charstr <- nam[!(nam %in% excp)]
    nam[!(nam %in% excp)] <- paste0(charstr, char)
    nam
  }
  
  # Creating the suffix for each dataset
  third <- paste0("_", gsub("3", "", third))
  sixth <- paste0("_", gsub("6", "", sixth))
  school <- paste0("_", school)
  
  # Lower case column names and transform to data frame
  all_data <- lapply(all_data, function(x) lapply(x, function(p) {
    names(p) <- tolower(names(p))
    p <- as.data.frame(p)
    p
  }))
    
  vars <- c("id_alumno", "id_profesor", "id_gradoaula", "llavepaiscentro")
  
  all_data[[1]] <- Map(function(x, y) setNames(x, namer(names(x), y, vars)),
                       all_data[[1]], third)
  
  all_data[[2]] <- Map(function(x, y) setNames(x, namer(names(x), y, vars)),
                       all_data[[2]], sixth)
  
  all_data[[3]] <- Map(function(x, y) setNames(x, namer(names(x), y, vars)),
                       all_data[[3]], school)
  
  # This function accepts two data frames and vector of common
  # variable names. The function only merges on the common variables
  # This is useful when doing dynamic merging such as in the code chunks below
  # where the common variables change between data frames.
  joiner <- function(x, y, keys, ...) {
    
    keys2 <- intersect(keys, names(x))
    keys3 <- intersect(keys2, names(y))
    print(keys3)
    full_join(x, y, keys3, ...)
  }
  
  #  merge all datasets of students separately from the score and then
  # merge together by a different key.
  student_3 <- Reduce(function(x, y) joiner(x, y, vars), all_data[[1]][1:4])
  scores_3 <- Reduce(function(x, y) joiner(x, y, vars), all_data[[1]][5:7])
  third <- full_join(student_3, scores_3, c("id_gradoaula", "llavepaiscentro")) %>%
    filter(!duplicated(id_alumno))

  student_6 <- Reduce(function(x, y) joiner(x, y, vars), all_data[[2]][1:5])
  scores_6 <- Reduce(function(x, y) joiner(x, y, vars), all_data[[2]][6:9])
  six <- full_join(student_6, scores_6, c("id_gradoaula", "llavepaiscentro")) %>%
    filter(!duplicated(id_alumno))

  dir_school <- joiner(all_data[[3]][[1]], all_data[[3]][[2]], vars)
  
  both_grades <- full_join(third, six)
  serce <- full_join(dir_school, both_grades, c("llavepaiscentro" = "llavepaiscentro")) %>%
    rename(oID = llavepaiscentro,
           sID = id_alumno,
           country = pais_student,
           ruralidad = admrur_student,
           genero = qa3_item_2_student,
           idgrade = grado_student)
  
  # dependencia is still missing
  
  # Still need to recode country names
  # p$country <- finder[as.character(p$idcntry)]

  setmove <- function(df, columns) {
    df[, c(columns, setdiff(names(df), columns))]
  }
  
  serce <- setmove(serce, c("sID",
                            "oID",
                            "country",
                            "ruralidad",
                            "genero",
                            "idgrade"))
  
  names(serce) <- gsub("_tccsc_", "_", names(serce))
  
  attributer <- function(x, value_change) {
    #attr(x, "label") <- value_change
    #attr(x, "labels") <- value_change
    attributes(x) <- value_change
    
    x
  }
  
  serce[] <- lapply(serce, attributer, NULL)
  
  if (save) {
    suffix <- switch(save_format[1],
                     "csv" = ".csv",
                     "Stata" = ".dta",
                     "SPSS" = ".sav")
    
    output_path <- paste0(output_path, "serce", suffix)
    
    if (save_format[1] == "csv") write_csv(serce, output_path)
    if (save_format[1] == "Stata") write_dta(serce, output_path, stata_version)
    if (save_format[1] == "SPSS") write_sav(serce, output_path)
  }
  
  if (return_df) return(serce)
  rm(list = ls()[!(ls() %in% c("serce"))])  
}


