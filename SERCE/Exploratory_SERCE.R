# Add checking for a backslash as the last character of directory

directory <- "/Users/cimentadaj/Downloads/serce/SERCE/"
serce <- function(directory, return_df = T, save = F, output_path = directory, save_format = c("csv", "Stata", "SPSS"), stata_version = 13) {
  require(tidyverse)
  require(haven)
  require(readr)
  require(downloader)
  require(stringi)
  
  unrar_fun <- "https://raw.githubusercontent.com/cimentadaj/LLECE/master/Functions/unrar.R"
  source_url(unrar_fun, sha = sha_url(unrar_fun))
  
  stopifnot(dir.exists(directory))
  
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
  
  folder <- paste0(directory, grep(".rar|.zip", list.files(directory), inv = T, value = T), "/")
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
                    "language_q",
                    "math_q",
                    "teacher_q"),
                  "3")
  
  sixth <- paste0(c("student",
                    "family",
                    "sscore",
                    "lscore",
                    "mscore",
                    "science_q",
                    "language_q",
                    "math_q",
                    "teacher_q"),
                  "6")
  
  school <- c("school_q", "director_q")
  
  all_data <- list(third = NULL, sixth = NULL, school = NULL)
  
  all_data[[1]] <- Map(function(x, y) assign(x, read_spss(y)), third, all_paths[[1]])
  all_data[[2]] <- Map(function(x, y) assign(x, read_spss(y)), sixth, all_paths[[2]])
  all_data[[3]] <- Map(function(x, y) assign(x, read_spss(y)), school, all_paths[[3]])
  
  for()
  # Merge only student data with the "id_alumno" key
  all3 <- Reduce(function(x, y) full_join(x, y, "id_alumno"), all_data[[1]][1:4])
  all3_2 <- Reduce(function(x, y) full_join(x, y, "id_profesor"), all_data[[1]][5:7])
  all3_3 <- full_join(all3, all3_2, "id_gradoaula.x" = "id_gradoaula")
  
  all6 <- Reduce(function(x, y) full_join(x, y, "id_alumno"), all_data[[2]][1:5])
  all6_2 <- Reduce(function(x, y) full_join(x, y, "id_profesor"), all_data[[2]][6:9])
  all6_3 <- full_join(all6, all6_2, "id_gradoaula.x" = "id_gradoaula")
  
  all_dir <- full_join(all_data[[3]][[1]], all_data[[3]][[2]], "LlavePaisCentro")
  
  all <- full_join(all3_3, all6_3)
  all2 <- full_join(all_dir, all, "LlavePaisCentro" = "LlavePaisCentro.x")
  
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
  
  
  
  # Lower case column names and transform to data frame
  data_compiled <- lapply(data_compiled, function(x) lapply(x, function(p) {
    names(p) <- tolower(names(p))
    p <- as.data.frame(p)
    p
  }))
  
  
  data_compiled[[1]] <- lapply(data_compiled[[1]], function(i) { i$idgrade <- 3; i})
  data_compiled[[2]] <- lapply(data_compiled[[2]], function(i) { i$idgrade <- 6; i})
  
  data_compiled2 <- lapply(data_compiled, function(x) lapply(x, function(p) {
    p$sID <- paste0(p$idgrade, p$idcntry, p$idstud)
    p$oID <- paste0(p$idgrade, p$idcntry, p$idschool)
    p$country <- finder[as.character(p$idcntry)]
    p
  }))
  
  # Function updates the names of vector nam with the suffix char, excluding the elements
  # 'excp'
  namer <- function(nam, char, excp) {
    charstr <- nam[!(nam %in% excp)]
    nam[!(nam %in% excp)] <- paste0(charstr, char)
    nam
  }
  
  merger <- function(dat, suffix) {
    df <- Map(function(x, y) setNames(x, namer(names(x), y, c("oID", "sID"))), dat, suffix)
    # Function merges every element of the list
    teach_dir <- df[grep("director|teacher", names(df), value = T)]
    teach_dir_merge <- Reduce(function(x, y) full_join(x, y, by = c("oID", "sID")), teach_dir)
    
    # Function merges every element of the list
    student2 <- df[grep("director|teacher", names(df), inv = T, value = T)]
    student2_merge <- Reduce(function(x, y) full_join(x, y, by = c("oID", "sID")), student2)
    
    all <- full_join(student2_merge, teach_dir_merge, by = c("oID", "sID"))
    
    all
  }
  
  three <- merger(data_compiled2[[1]], suffix = c("_student", "_director", "_family",
                                                  "_lteacher", "_mteacher", "_language",
                                                  "_math"))
  
  six <- merger(data_compiled2[[2]], suffix = c("_student", "_director", "_family",
                                                "_steacher", "_lteacher", "_mteacher",
                                                "_science", "_language", "_math"))
  
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
  
  names(all_data) <- gsub("date.df", "date_df", names(all_data))
  
  
  all_data2 <- setmove(all_data, c("sID",
                                   "oID",
                                   "country",
                                   "dependencia",
                                   "ruralidad",
                                   "genero",
                                   "idgrade"))
  
  if (save) {
    suffix <- switch(save_format[1],
                     "csv" = ".csv",
                     "Stata" = ".dta",
                     "SPSS" = ".sav")
    
    output_path <- paste0(output_path, "terce", suffix)
    
    if (save_format[1] == "csv") write_csv(all_data2, output_path)
    if (save_format[1] == "Stata") write_dta(all_data2, output_path, stata_version)
    if (save_format[1] == "SPSS") write_sav(all_data2, output_path)
  }
  
  if (return_df) return(all_data2)
  rm(list = ls()[!(ls() %in% c("all_data2"))])
  
}