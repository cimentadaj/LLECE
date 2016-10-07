library(dplyr)
library(readr)
###### Reading and merging third grade data #######

direc <- "/Users/cimentadaj/Downloads/Factores asociados/Texto/"
direc1 <- "/Users/cimentadaj/Downloads/Logro de aprendizaje/"

# unzip(paste0(direc1, "Resultados texto.zip"), exdir = paste0(direc1))

direc2 <- paste0(direc1, "Bases de datos texto/")

# for (i in list.files(direc, pattern = "*.csv")) {
#   write_csv(empty <- read_delim(paste0(direc, i), delim = ";", col_names = T), path = paste0(direc, i))
# }
# rm(empty)

files3 <- c("QA3.csv", "QD3.csv", "QF3.csv", "QP3L.csv", "QP3M.csv", "PL3_all_TERCE.csv", "PM3_all_TERCE.csv")
all3 <- paste0(direc, files3[1:(length(files3) - 2)])
all3 <- c(all3, paste0(direc2, files3[(length(files3) - 1):length(files3)]))

files6 <- c("QA6.csv", "QD6.csv", "QF6.csv", "QP6L.csv", "QP6M.csv", "QP6C.csv", "PL6_all_TERCE.csv", "PM6_all_TERCE.csv", "PC6_all_TERCE.csv")
all6 <- paste0(direc, files6[1:(length(files6) - 3)])
all6 <- c(all6, paste0(direc2, files6[(length(files6) - 2):length(files6)]))

vecname3 <- paste0(c("student", "director", "family", "lteacher", "mteacher", "language", "math"), "3")
vecname6 <- paste0(c("student", "director", "family", "lteacher", "mteacher", "steacher", "language", "math", "science"), "6")

data_compiled <- list(third = list(), sixth = list())
data_compiled[[1]] <- Map(function(x, y) assign(x, read_csv(y, col_names = T)), vecname3, all3)
data_compiled[[2]] <- Map(function(x, y) assign(x, read_csv(y, col_names = T)), vecname6, all6)


data_compiled <- lapply(data_compiled, function(x) lapply(x, function(p) {names(p) <- tolower(names(p)); p}))
data_compiled <- lapply(data_compiled, function(x) lapply(x, as.data.frame))

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
# excp
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
  
  # Here I set all names of each data frame to the suffix name. The suffix needs to be the same length as dat
  df <- Map(function(x, y) { names(x) <- paste0(names(x), y); x },  dat, suffix)
  # Function merges every element of the list
  teach_dir <- df[grep2_pattern("director","teacher", names(df))]
  teach_dir_merge <- Reduce(function(x, y) inner_join(x, y, by = c(grep("oID", names(x), value = T) = grep("oID", names(y), value = T))), teach_dir)
  # Here I'm trying to pass the key columns interactively be searching for oID in each dataframe and matching on that
  # variable. There's an error up to now that doesn't allow to name a character vector with a column name.
  # Function merges every element of the list
  student2 <- df[grep2_pattern("director","teacher", names(df), actual = F)]
  student2_merge <- Reduce(function(x, y) inner_join(x, y, by = c("sID")), student2)
  
  all <- inner_join(student2_merge, teach_dir_merge, by = c("oID.x" = "oID"))
  
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


all_data2 <- setmove(all_data, c("sID", "oID", "country", "dependencia", "ruralidad", "genero", "idgrade"))

rm(list = ls()[!(ls() %in% "all_data2")])
