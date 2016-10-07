library(dplyr)
library(readr)
###### Reading and merging third grade data #######

direc <- "/Users/cimentadaj/Downloads/Factores asociados/Texto/"
direc1 <- "/Users/cimentadaj/Downloads/Logro de aprendizaje/"

unzip(paste0(direc1, "Resultados texto.zip"), exdir = paste0(direc1))

direc2 <- paste0(direc1, "Bases de datos texto/")

for (i in list.files(direc, pattern = "*.csv")) {
  print(i)
  write_csv(empty <- read_delim(paste0(direc, i), delim = ";", col_names = T), path = paste0(direc, i))
}
rm(empty)

files3 <- c("QA3.csv", "QD3.csv", "QF3.csv", "QP3L.csv", "QP3M.csv", "PL3_all_TERCE.csv", "PM3_all_TERCE.csv")
all3 <- paste0(direc, files3[1:(length(files3) - 2)])
all3 <- c(all3, paste0(direc2, files3[(length(files3) - 1):length(files3)]))

files6 <- c("QA6.csv", "QD6.csv", "QF6.csv", "QP6L.csv", "QP6M.csv", "QP6C.csv", "PL6_all_TERCE.csv", "PM6_all_TERCE.csv", "PC6_all_TERCE.csv")
all6 <- paste0(direc, files6[1:(length(files6) - 3)])
all6 <- c(all6, paste0(direc2, files6[(length(files6) - 2):length(files6)]))

vecname3 <- paste0(c("student", "director", "family", "lteacher", "mteacher", "language", "math"), "3")
vecname6 <- paste0(c("student", "director", "family", "lteacher", "mteacher", "steacher", "language", "math", "science"), "6")

all_dat <- list(third = list(), sixth = list())
all_dat[[1]] <- Map(function(x, y) assign(x, read_csv(y, col_names = T)), vecname3, all3)
all_dat[[2]] <- Map(function(x, y) assign(x, read_csv(y, col_names = T)), vecname6, all6)


all_dat <- lapply(all_dat, function(x) lapply(x, function(p) {names(p) <- tolower(names(p)); p}))
all_dat <- lapply(all_dat, function(x) lapply(x, as.data.frame))

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

all_dat2 <- lapply(all_dat, function(x) lapply(x, function(p) {
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

merger <- function(list_file, suffix) {
  
  # Function loops over data frame all_data and suffix and changes the names of each dataframe
  # excluding names oID and sID
  all_data2 <- Map(function(x, y) setNames(x, namer(names(x), y, c("oID", "sID"))), list_file, suffix)
  
  # Function merges every element of the list
  all_data2 <- Reduce(function(x, y) base::merge(x, y, by = c("sID", "oID"), all = T), all_data2)
  
  cols <- lapply(all_data2$sID, substring, c(1, 4, 8, 10), c(3, 7, 9, 12))
  
  cols2 <- data.frame(do.call(rbind, cols))
  names(cols2) <- c("cntry", "school", "class", "student")
  
  all_data2 <- as.data.frame(append(all_data2, cols2, after = 0))
  all_data2
}

all_data4 <- merger(all_dat2[[1]],
                    suffix = c("_student", "_director", "_family",
                               "_lteacher", "_mteacher", "_language",
                               "_math"))

all_data6 <- merger(all_dat2[[2]],
                    suffix = c("_student", "_director", "_family",
                               "_lteacher", "_mteacher", "_steacher",
                               "_language", "_math", "_science"))

all_data <- full_join(all_data4, all_data6)

rm(list = ls()[!(ls() %in% "all_data")])
