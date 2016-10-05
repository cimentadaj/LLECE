library(haven)
library(data.table)
library(dplyr)
###### Reading and merging third grade data #######

direc <- "/Users/cimentadaj/Downloads/Factores asociados/Texto/"
direc2 <- "/Users/cimentadaj/Downloads/Logro de aprendizaje/Bases de datos texto/"
files <- c("QA3.csv", "QD3.csv", "QF3.csv", "QP3L.csv", "QP3M.csv", "QA6.csv", "QD6.csv", "QF6.csv", "QP6L.csv", "QP6M.csv", "QP6C.csv")
files2 <- c("PL3_all_TERCE.csv", "PM3_all_TERCE.csv", "PL6_all_TERCE.csv", "PM6_all_TERCE.csv", "PC6_all_TERCE.csv")
vecnames <- c("student3", "director3", "family3", "lteacher3", "mteacher3", "student6", "director6", "family6", "lteacher6", "mteacher6", "steacher6")
vecnames2 <- c("language3", "math3", "language6", "math6", "science6")

path <- paste0(direc, files) # All 3rd and 6th grader files without the exam scores
path2 <- paste0(direc2, files2) # All 3rd and 6th grader files for exam scores

first_b <- Map(function(x, y) assign(x, fread(y, header = T, sep = ";")), vecnames, path)
second_b <- Map(function(x, y) assign(x, fread(y, header = T, sep = ",")), vecnames2, path2)

all_dat <- append(first_b, second_b)
all_dat <- lapply(all_dat, function(x) { names(x) <- tolower(names(x)); x})
all_dat <- lapply(all_dat, as.data.frame)

secondl <- c("director3", "lteacher3", "mteacher3", "director6", "lteacher6", "mteacher6", "steacher6")

for (i in names(all_dat)) {
    all_dat[[i]]$oID <- paste0(all_dat[[i]]$idcntry, all_dat[[i]]$idschool)
    all_dat[[i]]$sID <- paste0(all_dat[[i]]$idcntry, all_dat[[i]]$idstud)
}

all_dat3 <- all_dat[grep("3", names(all_dat))]
all_dat6 <- all_dat[grep("6", names(all_dat))]

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

all_data4 <- merger(all_dat3,
                    suffix = c("_student", "_director", "_family",
                               "_lteacher", "_mteacher", "_language",
                               "_math"))

all_data6 <- merger(all_dat6,
                    suffix = c("_student", "_director", "_family",
                               "_lteacher", "_mteacher", "_steacher",
                               "_language", "_math", "_science"))

all_data <- full_join(all_data4, all_data6)
