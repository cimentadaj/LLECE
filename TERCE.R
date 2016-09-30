library(haven)
library(dplyr)
library(ggplot2)
library(data.table)
library(car)
library(psych)
library(arm)
library(effects)
library(lme4)

setwd("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Analysis Section")

###### Reading and merging third grade data #######

variable_list <- list(vars_student3 = c("oID", "idclass", "idstud", "sID", "idgrade", "idpl", "idpm", 
                                        "edad", "nina", "wgt", "wgt_sen", "organa3", "asisdoa3", "dependencia", 
                                        "ruralidad", "idstrat", "genero", "dqa3it01", "dqa3it02", "dqa3it03_01", 
                                        "dqa3it03_02", "dqa3it04_01", "dqa3it04_02", "dqa3it04_03", "dqa3it04_04", 
                                        "dqa3it04_05", "dqa3it04_06", "dqa3it05"),
                      vars_director3 = c("idgrade", "idcntry", "country", "idschool", "oID", "dependencia", 
                                         "sobremuestra", "bsw", "bsw_sen", "infrad", "morgana3", "misecf", 
                                         "mviolenf", "idsc", "idstrat", "ruralidad", "dqdit01", "dqdit02", 
                                         "dqdit03", "dqdit04", "dqdit05", "dqdit06", "dqdit07",paste0("dqdit24_",
                                                                                                      c(paste0(rep("0",9),1:9),10,11,12))),
                      vars_family3 = c("sID", "oID", "idgrade", "wgt", "wgt_sen", "isecf", "supervf", 
                                       "violenf", "subsgobf", "hrsestuf", "expectf", "informf", "prekfor6", 
                                       "inasclas", "padinmif", "madindig", "idst", "idsc", "dependencia", 
                                       "ruralidad", "idstrat", "genero", "edad", "idpl", "idpm", "idpc","dqfit09_01"),
                      vars_lteacher3 = c( "oID", "idtealin", "gradoasig", "coursename", "itcourse", 
                                          "bswl", "bswl_sen", "clambp", "relsalp", "monitop", "sobrem","dqpit11","dqpit11"),
                      vars_mteacher3 = c("idsc", "oID", "idstrat", "idgrade", "coursename", "dqpit11",
                                         "bswm", "bswm_sen"))


student3_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/3rd grade/QA3.dta")[variable_list[[1]]]
director3_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/3rd grade/QD3.dta")[variable_list[[2]]]
family3_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/3rd grade/QF3.dta")[variable_list[[3]]]
lteacher3_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/3rd grade/QP3Lengua.dta")[variable_list[[4]]]
mteacher3_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/3rd grade/QP3Matematica.dta")[variable_list[[5]]]
language_3 <-read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Logro de aprendizaje/Resultados Stata/3rd grade/PL3_all_TERCE.dta")
math_3 <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Logro de aprendizaje/Resultados Stata/3rd grade/PM3_all_TERCE.dta")

student6_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/6th grade/QA6.dta")
director6_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/6th grade/QD6.dta")
family6_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/6th grade/QF6.dta")
lteacher6_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/6th grade/QP6Lengua.dta")
mteacher6_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/6th grade/QP6Matematica.dta")
steacher6_data <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Factores asociados/Stata/6th grade/QP6Ciencia.dta")
science_6 <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Logro de aprendizaje/Resultados Stata/6th grade/PC6_all_TERCE.dta")
language_6 <-read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Logro de aprendizaje/Resultados Stata/6th grade/PL6_all_TERCE.dta")
math_6 <- read_dta("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Logro de aprendizaje/Resultados Stata/6th grade/PM6_all_TERCE.dta")


# Function updates the names of vector nam with the suffix char, excluding the elements
# excp
namer <- function(nam, char, excp) {
  charstr <- nam[!(nam %in% excp)]
  nam[!(nam %in% excp)] <- paste0(charstr, char)
  nam
}

merger <- function(..., suffix) {
  all_data <- list(...)
  
  all_data <- lapply(all_data, function(x) lapply(x, unclass))
  
  # Function loops over data frame all_data and suffix and changes the names of each dataframe
  # excluding names oID and sID
  all_data2 <- Map(function(x, y) setNames(x, namer(names(x), y, c("oID", "sID"))), all_data, suffix)
  
  # Function merges every element of the list
  all_data2 <- Reduce(function(x, y) merge(x, y, all = T), all_data2)
  
  cols <- lapply(all_data2$sID, substring, c(1, 4, 8, 10), c(3, 7, 9, 12))
  
  cols2 <- data.frame(do.call(rbind, cols))
  names(cols2) <- c("cntry", "school", "class", "student")
  
  all_data2 <- as.data.frame(append(all_data2, cols2, after = 0))
  all_data2
}

all_data3 <- merger(student3_data, director3_data, family3_data,
                    lteacher3_data, mteacher3_data, language_3, math_3,
                    suffix = c("_student", "_director", "_family",
                               "_lteacher", "_mteacher", "_language",
                               "_math"))

all_data6 <- merger(student6_data, director6_data, family6_data,
                    lteacher6_data, mteacher6_data, steacher6_data,
                    language_6, math_6,science_6,
                    suffix = c("_student", "_director", "_family",
                               "_lteacher", "_mteacher", "_steacher",
                               "_language", "_math", "_science"))

all_data <- full_join(all_data3, all_data6)
