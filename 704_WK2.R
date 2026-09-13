

# Cleans Messy Values

#CSV Import
library(readr)

messy <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/messy.csv")

head(enroll)


install.packages("dplyr") 
install.packages("stringr") 

library(dplyr)
library(stringr)

clean <- messy |>
  mutate(program =
           str_trim(program)) |>
 # mutate(gpa =
    #       na_if(gpa, 999)) |>   more then one error
  mutate(gpa = case_when(
    gpa == 999  ~ NA_real_,
    gpa == -99  ~ NA_real_,
    gpa == "."  ~ NA_real_,
    is.na(gpa)  ~ NA_real_,
    gpa == ""   ~ NA_real_,
    TRUE        ~ as.numeric(gpa)
  )) |>
  distinct(student_id,
           .keep_all = TRUE)

#Count Missing and Convert a Date
library(readr)

# enroll <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/enrollment.csv")

# head(enroll)

colSums(is.na(clean))

enroll <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/enrollment.csv")

enroll$enrolled_on <-
  as.Date(enroll$enrolled_on)

head(enroll)


#Filter in R
active <- clean |>
  filter(
    enrolled_status == "Active")

nrow(active)

#Excel import
install.packages("readxl")
library(readxl)

demo <- read_excel("/Users/ricwalter/Desktop/Miami/EPS704/WK1/institutional.xlsx",
                   sheet = "Demographics")

head(demo)

