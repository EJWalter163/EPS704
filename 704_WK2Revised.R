

# =====================================================================
# EPS 704  |  Week 2  |  R  |  Data Cleaning and Filtering
# =====================================================================
#
# BEFORE YOU RUN THIS FILE: find the path to your data.
#   1. Put this file and the data file in the SAME folder.
#   2. In RStudio click:  Session > Set Working Directory > To Source File Location.
#   3. To see the folder R is using now, run:  getwd()
#   4. If your data sits elsewhere, replace the file name with a full path.
# ---------------------------------------------------------------------
# Error Line 34 
#-----------------------------
#GITHUB EJWalter163/EPS704 
#------------------------

library(readr)                          # readr holds read_csv.
#install.packages("dplyr")

library(dplyr)                          # dplyr holds the cleaning verbs.
library(stringr)                        # stringr holds str_trim.

# read the messy file into messy.
messy <-   read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/messy.csv")          
dim(messy)                              # note the starting row and column counts.

# ---- Clean the values -----------------------------------------------
clean <- messy |>                                    # start a cleaning pipeline.
  mutate(program = str_trim(program)) |>             # remove extra spaces in program.
  mutate(gender = str_to_lower(gender)) |>           # lowercase gender for matching.
  mutate(gender = case_when(                         # recode spellings into two labels.
    gender %in% c("f", "female") ~ "Female",
    gender %in% c("m", "male")   ~ "Male",
    TRUE ~ gender)) |>
 # mutate(gpa = na_if(as.numeric(gpa), 999))|>
mutate(gpa = na_if(gpa, "999")) |>                   # mark the missing code 999 as NA.
  distinct(student_id, .keep_all = TRUE)             # drop duplicate students.

# ---- Count where values are missing ---------------------------------
colSums(is.na(clean))                   # count the missing values in each column.

# ---- Convert a text date to a real date (uses the clean enrollment file) ----
# read the clean enrollment file.
enroll <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/enrollment.csv")    
enroll$enrolled_on <- as.Date(enroll$enrolled_on)   # 2019-01-01 text becomes a real date.
class(enroll$enrolled_on)               # confirm the column is now a Date.

# ---- Filter to the rows you want ------------------------------------
active <- clean |>                                   # start from the cleaned table.
  filter(enrolled_status == "Active")                # keep only Active students.
dim(active)                             # compare the final counts with the start.