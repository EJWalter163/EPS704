# =====================================================================
# EPS 704  |  Week 3  |  R  |  Data Cleaning and Filtering
# =====================================================================
#
# BEFORE YOU RUN THIS FILE: find the path to your data.
#   1. Put this file and the data file in the SAME folder.
#   2. In RStudio click:  Session > Set Working Directory > To Source File Location.
#   3. To see the folder R is using now, run:  getwd()
#   4. If your data sits elsewhere, replace the file name with a full path.
# ---------------------------------------------------------------------

#-----------------------------
#GITHUB EJWalter163/EPS704 
#------------------------

library(readr)                          # readr holds read_csv.

#install.packages("dplyr")
library(dplyr)                          # dplyr holds the cleaning verbs.
library(stringr)                        # stringr holds str_trim.
install.packages("tidyr")
library(tidyr)

# read the messy file into messy.
messy <-   read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/messy.csv")          
dim(messy)                              # note the starting row and column counts.

enroll <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/enrollment.csv")    
head(enroll)

scores <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/scores_wide.csv")    
dim(scores)

advisors <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/advisors.csv")  
dim(advisors)
head(advisors)

# ---- Clean the values -----------------------------------------------
clean <- enroll |>                                    # start a cleaning pipeline.
  mutate(program = str_trim(program)) |>             # remove extra spaces in program.
  mutate(gender = str_to_lower(gender)) |>           # lowercase gender for matching.
  mutate(gender = case_when(                         # recode spellings into two labels.
    gender %in% c("f", "female") ~ "Female",
    gender %in% c("m", "male")   ~ "Male",
    TRUE ~ gender)) |>
  mutate(gpa = na_if(gpa, 999)) |>                   # mark the missing code 999 as NA.
  distinct(student_id, .keep_all = TRUE)    

clean2<- clean |>
  mutate(gpa_group = case_when(
    gpa >= 3.5 ~ "High",
    gpa >= 3.0 ~ "Middle",
    TRUE ~ "Low"))

clean2 |>
  group_by(program) |>
  summarise(mean_gpa =
              mean(gpa, na.rm = TRUE),students = n()   

#----Reshape long
long <- scores |>
  pivot_longer(cols = c(midterm, final, project),
    names_to = "score_type",    values_to = "score")
head(long)

#----Reshape wide
wide <- long |>
  pivot_wider( names_from = score_type, values_from = score)
head(wide)

#Summary of columns GPA & Credits
clean2 |>
  group_by(program) |>
  summarise(across(c(gpa, credits), mean, na.rm = TRUE))  # average both columns.

#Join table
joined <- enroll |>
  left_join(advisors, by = "advisor_id") # add advisor details, matched on advisor_id.
head(joined)
