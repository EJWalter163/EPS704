

#-----------------------------
#GITHUB EJWalter163/EPS704 
#------------------------

library(readr)                          # readr holds read_csv.

#install.packages("dplyr")
library(dplyr)                          # dplyr holds the cleaning verbs.

library(stringr)                        # stringr holds str_trim.

install.packages("tidyr")
library(tidyr)                     # dplyr holds the cleaning verbs.

enroll <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/enrollment.csv")    
head(enroll)

# ---- Clean the values -----------------------------------------------
enroll2 <- enroll |>                                    # start a cleaning pipeline.
  mutate(program = str_trim(program)) |>             # remove extra spaces in program.
  mutate(gender = str_to_lower(gender)) |>           # lowercase gender for matching.
  mutate(gender = case_when(                         # recode spellings into two labels.
    gender %in% c("f", "female") ~ "Female",
    gender %in% c("m", "male")   ~ "Male",
    TRUE ~ gender)) |>
  mutate(gpa = na_if(gpa, 999)) |>                   # mark the missing code 999 as NA.
  distinct(student_id, .keep_all = TRUE)  

# ---- One-number summaries Ave, Spread, & five number summary---------------------------
mean(enroll2$gpa, na.rm = TRUE)          
sd(enroll2$gpa, na.rm = TRUE)           
summary(enroll2$gpa)                    

# ---- Counts and group summaries  in program-------------------------------------
table(enroll2$program)                  
enroll2 |> group_by(program) |>
  summarise(mean_gpa = mean(gpa, na.rm = TRUE), students = n())

# ---- Two way table (cross tab) --------------------------------------
table(enroll2$program, enroll2$degree_level)   # programs down, degree levels across.

# ---- Correlation matrix ---------------------------------------------
cor(enroll2[c("gpa", "credits", "gre_q", "gre_v")],   # several numbers at once.
    use = "complete.obs")               # use rows with all values present.
