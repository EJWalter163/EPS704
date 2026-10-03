

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


pp <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/Tables/pre_post_scores.csv")  
head (pp)

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


cor(enroll2$gpa, enroll$credits, use = "complete.obs")

cor.test(enroll2$gpa,   enroll$credits)

t.test(gpa ~ graduated,       data = enroll2)

model <- aov(gpa ~ program, data = enroll2)
summary(model)

tab <- table(enroll2$degree_level,
  enroll2$graduated)
chisq.test(tab)

model <- lm(gpa ~ credits,  data = enroll)
summary(model)

t.test(pp$post, pp$pre,
       paired = TRUE)

