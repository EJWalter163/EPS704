

#-----------------------------
#GITHUB EJWalter163/EPS704 
#------------------------

library(readr)                          # readr holds read_csv.

#install.packages("dplyr")
library(dplyr)                          # dplyr holds the cleaning verbs.

library(stringr)                        # stringr holds str_trim.

install.packages("tidyr")
library(tidyr)                     # dplyr holds the cleaning verbs.

#install.packages("ggplot2")
library(ggplot2)

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


# Histogram
ggplot(enroll2, aes(x = gpa)) +         
  geom_histogram(binwidth = 0.1) +      
  labs(title = "Distribution of GPA", x = "GPA", y = "Number of students")

#Scatter
ggplot(enroll2, aes(x = credits, y = gpa)) + geom_point()

#Box Plot
ggplot(enroll2,aes(x = degree_level, y = gpa)) + geom_boxplot()

#Bar chart
ggplot(enroll2, aes(x = program)) +  geom_bar()

#Colour by a group, split into small plots, and save the figure to a file
ggplot(enroll2, aes(x = credits, y = gpa, color = degree_level)) +
  geom_point() +  facet_wrap(~ program)

ggsave("gpa_plot.png")
