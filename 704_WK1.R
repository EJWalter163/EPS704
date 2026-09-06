

#CSV Import
library(readr)

enroll <- read_csv("/Users/ricwalter/Desktop/Miami/EPS704/enrollment.csv")

head(enroll)


#Excel import
install.packages("readxl")
library(readxl)

demo <- read_excel("/Users/ricwalter/Desktop/Miami/EPS704/institutional.xlsx",
  sheet = "Demographics")

head(demo)

