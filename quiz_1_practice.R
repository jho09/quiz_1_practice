library(tidyverse)
library(apaTables)
library(haven)



# Load raw data
raw_data <- read_csv(file="lab_quiz_week2_data.csv")

# Inspect raw data
str(raw_data)

# Load data to make -999 a missing value
raw_data <- read_csv(file="lab_quiz_week2_data.csv",na=c("","NA","-999"))

# Create data set with categorical variables
categorical_variables <- select(raw_data,univ,prog_year)

# Convert categorical variables to factors
categorical_variables$prog_year <- as.factor(categorical_variables$univ)
levels(categorical_variables$prog_year) <- list("First Year"=1,"Second Year" = 2, "Third Year" = 3, "Fourth Year" = 4, "Grad School"=5)

categorical_variables$univ <- as.factor(categorical_variables$univ)
levels(categorical_variables$univ) <- list("Waterloo"=1,"Guelph"=2)

# Break scale items into separate data frames

age <- select(raw_data,age)
prog_sat_items <- select(raw_data,PS1,PS2,PS3,PS4,PS5)
dep_items <- select(raw_data,D1,D2,D3,D4,D5)
pos_affect_items <- select(raw_data,PA1,PA2,PA3,PA4,PA5)

# Check for out of range values
psych::describe(prog_sat_items)
psych::describe(dep_items)
psych::describe(pos_affect_items)

is_bad_value_prog_sat <- prog_sat_items<1 | prog_sat_items>6
prog_sat_items[is_bad_value_prog_sat] <- NA

is_bad_value_dep <- dep_items<1 | dep_items>4
dep_items[is_bad_value_dep] <- NA

is_bad_value_pos_affect <- pos_affect_items<1 | pos_affect_items>7
pos_affect_items[is_bad_value_pos_affect] <- NA

# Reverse score items

prog_sat_items <- mutate(prog_sat_items,PS1=7-PS1,PS2=7-PS2)
dep_items <- mutate(dep_items,D4=5-D4,D5=5-D5)
pos_affect_items <- mutate(pos_affect_items,PA1=8-PA1)

# Calculate mean scale scores

prog_sat <- psych::alpha(as.data.frame(prog_sat_items),check.keys = FALSE)$scores
dep <- psych::alpha(as.data.frame(dep_items),check.keys = FALSE)$scores
pos_affect <- psych::alpha(as.data.frame(pos_affect_items),check.keys = FALSE)$scores

# Combine everything into analytic_data

analytic_data <- cbind(categorical_variables,age,prog_sat,dep,pos_affect)

# Save analytic_data

save(analytic_data,file="practice1_analytic_data_practice.RData")
write_csv(analytic_data,path="practice1_analytic_data_practice.csv")
write_sav(analytic_data,path="practice1_analytic_data_practice.sav")


