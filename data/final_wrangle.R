# Lauri-Pekka Aho
# 6.3.2017
# lauri-pekka.aho@helsinki.fi

# This data is collected from Italian secondary school students who study both Portuguese 
# and mathematics. 

# The students filled in a survey with questions which vary from studying time to alcohol use.

setwd("~/GitHub/IODS-final/data")

# Reading both datasets and exploring the dimensions and structures of these datasets

math <- read.csv("student-mat.csv", sep = ";")
str(math)
dim(math)

por <- read.csv("student-por.csv", sep = ";")
str(por)
dim(por)

# Access the dplyr-package

library(dplyr)

# Selecting the columns to use as identifiers

join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# Joining the two datasets using selected columns

math_por <- inner_join(math, por, by = join_by, suffix=c(".math", ".por"))
str(math_por)
dim(math_por)

# Creating a new data with only the joined columns

alc <- select(math_por, one_of(join_by))

# Selecting the columns which were not used for joining the data

notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# Combining "duplicate" answers

# For every column name not used for joining...
for(column_name in notjoined_columns) {
  # Select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # Select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # If that first column vector is numeric...
  if(is.numeric(first_column)) {
    # Take a rounded average of each row of the two columns and
    # Add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # Else if it's not numeric...
    # Add the first column vector to the alc data frame
    alc[column_name] <- select(two_columns, 1)[[1]]
  }
}

# Define a new column alc_use by combining weekday and weekend alcohol use

alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# Define a new logical column 'high_use'

alc <- mutate(alc, high_use = (alc_use > 2))

###################################

# The following are new changes

###################################

# Creating a new logical column 'parent_highEDU' which tells if both parents have
# at least education level of secondary education

alc <- mutate(alc, parent_highEDU = (Medu > 2 & Fedu > 2))

# Logical column 'parent_home"; both parents work at home.

alc <- mutate(alc, parent_home = (Mjob == "at_home" & Fjob == "at_home"))


# Glimpse at the newly created alc-data

glimpse(alc)

# Saving the file

write.csv(alc, "alc.csv")
