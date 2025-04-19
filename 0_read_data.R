#===============================================================================
# Script to read soluble sugar concentrations from the Proctor Maple Research 
# Center Long-term Study
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("read_excel")) library("readxl")
if(!existsFunction("%>%")) library("tidyverse")

# Name of data file ----
file_name <- "../../Data/LTS Sugar.xlsx"

# Get sample dates from sheet names ----
dates <- excel_sheets(file_name) [-c(1:2)]

# Loop over sample dates to read data for each date ----
for (d in dates){
  
  # Read date-specific data ----
  tmp <- read_excel(
    path = file_name, 
    sheet = d, 
    range = "A9:H102",
    na = "NA",
    col_names = c("tn", "t", "tree", "ssc1", "ssc2", "ssc3", "ssc", "comments"),
    col_types = c("numeric", "text", "text", rep("numeric", 4), "text")
    ) %>% add_column (date = as_date(d))
  
  # Concatenate data ----
  if (d == dates[1]) {
    data <- tmp
  } else {
    data <- rbind (data, tmp)
  }
} # end date loop

# Correct dates for a few trees (see comments of data file for reasons) ----
data$date[data$date == as_date("2025-03-05") & data$tn == 3] <- 
  as_date("2025-03-06")
data$date[data$date == as_date("2025-03-19") & data$tn == 3] <- 
  as_date("2025-03-20")
trees <- c("871", "873", "874","877", "879", "880", "883", "884", "886", "887", 
           "888", "AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH")
data$date[data$date == as_date("2025-04-03") & data$tree %in% trees ] <- 
  as_date("2025-04-04")


#===============================================================================