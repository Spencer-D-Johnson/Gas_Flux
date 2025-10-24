# Read in the licor .data file, remove all datapoints with diagnostic errors, and write a .csv version
# Spencer Johnson
# 5/29/25

# Load packages
library(tidyverse)

# *** Change to your file name (minus extension) ***
filename = "licor_10-10-25_TW"  

# Read in tab-delimited data (besides first 5 lines in header)
gas = read.table(paste(filename, ".data", sep = ""), skip = 5, sep = "\t")
# Make column names the column names in the data frame, then delete column name and unit rows
colnames(gas) = gas[1,]
gas = gas[-c(1:2),]
# Convoluted code to make date format match current .csv files: convert to date class, 
# then change format to m/d/y with extra spaces for m and d, then remove any leading 0s
# with spaces in front, then remove any remaining spaces
gas$DATE = gsub(" ", "", gsub(" 0", "", format(as.Date(gas$DATE, "%Y-%m-%d"), " %m/ %d/%Y")))

# Filter out diagnostic errors (DIAG is sum of error codes (exponents of 2); 0 = no errors)
gas = gas %>% filter(DIAG == 0)
head(gas)

# Write .csv file with no ID column
write.csv(gas, paste(filename, ".csv", sep = ""), row.names = FALSE)
