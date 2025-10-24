### Plotting Licor Data to Determine the Best Measurement Period ###
# Spencer Johnson
# Last update: 8/27/24

# Load tidyverse (ggplot, tidy data wrangling with dplyr, lubridate, etc)
library(tidyverse)
library(plotly) # Interactive plots
theme_set(theme_bw())

# Read in .csvs with licor data
gas.files = list.files(pattern = "licor*")
gas.data.separate = lapply(gas.files, read.csv)

# Combine into one table
gas = gas.data.separate[[1]]

for (i in 2:length(gas.data.separate)){
  gas = rbind(gas, gas.data.separate[[i]])
}

# Convert time column to "period" class that's easier to work with
gas$ptime = hms(gas$TIME)
head(gas$ptime)

# Test filtering
# test = gas %>% filter(DATE == "7/2/2024" & ptime >= "10H 3M 51S" & ptime <= "10H 10M 11S")

# Read in field data (*** check file name ***)
times = read.csv("Visit Level Data Master - 10-13-25 update.csv")
# Convert seal times to period class
times$ptime.seal = hms(times$Time_Seal)
times$ptime.unseal = hms(times$Time_Unseal)

# # Subset to a particular measurement interval
# Row = 165

## Graph each seal/unseal interval to determine the best measurement period
# Change sequence to i = 1:however many data points you want to look at
for (i in 1:54){
  
  # Ask user for row number
  Row = as.integer(readline("Row number (first line of data = 1):"))
  
  # Subset data to seal/unseal period
  gas.subset = gas %>% filter(DATE == times[Row, "Date"] & ptime >= times$ptime.seal[Row] & ptime <= times$ptime.unseal[Row]) %>% 
    select(CO2, CH4, TIME, ptime) %>% pivot_longer(c(CO2, CH4))
  
  # Print what measurement you're looking at
  print( paste(times[Row,1], times[Row,2], times[Row,3], sep = " ") )
  
  # Create interactive plot of CO2 and CH4 together
  print(ggplotly(
    ggplot(gas.subset, aes(x = as_datetime(hms(gas.subset$TIME)), y = value)) + geom_line() + facet_wrap(~name, ncol = 1, scales = "free_y") + 
      labs(x = "Time", y = "Concentration (PPM for CO2, PPB for CH4)")
    ))

}
-----------------------------------------

# # View light and dark together to compare slopes (example)
# gases.twm2D = gases %>% filter(ptime >= times$ptime.seal[48] & ptime <= times$ptime.unseal[48])
# ggplot(rbind(gases.twm2L, gases.twm2D), aes(x = ptime, y = CO2)) + geom_line()
# ggplot(rbind(gases.twm2L, gases.twm2D), aes(x = ptime, y = CH4)) + geom_line()

# # TWH3
# gases.twl3L = gases %>% filter(ptime >= times$ptime.seal[55] & ptime <= times$ptime.unseal[55])
# ggplot(gases.twl3L, aes(x = ptime, y = CO2)) + geom_line()
# ggplot(gases.twl3L, aes(x = ptime, y = CH4)) + geom_line()
# 
# gases.twl3D = gases %>% filter(ptime >= times$ptime.seal[56] & ptime <= times$ptime.unseal[56])
# ggplot(rbind(gases.twl3L, gases.twl3D), aes(x = ptime, y = CO2)) + geom_line()
# ggplot(rbind(gases.twl3L, gases.twl3D), aes(x = ptime, y = CH4)) + geom_line()
