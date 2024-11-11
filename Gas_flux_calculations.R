### Gas flux calculations - slopes, chamber volume, temp and lux averages, flux
# Spencer Johnson
# Last Update: 8/27/24

# Load packages
library(tidyverse)

# Read in master .csvs
visit = read.csv("Visit Level Data Master.csv")
collar = read.csv("Collar Level Data Master.csv")

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

# Convert measurement times to period class
visit$ptime.start = hms(visit$Time_Start_Measurement)
visit$ptime.end = hms(visit$Time_End_Measurement)
head(visit$ptime.start)
head(visit$ptime.end)

## Fill in start and end CO2 and CH4, slopes, 95% CIs for slopes
for (row in 1:nrow(visit)){
  gas.subset = gas %>% filter(DATE == visit[row, "Date"] & ptime >= visit$ptime.start[row] & ptime <= visit$ptime.end[row])
  
  visit[row, "CO2_Start_Measurement"] = gas.subset[1, "CO2"]
  visit[row, "CO2_End_Measurement"] = gas.subset[nrow(gas.subset), "CO2"]
  visit[row, "CH4_Start_Measurement"] = gas.subset[1, "CH4"]
  visit[row, "CH4_End_Measurement"] = gas.subset[nrow(gas.subset), "CH4"]
  
  mod.co2 = lm(CO2 ~ as.numeric(ptime), data = gas.subset)
  mod.ch4 = lm(CH4 ~ as.numeric(ptime), data = gas.subset)
  visit[row, "CO2_slope_ppm_s.1"] = mod.co2$coefficients[[2]]
  visit[row, "CH4_slope_ppb_s.1"] = mod.ch4$coefficients[[2]]
  
  ci.co2 = confint(mod.co2, "as.numeric(ptime)", level = 0.95)
  ci.ch4 = confint(mod.ch4, "as.numeric(ptime)", level = 0.95)
  visit[row, "CO2_lower_95CI"] = ci.co2[1]
  visit[row, "CO2_upper_95CI"] = ci.co2[2]
  visit[row, "CH4_lower_95CI"] = ci.ch4[1]
  visit[row, "CH4_upper_95CI"] = ci.ch4[2]
}

## Calculate chamber volume, water level
# join collar and visit level data
visit = visit %>% inner_join(collar, by = "CollarID")

for (row in 1:nrow(visit)){
  vol = 0.078169  # volume of just chamber in m^3
  vol = vol + visit[row, "Mean_Depth_cm"]*0.01*.114009  # add height of collar (in m) times basal area (m^2)
  if (visit$Extension_YN[row] == "Y"){
    vol = vol + 0.113372   # add extension volume (m^3) if applicable
  }
  standing.water = visit$Mean_Depth_cm[row] - visit$Piez_Water_Depth_cm[row] # calculate water level (+ if standing water)
  if (visit$Piez_Water_Depth_cm[row] < visit$Mean_Depth_cm[row]){  # Determine if there's standing water
    vol = vol - standing.water*0.01*.114009  # subtract height of standing water (in m) times basal area
  }
  visit[row, "Chamber_Volume_m3"] = vol
  visit[row, "Water_Level_cm"] = standing.water
}

## Read in and combine temp/lux data
templux.files = list.files(pattern = "chamber*")
templux.data.separate = lapply(templux.files, read.csv)

# Combine into one table
templux = templux.data.separate[[1]] %>% select(Date, Time_GMT.09.00, Temp_C, Intensity_Lux) %>% drop_na()

for (i in 2:length(templux.data.separate)){
  templux = rbind(templux, templux.data.separate[[i]] %>% select(Date, Time_GMT.09.00, Temp_C, Intensity_Lux) %>% drop_na())
}

templux$Intensity_Lux = as.numeric(templux$Intensity_Lux)  # make lux numeric (rather than character)

## Get average temp, lux for measurement periods
for (row in 1:nrow(visit)){
  tl.subset = templux %>% filter(Date == visit[row, "Date"] & as.numeric(hms(Time_GMT.09.00)) >= (as.numeric(visit[row, "ptime.start"]) + visit$Hobo_Licor_offset_s[row]) & as.numeric(hms(Time_GMT.09.00)) <= (as.numeric(visit[row, "ptime.end"]) + visit$Hobo_Licor_offset_s[row]))
  visit$Mean_Temp_C[row] = mean(tl.subset$Temp_C)
  visit$Mean_Lux[row] = format(mean(tl.subset$Intensity_Lux), scientific = FALSE)
}

## Get max ht and relative max ht for vegetation
for (row in 1:nrow(visit)){
  visit$Max_Ht_cm[row] = max(visit$Max_Ht_Herb_cm[row], visit$Max_Ht_Wood_cm[row], na.rm = TRUE)
}

june.hts = visit %>% filter(mdy(Date) < "2024-07-01") %>%
  group_by(CollarID) %>% 
  summarise(June_Max_Ht_cm = mean(Max_Ht_cm))

visit = visit %>% inner_join(june.hts, by = "CollarID")

for (row in 1:nrow(visit)){
  visit$Relative_Max_Ht_cm[row] = visit$Max_Ht_cm[row] - visit$June_Max_Ht_cm[row]
}

## Calculate days since thaw (thaw started on 5/2/24 on avg based on 7 working soil temp loggers; 
# most loggers (5) started before that, between 4/27 and 5/1, but 2 that are more shaded started 5/4 and 5/15)
for (row in 1:nrow(visit)){
  visit$Days_Since_Thaw[row] = yday(mdy(visit$Date))[row] - 123
}


## Calculate fluxes
for (row in 1:nrow(visit)){
  # Moles of gas in chamber: volume/(R*temp in K) per ideal gas law; pressure assumed to be 1 atm (could incorporate barologger data at some point)
  n = visit$Chamber_Volume_m3[row]/(8.2057*10^-5*(273.15+visit$Mean_Temp_C[row]))
  # Fluxes in micromoles per sq m per sec: slope * total moles of gas * area^(-1); extra conversion for CH4 to get from ppb to micromoles
  visit$CO2_flux_umol_m.2_sec.1[row] = format(visit$CO2_slope_ppm_s.1[row]*n/(0.114009), scientific = FALSE)
  visit$CH4_flux_umol_m.2_sec.1[row] = format(visit$CH4_slope_ppb_s.1[row]*n/(1000*0.114009), scientific = FALSE)
  # CO2 equiv emissions
  visit$Net_GHG_CO2_equiv_100yr[row] = as.numeric(visit$CO2_flux_umol_m.2_sec.1[row]) + as.numeric(visit$CH4_flux_umol_m.2_sec.1[row])*28.5
  visit$Net_GHG_CO2_equiv_20yr[row] = as.numeric(visit$CO2_flux_umol_m.2_sec.1[row]) + as.numeric(visit$CH4_flux_umol_m.2_sec.1[row])*82
}

## Export data
write.csv(visit %>% select(-c(ptime.start,ptime.end,Depth_2a_cm,Depth_2e_cm,Depth_4a_cm,Depth_4e_cm,Depth_3c_cm,Mean_Depth_cm,June_Max_Ht_cm,None_Top_Layer,Prop_Veg)),
          file = "Visit Level Data Master - 10-10-24 Update.csv")

