### Gas flux and covariate calculations:
# -Add year, put all start times in AKST
# -Calculate start and end CO2, CH4 for measurement periods; calculate slopes, 95% CIs for slopes
# -Calculate water levels; correct water depths of silty piezometers to 51.5 cm
# -Calculate chamber volumes
# -Calculate average light intensity and corrected temperature for each measurement period
# -Calculate max veg heights and changes since June
# -Select best atmospheric pressure for each measurement period
# -Calculate days since thaw for each measurement
# -Calculate CO2 and CH4 fluxes, CO2e emissions on 20 and 100 yr time frames

# Spencer Johnson
# Last Update: 10/24/25

# To fix: 

# Load packages
library(tidyverse)
library(mgcv)

# Read in master .csvs
visit = read.csv("Visit Level Data Master - 10-23-25 update.csv")
collar = read.csv("Collar Level Data Master.csv")

## Time adjustments
# Populate year column
visit$Year = year(as.POSIXct(visit$Date, format = "%m/%d/%Y"))
# Populate corrected start time (all in AKST - 2025 data collected in AKDT)
visit$Time_Start_Corrected = 
  c(as.POSIXct(paste(visit$Date[visit$Year == 2024], visit$Time_Start_Measurement[visit$Year == 2024]), format = "%m/%d/%Y %H:%M:%OS", tz = "ETC/GMT+9"),
    as.POSIXct(paste(visit$Date[visit$Year == 2025], visit$Time_Start_Measurement[visit$Year == 2025]), format = "%m/%d/%Y %H:%M:%OS", tz = "ETC/GMT+8"))

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

# Correct water levels for silty piezometers
visit$Piez_Water_Depth_cm[visit$Piez_Condition == "Silty"] = 51.5

# Select mean depth to ground for each collar during 4 measurement periods (all collars have depth to ground for at least June 2024, but many 
# were flooded during subsequent periods and were not measured from lip to sediment)
Collar_hts = collar %>% select(CollarID, Mean_Depth_cm_June24, Mean_Depth_cm_Oct24, Mean_Depth_cm_June25, Mean_Depth_cm_Oct25) %>%
  pivot_longer(c(Mean_Depth_cm_June24, Mean_Depth_cm_Oct24, Mean_Depth_cm_June25, Mean_Depth_cm_Oct25)) %>%
  mutate(name = factor(name, levels = c("Mean_Depth_cm_June24", "Mean_Depth_cm_Oct24", "Mean_Depth_cm_June25", "Mean_Depth_cm_Oct25")))
  
# Plot depths by collar
# ggplot(Collar_hts, aes(x = name, y = value)) +
#     geom_point() +
#     facet_wrap(~CollarID)

# Take average depth to ground for each collar: hard to tell how many changes over time are "real". For example, TMH1 likely wouldn't
# have moved enough to account for the June 2024 dip (and then moved back), and there's no reason to think TEH3 would have been further 
# underground in Oct 2024. There's probably some random variation in what exact points get sampled each time. Changes in TEL1 and TEM3 
# are also suspect - but it's plausible these collars got pushed further into the mud over the 2024-25 winter.
Final_hts = Collar_hts %>% group_by(CollarID) %>%
  summarise(Depth.to.ground = mean(value, na.rm = TRUE))

visit = visit %>% inner_join(Final_hts, by = "CollarID")
visit$Collar_to_Ground_cm = visit$Depth.to.ground

for (row in 1:nrow(visit)){
  vol = 0.078169  # volume of just chamber in m^3
  vol = vol + visit[row, "Collar_to_Ground_cm"]*0.01*.114009  # add height of collar (in m) times basal area (m^2)
  if (visit$Extension_YN[row] == "Y"){
    vol = vol + 0.113372   # add extension volume (m^3) if applicable
  }
  standing.water = visit$Collar_to_Ground_cm[row] - visit$Piez_Water_Depth_cm[row] # calculate water level (+ if standing water)
  if (visit$Piez_Water_Depth_cm[row] < visit$Collar_to_Ground_cm[row]){  # Determine if there's standing water
    vol = vol - standing.water*0.01*.114009  # subtract height of standing water (in m) times basal area
  }
  visit[row, "Chamber_Volume_m3"] = vol
  visit[row, "Water_Level_cm"] = standing.water
}

## Read in and combine temp/lux data
templux.files = list.files(pattern = "chamber*")
templux.data.separate = lapply(templux.files, read.csv)

# Combine into one table
templux = templux.data.separate[[1]] %>% 
  select(Date, starts_with("Time_GMT"), Temp_C, Intensity_Lux) %>% 
  drop_na() %>% 
  rename(Time = starts_with("Time_GMT"))

for (i in 2:length(templux.data.separate)){
  templux = rbind(templux, 
                  templux.data.separate[[i]] %>% 
                    select(Date, starts_with("Time_GMT"), Temp_C, Intensity_Lux) %>% 
                    drop_na() %>%
                    rename(Time = starts_with("Time_GMT")))
}

templux$Intensity_Lux = as.numeric(templux$Intensity_Lux)  # make lux numeric (rather than character)

# Correct chamber temperatures for light levels
temp.corr.gam = readRDS("Temp_Correction_GAM.rds")
templux$Temp_correction = predict(temp.corr.gam, newdata = data.frame(Intensity_Lux = templux$Intensity_Lux))
templux$Temp_C_corrected = templux$Temp_C - templux$Temp_correction

## Get average temp, lux for measurement periods
for (row in 1:nrow(visit)){
  if (visit$Date[row] == "10/3/2025"){
    if (visit$Light_Dark[row] == "Light"){  # Forgot light sensor on 10/3/25: took representative data afterward at specified times (converted to second of day)
      tl.subset = templux %>% filter(Date == "10/3/2025") %>% filter(as.numeric(hms(Time)) >= 59400) %>% filter(as.numeric(hms(Time)) <= 59640)
    }
    else { tl.subset = templux %>% filter(Date == "10/3/2025") %>% filter(as.numeric(hms(Time)) >= 59880) %>% filter(as.numeric(hms(Time)) <= 60240)
    }
  }
  else{  
    tl.subset = templux %>% filter(Date == visit[row, "Date"] & as.numeric(hms(Time)) >= (as.numeric(visit[row, "ptime.start"]) + visit$Hobo_Licor_offset_s[row]) & as.numeric(hms(Time)) <= (as.numeric(visit[row, "ptime.end"]) + visit$Hobo_Licor_offset_s[row]))
  }
  visit$Mean_Temp_C[row] = mean(tl.subset$Temp_C_corrected)
  visit$Mean_Lux[row] = format(mean(tl.subset$Intensity_Lux), scientific = FALSE)
}

## Get max ht and relative max ht for vegetation
for (row in 1:nrow(visit)){
  visit$Max_Ht_cm[row] = max(visit$Max_Ht_Herb_cm[row], visit$Max_Ht_Wood_cm[row], 0, na.rm = TRUE)
}

june.hts = visit %>% filter(yday(mdy(Date)) >= 153 & yday(mdy(Date)) <= 181) %>%  # filter measurements from June only
  group_by(CollarID, Year) %>% 
  summarise(June_Max_Ht_cm = mean(Max_Ht_cm)) # should just be combining light and dark (same height measurement)

visit = visit %>% inner_join(june.hts, by = c("CollarID", "Year"))

for (row in 1:nrow(visit)){
  visit$Relative_Max_Ht_cm[row] = visit$Max_Ht_cm[row] - visit$June_Max_Ht_cm[row]
}

## Get atm pressure for each measurement period
pressure = read.csv("Atm_Pressure.csv") %>% drop_na()
pressure$DateTimeStamp = force_tz(as.POSIXct(pressure$DateTimeStamp, format = "%m/%d/%Y %H:%M"), "ETC/GMT+9")
pressure$TimeNumeric = as.numeric(pressure$DateTimeStamp) # Converts to number of seconds since Jan 1, 1970
visit$TimeNumeric = as.numeric(visit$Time_Start_Corrected)
for (row in 1:nrow(visit)){
  t = visit$TimeNumeric[row]
  d = pressure$TimeNumeric - t
  p.row = which(abs(d) == min(abs(d), na.rm = TRUE))
  visit$Atm_Pressure_mb[row] = mean(pressure$Pressure_mb[p.row])
}

## Calculate days since thaw (last time temp reached 1 degree C; average of the 3 lower transect loggers)
# thaw2024 = mean(c(yday(mdy("4/30/2024")), yday(mdy("5/5/2024")), yday(mdy("5/7/2024"))))  # Day 125
# as.Date(thaw2024, origin = "2023-12-31")  # 5/4/24
# thaw2025 = mean(c(yday(mdy("4/12/2025")), yday(mdy("4/22/2025")), yday(mdy("4/23/2025"))))  # Day 109
# as.Date(102, origin = "2024-12-31") # 4/19/25; set origin to last day of 2024 to match output with yday function

for (row in 1:nrow(visit)){
  if (visit$Year[row] == 2024){
    visit$Days_Since_Thaw[row] = yday(mdy(visit$Date))[row] - 125 
  }
  if (visit$Year[row] == 2025){
    visit$Days_Since_Thaw[row] = yday(mdy(visit$Date))[row] - 109
  } 
}

## Calculate fluxes
for (row in 1:nrow(visit)){
  # Moles of gas in chamber: volume/(R*temp in K) per ideal gas law; R is 8.2057366*10^-5 m^3*atm*mol^-1*K^-1 times 1013.25027 mb/atm
  n = visit$Chamber_Volume_m3[row]*visit$Atm_Pressure_mb[row]/(0.083145*(273.15+visit$Mean_Temp_C[row]))
  # Fluxes in micromoles per sq m per sec: slope * total moles of gas * area^(-1); extra conversion for CH4 to get from ppb to micromoles
  visit$CO2_flux_umol_m.2_sec.1[row] = format(visit$CO2_slope_ppm_s.1[row]*n/(0.114009), scientific = FALSE)
  visit$CH4_flux_umol_m.2_sec.1[row] = format(visit$CH4_slope_ppb_s.1[row]*n/(1000*0.114009), scientific = FALSE)
  # CO2 equiv emissions
  visit$Net_GHG_CO2_equiv_100yr[row] = as.numeric(visit$CO2_flux_umol_m.2_sec.1[row]) + as.numeric(visit$CH4_flux_umol_m.2_sec.1[row])*28.5
  visit$Net_GHG_CO2_equiv_20yr[row] = as.numeric(visit$CO2_flux_umol_m.2_sec.1[row]) + as.numeric(visit$CH4_flux_umol_m.2_sec.1[row])*82
}

## Export data
write.csv(visit %>% select(-c(ptime.start,ptime.end,Depth.to.ground,June_Max_Ht_cm,TimeNumeric)),
          file = "Visit Level Data Master - 10-24-25 update.csv", row.names = FALSE)

