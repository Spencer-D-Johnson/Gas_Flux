# Calculate gas flux averages for veg analyses
# Spencer Johnson
# 10/24/25

# Load packages
library(tidyverse)

# Load data
visit = read.csv("Visit Level Data Master - 10-24-25 update.csv")

# Convert categorical variables to factors
visit = visit %>% filter(Flag != "Extra" & Flag != "Extra_Pair") # Get rid of extra measurements
visit$CollarID = as.factor(visit$CollarID)
visit$Site = as.factor(visit$Site)

# Separate out light measurements
light = visit %>% 
  filter(Light_Dark == "Light")
# Separate out dark measurements
dark = visit %>% 
  filter(Light_Dark == "Dark")
# Calculate photosynthesis by subtracting dark from light for corresponding measurements (assumes respiration rate is constant between light and dark measurements)
photo = data.frame(CollarID = light$CollarID, 
                   Date = light$Date,
                   Site = light$Site,
                   Photosynthesis_umolCO2_m.2_s.1 = light$CO2_flux_umol_m.2_sec.1 - dark$CO2_flux_umol_m.2_sec.1)

photo_join = light %>% inner_join(photo, by = c("CollarID", "Date", "Site"))
photo_join$CollarID = as.factor(photo_join$CollarID)
photo_join$Site = as.factor(photo_join$Site)
# Add in dark measurements, CH4 averages
photo_join$Dark_CO2_flux_umol_m.2_sec.1 = dark$CO2_flux_umol_m.2_sec.1
photo_join$CH4_mean_flux_umol_m.2_sec.1 = (light$CH4_flux_umol_m.2_sec.1 + dark$CH4_flux_umol_m.2_sec.1)/2

# Convert date to DateTime class
photo_join$Date = as.POSIXct(photo_join$Date, format = "%m/%d/%Y")

# Calculate global averages
avg = photo_join %>% group_by(CollarID, Year) %>%
  summarise(CO2_mean = mean(CO2_flux_umol_m.2_sec.1),
            Photosynthesis_mean = mean(Photosynthesis_umolCO2_m.2_s.1),
            Respiration_mean = mean(Dark_CO2_flux_umol_m.2_sec.1),
            CH4_mean = mean(CH4_mean_flux_umol_m.2_sec.1),
            Water_Level_mean = mean(Water_Level_cm),
            Air_Temp_mean = mean(Mean_Temp_C),
            Soil_Temp_mean = mean(Soil_Temp_C),
            Light_Intensity_mean = mean(Mean_Lux))

# Calculate averages before dam
avg_beforeDam = photo_join %>% filter((Year == 2024) & (month(Date) %in% c(6, 7))) %>%
  group_by(CollarID, Year) %>%
  summarise(CO2_mean_beforeDam = mean(CO2_flux_umol_m.2_sec.1),
            Photosynthesis_mean_beforeDam = mean(Photosynthesis_umolCO2_m.2_s.1),
            Respiration_mean_beforeDam = mean(Dark_CO2_flux_umol_m.2_sec.1),
            CH4_mean_beforeDam = mean(CH4_mean_flux_umol_m.2_sec.1),
            Water_Level_mean_beforeDam = mean(Water_Level_cm))

# Calculate averages after dam
avg_afterDam = photo_join %>% filter(!((month(Date) %in% c(6, 7)) & (Year == 2024))) %>%
  group_by(CollarID, Year) %>%
  summarise(CO2_mean_afterDam = mean(CO2_flux_umol_m.2_sec.1),
            Photosynthesis_mean_afterDam = mean(Photosynthesis_umolCO2_m.2_s.1),
            Respiration_mean_afterDam = mean(Dark_CO2_flux_umol_m.2_sec.1),
            CH4_mean_afterDam = mean(CH4_mean_flux_umol_m.2_sec.1),
            Water_Level_mean_afterDam = mean(Water_Level_cm))

# Calculate summer averages (July - Aug 15th)
avg_summer = photo_join %>% filter((month(Date) == 7) | 
                                   (month(Date) == 8 & mday(Date) <= 15)) %>%
  group_by(CollarID, Year) %>%
  summarise(CO2_mean_summer = mean(CO2_flux_umol_m.2_sec.1),
            Photosynthesis_mean_summer = mean(Photosynthesis_umolCO2_m.2_s.1),
            Respiration_mean_summer = mean(Dark_CO2_flux_umol_m.2_sec.1),
            CH4_mean_summer = mean(CH4_mean_flux_umol_m.2_sec.1),
            Water_Level_mean_summer = mean(Water_Level_cm),
            Air_Temp_mean_summer = mean(Mean_Temp_C),
            Soil_Temp_mean_summer = mean(Soil_Temp_C),
            Light_Intensity_mean_summer = mean(Mean_Lux))

avg_combined = avg %>% inner_join(avg_summer, by = c("CollarID", "Year")) %>%
  full_join(avg_beforeDam, by = c("CollarID", "Year")) %>%
  inner_join(avg_afterDam, by = c("CollarID", "Year")) %>%
  mutate(Collar_Year = paste(CollarID, Year, sep = "_"))

avg_combined = avg_combined[,c(1:2,29,3:28)]

# Write .csv file with no ID column
write.csv(avg_combined, "Gas_Flux_Avgs.csv", row.names = FALSE)
