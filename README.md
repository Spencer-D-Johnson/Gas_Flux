This repository includes R code for processing, visualizing, and analyzing KBNERR/ACCS's beaver project gas flux data, including analyzing environmental covariates:

Clean_gas_data.R: Removes data with error messages from the raw .DATA output from the licor machine, and outputs a .csv file

gas.flux.start.end.time.plotting.R: Plots CO2 and CH4 concentrations together over time using combined licor data and 
seal/unseal times from a .csv file. User plots each chamber set individually and uses interactive plot to determine best 
measurement interval.

Gas_flux_calculations.R: Fills out gas flux data table with calculated values (flux values, environmental covariates, etc) and outputs completed .csv.

Gas_Flux_Data_Analysis_2.0.Rmd: R Markdown code plotting and modelling CO2 and CH4 flux (including photosynthetic flux and flux from respiration) using environmental covariates. Modelling performed using GAMs. Completed winter 2024-2025 - only uses 2024 data. 

Gas_Flux_Averages_for_Veg_Analysis.R: Calculates relevant gas flux averages by collar-year for use in vegetation ordinations 

LuxVsPAR_and_temp_correction_HOMET_2024and2025.Rmd: R Markdown code analyzing two experiments comparing Hobo temp/light sensor data at KBNERR's Homer Spit weather station to temperature and PAR measured at the station. Outputs an .RDA file saving a GAM modelling light intensity effects on temperature readings.

