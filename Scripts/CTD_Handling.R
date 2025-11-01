#import zip file for CTD data from downloads called "Excel"
unzip("Downloads/Excel.zip", exdir = "Data/CTD_Raw")
#list files in the unzipped folder

library(readxl)
b50049_ctd <- read_excel("Data/CTD/b50049_ctd.xls",na="-999")
View(b50049_ctd)


#add column names in the following order: "cast_ID" , "decimal_year", "date", "latitude", "longitude", "pressure_dbar", "depth_m", "temperature_C", "conductivity_S_per_m", "salinity_PSS78", "dissolved_oxygen_umol_per_kg", "beam_attenuation_1_per_m",  "fluorescence_RFU",  "PAR_uE_per_m2_per_s" 	
colnames(b50049_ctd) <- c("cast_ID" , "decimal_year", "date", "latitude", "longitude", "pressure_dbar", "depth_m", "temperature_C", "conductivity_S_per_m", "salinity_PSS78", "dissolved_oxygen_umol_per_kg", "beam_attenuation_1_per_m",  "fluorescence_RFU",  "PAR_uE_per_m2_per_s" )
#view the data
View(b50049_ctd)

#Extract first/surface temperature measurements (upper 200m) (as only interested in upper/pelagic) from each cast, removing the unnecessary columns and missing data 
library(dplyr)
b50049_ctd_surface <- b50049_ctd %>%
  filter(depth_m <= 200) %>%
  select(cast_ID, date, latitude, longitude, depth_m, temperature_C, salinity_PSS78, dissolved_oxygen_umol_per_kg) %>%
  na.omit()
#view the surface data
View(b50049_ctd_surface)

##### Alternatively, extract just the surface temperature (shallowest depth) from each cast
b50049_surface_temps <- b50049_ctd %>%
  group_by(cast_ID) %>%
  slice_min(order_by = depth_m, n = 1) %>%
  select(cast_ID, date, latitude, longitude, temperature_C)
#view the surface temperature data
View(b50049_surface_temps)


plot(b50049_ctd$temperature_C, b50049_ctd$depth_m, type="l", ylim=rev(range(b50049_ctd$depth_m)),
     xlab="Temperature (°C)", ylab="Depth (m)",
     main="CTD Temperature Profile - BATS Cast 49")

