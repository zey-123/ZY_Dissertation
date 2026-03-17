# This script contains all the unused/trial codes that were ran in all the scripts for the dissertation
# If wanting to run the code to see outputs, paste into their respective sections.



# CTD_Handling - Unused ----

# Other methods of handling variable column numbers
# 1) Option 1: initial method - doesnt handle column numbers 
# Function to read and clean one CTD file
#read_and_extract_surface <- function(file) {
#  df <- read_excel(file, na = "-999", col_names = FALSE) # read without column names
#  colnames(df) <- col_names # Apply the same cleaning steps

#  df_surface <- df %>% #  Extract upper 200m data
#    filter(depth_m <= 200) %>%  # keep only upper/pelagic
#    select(cast_ID, decimal_year, latitude, longitude, depth_m, temperature_C) %>% # select relevant columns
#    arrange(desc(depth_m)) %>% # arrange by depth descending
#    na.omit() # remove rows with missing data

# Add filename (cast ID reference) just in case
#  df_surface$file_name <- basename(file)

# return(df_surface)
#}

# 3) Option 3: alternative method to handling variable columns by dropping extra date column if present - OBSERVATIONS 484321 (LESS THAN OPTION 2)
read_and_extract_surface <- function(file) {
  # Read without headers, treat -999 as NA
  df <- read_excel(file, na = "-999", col_names = FALSE)
  
  # Standard expected columns (13 core variables)
  expected_cols <- c(
    "cast_ID", "decimal_year", "latitude", "longitude",
    "pressure_dbar", "depth_m", "temperature_C", "conductivity_S_per_m",
    "salinity_PSS78", "dissolved_oxygen_umol_per_kg",
    "beam_attenuation_1_per_m", "fluorescence_RFU", "PAR_uE_per_m2_per_s" )
  
  # Sometimes there's an extra date column → 14 columns
  # Try to identify and drop it automatically
  if (ncol(df) > length(expected_cols)) {
    # Create a temporary name list with extra cols labeled as "extra"
    colnames(df) <- c(expected_cols, rep("extra", ncol(df) - length(expected_cols)))
    
    # Drop any column that looks like a date or extra
    df <- df %>% select(-matches("extra"))
  } else {
    colnames(df) <- expected_cols[1:ncol(df)]
  }
  
  # Now filter and tidy
  df_surface <- df %>%
    # Remove nonsensical values
    filter(!is.na(depth_m), !is.na(temperature_C)) %>%
    filter(depth_m <= 200) %>%
    select(any_of(c("cast_ID", "decimal_year", "latitude", "longitude",
                    "depth_m", "temperature_C"))) %>%
    arrange(desc(depth_m))
  
  # Add filename as identifier
  df_surface$file_name <- basename(file)
  
  return(df_surface)
}

# More visualizing using long, lat info and mapping
library(ggplot2)
library(maps)
# Get world map data
world_map <- map_data("world")
# Plot temperature data on world map for every year starting from 1988 to 2015 zooming into BATS region
ggplot(bats_temp_surface, aes(longitude, latitude)) +
  geom_point(aes(color = temperature_C)) +
  facet_wrap(~ Year) +
  scale_color_viridis_c() +
  coord_fixed(xlim = c(-65, -60), ylim = c(30, 35))

#visualising spatial temperature data over the years - world map with points colored by temperature
ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_point(data = bats_temp_surface,
             aes(x = longitude, y = latitude, color = temperature_C),
             alpha = 0.7) +
  scale_color_viridis_c(option = "plasma") +
  coord_fixed(xlim = c(-65, -60), ylim = c(30, 35)) +
  labs(title = "Sea Surface Temperature at BATS (1988–2016)",
       x = "Longitude", y = "Latitude", color = "Temp (°C)") +
  theme_minimal()


head(world_map)

bats_temp_surface$longitude <- as.numeric(bats_temp_surface$longitude)
bats_temp_surface$latitude <- as.numeric(bats_temp_surface$latitude)

range(bats_temp_surface$longitude)

ggplot(bats_temp_surface, aes(longitude, latitude)) +
  geom_point()+
  xlim(40, 100) +
  ylim(30, 40)+
  theme_classic()

bats_temp_surface <- bats_temp_surface |>
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude))
bats_temp_surface$longitude <- -bats_temp_surface$longitude


library(ggplot2)
library(maps)

world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map,
    aes(long, lat, group = group),
    fill = "grey90",color = "white",
    linewidth = 0.2) +
  geom_point(data = bats_temp_surface,
    aes(longitude, latitude),
    size = 1.3,
    alpha = 0.6) +
  coord_fixed( xlim = c(-90, -50),
    ylim = c(20, 45))+
  labs(title = "BATS CTD Sampling Locations",
    x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# Map with points colored by temperature
ggplot() +
  geom_polygon( data = world_map,
    aes(long, lat, group = group),
    fill = "grey85", color = "white",
    linewidth = 0.2) +
  geom_point( data = bats_temp_surface,
    aes(longitude, latitude, color = temperature_C), size = 1.5, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", name = "SST (°C)") +
  coord_fixed( xlim = c(-90, -50), ylim = c(20, 45))+
  labs(title = "Sea Surface Temperature at BATS", subtitle = "CTD surface observations (1988–2016)") +
  theme_classic(base_size = 12)+
  theme(panel.grid = element_blank())


### EXTRA
#standardizing dates all into proper date objects
#but first figuring out what the date entails 
min(bats_temp_surface$date)
max(bats_temp_surface$date)
summary(bats_temp_surface$date)
bats_temp_surface %>%
  mutate(length = nchar(as.character(date))) %>%
  count(length)

#now standardize
library(dplyr)
library(lubridate)
library(stringr)

bats_temp_surface <- bats_temp_surface %>%
  mutate( date_clean = case_when(
      nchar(as.character(date)) == 8 ~ as.Date(as.character(date), format = "%Y%m%d"), # YYYYMMDD
      nchar(as.character(date)) == 6 ~ as.Date(paste0(as.character(date), "01"), format = "%Y%m%d"), #YYYYMM
      (date > 0 & date < 100) ~ date_decimal(1988 + date),  # assume base year 1988 = Decimal year (e.g. 31.783) 
      str_detect(as.character(date), "\\.") ~ date_decimal(1988 + as.numeric(date)), #Long decimal version
      TRUE ~ as.Date(NA) #Everything else is missing
    ))
summary(bats_temp_surface$date_clean)
head(bats_temp_surface %>% select(date, date_clean))


##### Alternatively, extractin just the surface temperature (shallowest depth) from each cast
#b50049_surface_temps <- b50049_ctd %>%
#  group_by(cast_ID) %>%
#  slice_min(order_by = depth_m, n = 1) %>%
#  select(cast_ID, date, latitude, longitude, temperature_C,depth_m)
#view the surface temperature data
#View(b50049_surface_temps)

plot(b50049_ctd$temperature_C, b50049_ctd$depth_m, type="l", ylim=rev(range(b50049_ctd$depth_m)),
     xlab="Temperature (°C)", ylab="Depth (m)",
     main="CTD Temperature Profile - BATS Cast 49")




# Method2Indices - Unused ----
#N/A


#PhenologicalIndiciesCalc - Unused ----

####Phytoplankton Bloom Start ----
#perform timeseries smoothing using local plynomial regression fitting method to have this as main cornflowerblue trend with a faded light blue in the background showing average 
ggplot(bloom_start_phytoplankton %>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point() +
  geom_smooth(method = "loess", color = "cornflowerblue", se = TRUE) + # loess is a local polynomial regression fitting method that can capture non-linear trends in the data. It will create a smooth curve that follows the general pattern of the data points, while also showing the confidence interval (shaded area) around the trend line.
  labs(title = "Phytoplankton Bloom Start Day with Loess Smoothing",
       x = "Year",y = "Bloom Start Day of Year") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()

# Median based threshold - year/day where Chl levels first rise a small threshold above median values (Siegel 2002) - could be an alternative method for phytoplankton bloom start detection.
calculate_bloom_start_phyto_medianthresh <- function(data, frac = 0.5, duration = 3) { # frac: fraction above median to define threshold; duration: number of consecutive days
  data %>%
    mutate(Year = as.numeric(format(Date, "%Y")), 
           DayOfYear = as.numeric(format(Date, "%j"))) %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(MedianBiomass = median(Biomass, na.rm = TRUE), #Calculate median biomass for the year
           Threshold = MedianBiomass * (1 + frac), #Define threshold as a fraction above median
           AboveThreshold = Biomass > Threshold, #Identify days above threshold
           RunID = cumsum(AboveThreshold != lag(AboveThreshold, default = first(AboveThreshold))) # Create run ID for consecutive above-threshold days
    ) %>%
    group_by(Year, RunID) %>% #group by year and run ID
    mutate(RunLength = n()) %>% #Calculate length of each run
    ungroup() %>%
    filter(AboveThreshold, RunLength >= duration) %>% #Filter for valid bloom periods
    group_by(Year) %>% #Get first day of bloom each year
    slice(1) %>%
    select(Year, BloomStartDay = DayOfYear) %>%
    ungroup()
}
bloom_start_phyto_medianthresh <- calculate_bloom_start_phyto_medianthresh(norm_chl %>% rename(Date = Date, Biomass = Chl_mean_200m_mg_m3))

#visualize
ggplot(bloom_start_phyto_medianthresh, aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "lightblue") +
  geom_point() +
  geom_smooth(method = "lm", color = "lightblue", linetype = "dashed",se = FALSE) +
  labs(title = "Phytoplankton Bloom Start Day (Median Threshold Method) Over Years",
       x = "Year",
       y = "Bloom Start Day of Year") +
  theme_classic()


### Alternative Phytoplankton: Year day of maximum instantaneous growth rate within a defined period; used for indexing phytoplankton.
# Function to calculate bloom start date based on maximum growth rate for phytoplankton
calculate_bloom_start_growth_rate_phytoplankton <- function(data) {
  data %>%
    mutate(Year = as.numeric(format(Date, "%Y")),
           DayOfYear = as.numeric(format(Date, "%j"))) %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(GrowthRate = (Biomass - lag(Biomass)) / lag(Biomass)) %>%
    filter(!is.na(GrowthRate), is.finite(GrowthRate)) %>%  # safety filter
    slice_max(GrowthRate, n = 1, with_ties = FALSE) %>%
    select(Year, BloomStartDay = DayOfYear) %>%
    ungroup()
}

bloom_start_growth_rate_phytoplankton <- calculate_bloom_start_growth_rate_phytoplankton(norm_chl %>% rename(Date = Date, Biomass = Chl_mean_200m_mg_m3))

#visualize
ggplot(bloom_start_growth_rate_phytoplankton, aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  geom_smooth(method = "lm", color = "darkgreen", linetype = "dashed",se = FALSE) +
  labs(title = "Phytoplankton Bloom Start Day (Growth Rate Method) Over Years",
       x = "Year",
       y = "Bloom Start Day of Year") +
  theme_classic()





# PhytoplanktonHandling - Unused ----
# TESTING OTHER APPROACHES
### Step 1: Interpolation to a Standard Depth Grid (every 5 m from 0 m to 200 m.)-

# Group by unique depths and average chlorophyll values if duplicates exist

BATS_chla <- BATS_chla %>%
  group_by(yyyymmd, Depth) %>%
  summarise(Chl = mean(Chl, na.rm = TRUE)) %>%
  ungroup()

# Create a standard depth grid
standard_depths <- seq(0, 200, by = 5) # Standard depth grid from 0 to 200 m at 5 m intervals
Chl <- BATS_chla$Chl
Depth <- BATS_chla$Depth

interp_chl <- approx(Depth, Chl, xout=standard_depths, rule=2)$y  # rule=2 => use boundary values for extrapolation

# linear interpolation
interp_chl <- approx(depth, chl_ug_L, xout=std_grid, rule=2)$y  # rule=2 => use boundary values for extrapolation

# convert µg L^-1 to µg m^-3
chl_ug_m3 <- interp_chl * 1000

# integrate 0-200 m (gives µg m^-2)
chl_ug_m2 <- trapz(std_grid, chl_ug_m3)
chl_mg_m2 <- chl_ug_m2 / 1000

# if cast only to 150 m, option A scale, option B extend with last value
Zmax <- max(depth)
# option A: scale
integral_to_Zmax <- trapz(seq(0,Zmax,by=1), approx(depth, chl_ug_L*1000, xout=seq(0,Zmax,by=1))$y)
scaled_integral_ug_m2 <- integral_to_Zmax * (200 / Zmax)

# option B: extend using last measured value down to 200 m
ext_depths <- c(depth,200)
ext_chl <- c(chl_ug_L, tail(chl_ug_L,1))
ext_interp <- approx(ext_depths, ext_chl, xout=std_grid)$y * 1000
extended_integral_ug_m2 <- trapz(std_grid, ext_interp)


#convert Depth_m and Chl_a_ug_per_L to numeric
BATS_chla$Depth <- as.numeric(BATS_chla$Depth)
BATS_chla$Chl <- as.numeric(BATS_chla$Chl)

#remove rows with NA values in Depth or Chl
BATS_chla <- BATS_chla %>%
  filter(!is.na(Depth) & !is.na(Chl))


# Trying stuff out 
### Step 1: Decide on reference depth as 200m and Interpolate each cast onto a standard depth grid. -
standard_depths <- seq(0, 200, by = 1) # Standard depth grid from 0 to 200 m at 1 m intervals
BATS_chla_interpolated <- BATS_chla %>%
  group_by(yyyymmd) %>%
  do({
    interp_chl <- approx(.$Depth, .$Chl, xout = standard_depths, rule = 2)$y
    data.frame(Depth = standard_depths, Chl = interp_chl)
  }) %>%
  ungroup()
View(BATS_chla_interpolated)


### Step 2: Normalize either: By depth-integrated chlorophyll (average over layer) OR Z-score per depth OR Relative to total chlorophyll in upper layer -
BATS_chla_normalized <- BATS_chla_interpolated %>%
  group_by(yyyymmd) %>%
  mutate(Total_Chl = sum(Chl, na.rm = TRUE),
         Chl_Normalized = Chl / Total_Chl) %>%
  ungroup()
View(BATS_chla_normalized)

### Step 3: Visualize time-series at specific depths or depth-averaged -
# Example: Time-series at 10m depth
chl_10m <- BATS_chla_normalized %>%
  filter(Depth == 10)
plot(as.Date(chl_10m$yyyymmd, format="%Y%m%d"), chl_10m$Chl_Normalized, type="l",
     xlab="Date", ylab="Normalized Chlorophyll a at 10m",
     main="Time-series of Normalized Chlorophyll a at 10m Depth")
# Example: Depth-averaged normalized chlorophyll
chl_depth_avg <- BATS_chla_normalized %>%
  group_by(yyyymmd) %>%
  summarise(Depth_Avg_Chl = mean(Chl_Normalized, na.rm = TRUE)) %>%
  ungroup()
plot(as.Date(chl_depth_avg$yyyymmd, format="%Y%m%d"), chl_depth_avg$Depth_Avg_Chl, type="l",
     xlab="Date", ylab="Depth-averaged Normalized Chlorophyll a",
     main="Time-series of Depth-averaged Normalized Chlorophyll a")

### Step 4: Seasonal pattern analysis-

### Depth Integration (z-score) 
# calculating the mean - Sum all the chlorophyll values in the BATS_chla dataset and divide by the number of data points.
(mean_chla <- mean(BATS_chla$Chl, na.rm = TRUE))
# calculating the standard deviation - Calculate the standard deviation of the chlorophyll values in the BATS_chla dataset.
(sd_chla <- sd(BATS_chla$Chl, na.rm = TRUE))

# calculating the z-score - For each chlorophyll value, subtract the mean and divide by the standard deviation.
BATS_chla <- BATS_chla %>%
  mutate(Chl_zscore = (Chl - mean_chla) / sd_chla)
View(BATS_chla)

#order dataset by depth
BATS_chla <- BATS_chla %>%
  arrange(Depth)
View(BATS_chla)

### Vertical integration
### Compute depth-integrated chlorophyll by integration - zero as bottom of integration 200 as top Chl(z) dz then normalize by dividing integration depth to get an avg
BATS_chla_depth_integrated <- BATS_chla %>%
  group_by(yyyymmd) %>% # group by date
  summarise(Depth_Integrated_Chl = sum(Chl, na.rm = TRUE) * (200 / n())) %>% # integrate and normalize
  ungroup() # ungroup the data
View(BATS_chla_depth_integrated)





# StatisticalModelling - Unused ----
#N/A

#Stratification_CTD - Unused ----
#N/A

#ZooplanktonData - Unused ----
#N/A