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


#PhenologicalIndiciesCalc - Unused ----


# PhytoplanktonHandling - Unused ----


# StatisticalModelling - Unused ----


#Stratification_CTD - Unused ----


#ZooplanktonData - Unused ----