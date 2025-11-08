##### Example of reading in and handling CTD data from BATS cast 49 ----
library(readxl)
b50049_ctd <- read_excel("Data/CTD/b50049_ctd.xls",na="-999")
View(b50049_ctd)

#add column names in the following order: "cast_ID" , "decimal_year", "latitude", "longitude", "pressure_dbar", "depth_m", "temperature_C", "conductivity_S_per_m", "salinity_PSS78", "dissolved_oxygen_umol_per_kg", "beam_attenuation_1_per_m",  "fluorescence_RFU",  "PAR_uE_per_m2_per_s"
colnames(b50049_ctd) <- c("cast_ID" , "decimal_year", "latitude", "longitude", "pressure_dbar", "depth_m", "temperature_C", "conductivity_S_per_m", "salinity_PSS78", "dissolved_oxygen_umol_per_kg", "beam_attenuation_1_per_m",  "fluorescence_RFU",  "PAR_uE_per_m2_per_s" )
#view the data
View(b50049_ctd)

#Extract first/surface temperature measurements (upper 200m) (as only interested in upper/pelagic) from each cast, removing the unnecessary columns and missing data
library(dplyr)
b50049_ctd_surface <- b50049_ctd %>%
  filter(depth_m <= 200) %>%
  select(cast_ID, decimal_year, latitude, longitude, depth_m, temperature_C) %>%
  arrange(desc(depth_m)) %>%
  na.omit()
#view the surface data
View(b50049_ctd_surface)

#Visualizing temperature profile for cast 49
plot(b50049_ctd_surface$temperature_C, b50049_ctd_surface$depth_m, type="l", ylim=rev(range(b50049_ctd_surface$depth_m)),
     xlab="Temperature (°C)", ylab="Depth (m)",
     main="CTD Temperature Profile - BATS Cast 49")

#plot(b50049_ctd_surface$temperature_C, b50049_ctd_surface$pressure_dbar, type="l", ylim=rev(range(b50049_ctd_surface$pressure_dbar)),
 #    xlab="Temperature (°C)", ylab="Pressure (dbar)",
 #    main="CTD Temperature Profile - BATS Cast 49")

#plotting depth against pressure 
#plot(b50049_ctd_surface$depth_m, b50049_ctd_surface$pressure_dbar,
#     xlab="Depth (m)", ylab="Pressure (dbar)",
#     main="Depth vs Pressure - BATS Cast 49")


b50049_ctd_surface %>%
  select(pressure_dbar, depth_m) %>%
  arrange(pressure_dbar) %>%
  head()



##### Combining all CTD ----
# 1 looking at the zip file path
# NOTE: Place Excel.zip in  Downloads folder or update the path below if I change this 

# Define the path to  zip file
zip_path <- "/Users/zeynepyuksel/Downloads/Excel.zip"

# Check that R can see it
file.exists(zip_path)

# Create a temporary or target folder to extract into
unzip_dir <- "data_local"
# Unzip it
unzip(zipfile = zip_path, exdir = unzip_dir)
# List the files extracted
list.files(unzip_dir)

#2 combining all the CTD files into one data frame - Reads all CTD .xls files in your folder, Applies the exact same cleaning steps to each, and Combines them into one large data frame (e.g. bats_temp_surface).
library(readxl)
library(dplyr)

# Define the folder path where all your CTD files are stored
path <- "data_local"

# List all .xls files
ctd_files <- list.files(path, pattern = "_ctd\\.xls$", full.names = TRUE)

# Define the column names once
col_names <- c(
  "cast_ID", "decimal_year", "latitude", "longitude",
  "pressure_dbar", "depth_m", "temperature_C", "conductivity_S_per_m",
  "salinity_PSS78", "dissolved_oxygen_umol_per_kg",
  "beam_attenuation_1_per_m", "fluorescence_RFU", "PAR_uE_per_m2_per_s"
)

# Other methods of handling variable column numbers----
# 1) initial method - doesnt handle column numbers 
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

# 2) Alternative method handling 13 or 14 columns - OBSERVATIONS 484447 (MORE THAN OPTION 3)
read_and_extract_surface <- function(file) {
   df <- read_excel(file, na = "-999", col_names = FALSE)

  # Count number of columns in this file
   n_cols <- ncol(df)
  
  # Define both possible name sets
  col_names_13 <- c("cast_ID", "decimal_year", "latitude", "longitude",
                    "pressure_dbar", "depth_m", "temperature_C",
                    "conductivity_S_per_m", "salinity_PSS78",
                    "dissolved_oxygen_umol_per_kg", "beam_attenuation_1_per_m",
                    "fluorescence_RFU", "PAR_uE_per_m2_per_s")
  
  col_names_14 <- c("cast_ID", "decimal_year", "date", "latitude", "longitude",
                    "pressure_dbar", "depth_m", "temperature_C",
                     "conductivity_S_per_m", "salinity_PSS78",
                     "dissolved_oxygen_umol_per_kg", "beam_attenuation_1_per_m",
                    "fluorescence_RFU", "PAR_uE_per_m2_per_s")
  
  # Apply appropriate column names
  if (n_cols == 13) {
    colnames(df) <- col_names_13
  } else if (n_cols == 14) {
    colnames(df) <- col_names_14
  } else {
    warning(paste("Unexpected number of columns in file:", basename(file)))
    return(NULL)
  }
  
  # Continue cleaning — works for both
  df_surface <- df %>%
    filter(depth_m <= 200) %>%
    select(any_of(c("cast_ID", "decimal_year", "latitude", "longitude",
                    "depth_m", "temperature_C"))) %>%
    arrange(desc(depth_m)) %>%
    na.omit()
  
  df_surface$file_name <- basename(file)
  return(df_surface)
}

##################################### 
# 3) Option 3 - alternative method to handling variable columns by dropping extra date column if present - OBSERVATIONS 484321 (LESS THAN OPTION 2)
read_and_extract_surface <- function(file) {
  # Read without headers, treat -999 as NA
  df <- read_excel(file, na = "-999", col_names = FALSE)
  
  # Standard expected columns (13 core variables)
  expected_cols <- c(
    "cast_ID", "decimal_year", "latitude", "longitude",
    "pressure_dbar", "depth_m", "temperature_C", "conductivity_S_per_m",
    "salinity_PSS78", "dissolved_oxygen_umol_per_kg",
    "beam_attenuation_1_per_m", "fluorescence_RFU", "PAR_uE_per_m2_per_s"
  )
  
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

############################################
#** Apply the function to every CTD file
bats_temp_surface <- lapply(ctd_files, read_and_extract_surface)

# Combine into one big data frame
bats_temp_surface <- bind_rows(bats_temp_surface)

# Inspecting new dataset - bats_temp_surface and checking everything is correct ----
View(bats_temp_surface)
summary(bats_temp_surface$depth_m) # checking range
#Minimum = 1.97 (surface) and Maximum ≤ 200 = 199.68, so no depth >200 and the filtering worked YAY
length(unique(bats_temp_surface$cast_ID)) # checking number of unique casts combined
# 5006

bats_temp_surface %>%
  count(cast_ID) %>%
  arrange(desc(n)) %>%
  head(10)
# shows number of measurements per cast_ID, top 10 casts with most measurements
length(unique(bats_temp_surface$file_name)) # 447 - this is the number of individual files loaded and is correct YAY




#### Visualizing general temperature trends ----


# ----
# Visualizing average surface temperature over time
library(ggplot2)

bats_temp_yearly <- bats_temp_surface %>%
  group_by(year = floor(decimal_year)) %>%
  #filter(year < 2016) %>% #removing 2016 as it appears to be an outlier? 
  summarise(mean_temp = mean(temperature_C, na.rm = TRUE))

ggplot(bats_temp_yearly, aes(x = year, y = mean_temp)) +
  geom_line(color = "darkgreen") +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "forestgreen", linetype = "dashed") +
  labs(x = "Year", y = "Mean Temperature (°C)",
       title = "Annual Mean Surface Temperature at BATS Over Time") +
  theme_classic()

#look at the range for temperature data
summary(bats_temp_surface$temperature_C)

bats_temp_surface %>%
  group_by(file_name) %>%
  summarise(min_T = min(temperature_C, na.rm = TRUE),
            max_T = max(temperature_C, na.rm = TRUE))

summary(bats_temp_surface$decimal_year)
range(bats_temp_surface$decimal_year)
hist(bats_temp_surface$temperature_C, breaks = 50, main = "Temperature Distribution", xlab = "Temperature (°C)")


#visualizing relationship between depth and temperature averaged across years by using bins of depth----
bats_temp_depth <- bats_temp_surface %>%
  group_by(depth_bin = cut(depth_m, breaks = seq(0, 200, by = 10))) %>% # create depth bins of 10m intervals
  summarise(mean_temp = mean(temperature_C, na.rm = TRUE), # calculate mean temperature for each depth bin
            mid_depth = mean(as.numeric(sub("\\((.+),(.+)\\]", "\\1", depth_bin)) + 5)) # calculate mid-point of each bin

ggplot(bats_temp_depth, aes(x = mean_temp, y = mid_depth)) +
  geom_line(color = "blue") +
  geom_point() +
  scale_y_reverse() + # reverse y-axis to have depth increasing downwards
  labs(x = "Mean Temperature (°C)", y = "Depth (m)",
       title = "Average Temperature Profile by Depth at BATS") +
  theme_classic()

#visualizing temperature with depth over time using a heatmap----
bats_temp_surface <- bats_temp_surface %>%
  mutate(year = floor(decimal_year),
         depth_bin = cut(depth_m, breaks = seq(0, 200, by = 10)))
bats_temp_heatmap <- bats_temp_surface %>%
  group_by(year, depth_bin) %>%
  summarise(mean_temp = mean(temperature_C, na.rm = TRUE),
            mid_depth = mean(as.numeric(sub("\\((.+),(.+)\\]", "\\1", depth_bin)) + 5)) %>%
  ungroup()
ggplot(bats_temp_heatmap, aes(x = year, y = mid_depth, fill = mean_temp)) +
  geom_tile() +
  scale_y_reverse() + # reverse y-axis
  scale_fill_viridis_c(option = "plasma") + # use viridis color scale
  labs(x = "Year", y = "Depth (m)", fill = "Mean Temp (°C)",
       title = "Heatmap of Mean Temperature by Depth and Year at BATS") +
  theme_classic()








####################### EXTRA ###############################################
#standardizing dates all into proper date objects----
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
  mutate(
    date_clean = case_when(
      nchar(as.character(date)) == 8 ~ as.Date(as.character(date), format = "%Y%m%d"), # YYYYMMDD
      nchar(as.character(date)) == 6 ~ as.Date(paste0(as.character(date), "01"), format = "%Y%m%d"), #YYYYMM
      (date > 0 & date < 100) ~ date_decimal(1988 + date),  # assume base year 1988 = Decimal year (e.g. 31.783) 
      str_detect(as.character(date), "\\.") ~ date_decimal(1988 + as.numeric(date)), #Long decimal version
      TRUE ~ as.Date(NA) #Everything else is missing
    )
  )
summary(bats_temp_surface$date_clean)
head(bats_temp_surface %>% select(date, date_clean))


##### Alternatively, extract just the surface temperature (shallowest depth) from each cast----
#b50049_surface_temps <- b50049_ctd %>%
#  group_by(cast_ID) %>%
#  slice_min(order_by = depth_m, n = 1) %>%
#  select(cast_ID, date, latitude, longitude, temperature_C,depth_m)
#view the surface temperature data
#View(b50049_surface_temps)


plot(b50049_ctd$temperature_C, b50049_ctd$depth_m, type="l", ylim=rev(range(b50049_ctd$depth_m)),
     xlab="Temperature (°C)", ylab="Depth (m)",
     main="CTD Temperature Profile - BATS Cast 49")
