###### Example of reading in and handling CTD data from BATS cast 49 ----
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
  select(cast_ID, date, latitude, longitude, depth_m, temperature_C) %>%
  arrange(desc(depth_m)) %>%
  na.omit()
#view the surface data
View(b50049_ctd_surface)



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
  "cast_ID", "decimal_year", "date", "latitude", "longitude",
  "pressure_dbar", "depth_m", "temperature_C", "conductivity_S_per_m",
  "salinity_PSS78", "dissolved_oxygen_umol_per_kg",
  "beam_attenuation_1_per_m", "fluorescence_RFU", "PAR_uE_per_m2_per_s"
)

# Function to read and clean one CTD file
read_and_extract_surface <- function(file) {
  df <- read_excel(file, na = "-999", col_names = FALSE) # read without column names
  colnames(df) <- col_names # Apply the same cleaning steps
  
  df_surface <- df %>% #  Extract upper 200m data
    filter(depth_m <= 200) %>%  # keep only upper/pelagic
    select(cast_ID, date, latitude, longitude, depth_m, temperature_C) %>% # select relevant columns
    arrange(desc(depth_m)) %>% # arrange by depth descending
    na.omit() # remove rows with missing data
  
  # Add filename (cast ID reference) just in case
  df_surface$file_name <- basename(file)
  
  return(df_surface)
}

# Apply the function to every CTD file
bats_temp_surface <- lapply(ctd_files, read_and_extract_surface)

# Combine into one big data frame
bats_temp_surface <- bind_rows(bats_temp_surface)

# Inspect result and checking its correct
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
length(unique(bats_temp_surface$file_name))






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