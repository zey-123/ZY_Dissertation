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
  "beam_attenuation_1_per_m", "fluorescence_RFU", "PAR_uE_per_m2_per_s")


# Diagnosing column misalignment issues ----
diagnose_ctd_file <- function(ctd_files) {
  df <- read_excel(ctd_files, na = "-999", col_names = FALSE)
  n_cols <- ncol(df)

  # Expected numeric column for temperature is always column 7 (in 13-col format)
  # or column 8 (in 14-col format, because of the date column)
  
  # Determine expected temperature column index
  expected_temp_col <- ifelse(n_cols == 13, 7,
                         ifelse(n_cols == 14, 8, NA))

  # If unexpected column count, flag immediately
  if (is.na(expected_temp_col)) {
    return(tibble(
      file = basename(ctd_files),
      n_cols = n_cols,
      has_date_col = NA,
      temp_numeric_rate = NA,
      temp_misaligned = TRUE,
      flag = "Unexpected number of columns"
    ))
  }

  # Extract the column that SHOULD be temperature
  temp_col <- df[[expected_temp_col]]

  # Calculate % numeric values in the temperature column
  numeric_rate <- mean(suppressWarnings(!is.na(as.numeric(temp_col))))

  # Heuristic: if <80% of the column is numeric, it’s likely misaligned
  misaligned <- numeric_rate < 0.8

  tibble(
    file = basename(ctd_files),
    n_cols = n_cols,
    has_date_col = n_cols == 14,
    temp_numeric_rate = numeric_rate,
    temp_misaligned = misaligned,
    flag = dplyr::case_when(
      misaligned ~ "Temperature column misaligned",
      n_cols == 14 ~ "Has date column (OK)",
      n_cols == 13 ~ "No date column (OK)",
      TRUE ~ "Unknown"
    )
  )
}

all_files <- list.files(path, full.names = TRUE, pattern = "_ctd\\.xls$")

diagnostic_results <- purrr::map_dfr(all_files, diagnose_ctd_file)

diagnostic_summary <- diagnostic_results %>%
  count(flag)

print(diagnostic_summary)

problem_files <- diagnostic_results %>%
  filter(temp_misaligned | flag == "Unexpected number of columns")

print(problem_files)










# Other methods of handling variable column numbers----
# 1) Option 1: initial method - doesnt handle column numbers ----
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

# 2) ***Option 2***: Alternative method handling 13 or 14 columns - OBSERVATIONS 484447 (MORE THAN OPTION 3) ----

ctd_files <-ctd_files[!basename(ctd_files) %in% "b20040_ctd.xls"] #getting rid of corrupted file 

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

# 3) Option 3: alternative method to handling variable columns by dropping extra date column if present - OBSERVATIONS 484321 (LESS THAN OPTION 2) ----
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


##########Continue ----
#** Apply the function to every CTD file
bats_temp_surface <- lapply(ctd_files, read_and_extract_surface)

# Combine into one big data frame
bats_temp_surface <- bind_rows(bats_temp_surface)

# Inspecting new dataset - bats_temp_surface and checking everything is correct ----
View(bats_temp_surface)
summary(bats_temp_surface$depth_m) # checking range
#Minimum = 1.97 (surface) and Maximum ≤ 200 = 199.68, so no depth >200 and the filtering worked YAY
summary(bats_temp_surface$temperature_C) # checking temperature range
length(unique(bats_temp_surface$cast_ID)) # checking number of unique casts combined
# 5006

bats_temp_surface %>%
  count(cast_ID) %>%
  arrange(desc(n)) %>%
  head(10)
# shows number of measurements per cast_ID, top 10 casts with most measurements
length(unique(bats_temp_surface$file_name)) # 447 - this is the number of individual files loaded and is correct YAY




#### Visualizing general trends ----

############  1) Visualizing average surface temperature over time 
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
       title = "Annual Mean Sea Surface Temperature at BATS Over Time") +
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

############  2) Visualizing relationship between depth and temperature averaged across years by using bins of depth
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

############  3) Visualizing temperature with depth over time using a heatmap
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


#Making a clean data set for temperature and reducing rows by aggregating  -----
# doing this like such: Data <-aggregate(temp ( metric to summarise) , by = deciyear, (function) mean) - rounding decimal year to be full number 

# rounding decimal year to a whole number
bats_temp_surface_clean <- bats_temp_surface %>%
  mutate(decimal_year_whole = floor(decimal_year))

bats_temp_FINAL<- bats_temp_surface_clean %>%
  group_by(decimal_year_whole) %>% # grouping by decimal year 
  summarise(mean_temp = mean(temperature_C, na.rm = TRUE)) # calculating mean temperature for each year
View(bats_temp_FINAL)

plot(mean_temp~decimal_year_whole, data=bats_temp_FINAL, type="b",
     xlab="Decimal Year", ylab="Mean Surface Temperature (°C)",
     main="Mean Surface Temperature at BATS Over Time")
bats_temp_clean <- bats_temp_surface %>%
  aggregate(temperature_C ~ decimal_year, data = ., FUN = mean, na.rm = TRUE) # aggregating temperature by decimal year to get mean temperature

#save final bats_temp_Final to github 
write.csv(bats_temp_FINAL, "Data/bats_temp_FINAL.csv", row.names = FALSE)


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
  geom_polygon(
    data = world_map,
    aes(long, lat, group = group),
    fill = "grey90",
    color = "white",
    linewidth = 0.2
  ) +
  geom_point(
    data = bats_temp_surface,
    aes(longitude, latitude),
    size = 1.3,
    alpha = 0.6
  ) +
  coord_fixed(
    xlim = c(-90, -50),
    ylim = c(20, 45))+
  labs(
    title = "BATS CTD Sampling Locations",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# Map with points colored by temperature
ggplot() +
  geom_polygon(
    data = world_map,
    aes(long, lat, group = group),
    fill = "grey85",
    color = "white",
    linewidth = 0.2) +
  geom_point(
    data = bats_temp_surface,
    aes(longitude, latitude, color = temperature_C),
    size = 1.5,
    alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", name = "SST (°C)") +
  coord_fixed(
    xlim = c(-90, -50),
    ylim = c(20, 45))+
  labs(
    title = "Sea Surface Temperature at BATS",
    subtitle = "CTD surface observations (1988–2016)") +
  theme_classic(base_size = 12)+
  theme(panel.grid = element_blank())


# contour plot displaying vertical temperature profiles and maybe mixed layer depth (MLD) in the BATS side for the period 1988-2016
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)
library(akima)
install.packages("akima")

bats_temp <- bats_temp_surface %>%
  filter(decimal_year >= 1988 & decimal_year <= 2016) %>%
  mutate(Year = floor(decimal_year),    # Extract year
    frac = decimal_year - Year,    # Fractional part of year
    DayOfYear = round(frac * 365.25),    # Convert fraction of year to days (ignore leap year for simplicity)
    Date = as.Date(DayOfYear, origin = paste0(Year, "-01-01"))    # Create a Date
  )

bats_avg <- bats_temp %>%
  group_by(Date, depth_m) %>%
  summarise(Temp = mean(temperature_C, na.rm = TRUE), .groups = "drop")

#SST figure (raw + deseasonalised + pre/post-2000 dashed trends)

library(dplyr)
library(lubridate)
library(ggplot2)

cut_year <- 2000

# 1) Build an SST time series from CTD: median temp in upper 200 m for each Date
sst_daily <- bats_temp_surface %>%
  mutate(Date = as.Date(Date),
         Year = year(Date),
         Month = month(Date)) %>%
  filter(!is.na(Date), !is.na(temperature_C), depth_m <= 11) %>%  # "surface" definition
  group_by(Date, Year, Month) %>%
  summarise(SST = median(temperature_C, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date)

# 2) Deseasonalise SST by removing the mean monthly climatology
climatology <- sst_daily %>%
  group_by(Month) %>%
  summarise(clim = mean(SST, na.rm = TRUE), .groups = "drop")

sst_ds <- sst_daily %>%
  left_join(climatology, by = "Month") %>%
  mutate(SST_deseason = SST - clim)

# 3) Rescale raw SST onto anomaly scale so both can be plotted + add a correct 2nd y-axis
sf <- diff(range(sst_ds$SST_deseason, na.rm = TRUE)) / diff(range(sst_ds$SST, na.rm = TRUE))
sst_mean <- mean(sst_ds$SST, na.rm = TRUE)

# 4) Creating seperate data frames for each series to plot with different aesthetics 
raw_df  <- sst_ds %>% mutate(Series = "Raw SST",
                             y = (SST - sst_mean) * sf)
ds_df   <- sst_ds %>% mutate(Series = "Deseasonalised SST",
                             y = SST_deseason)
pre_df  <- sst_ds %>% filter(Year < cut_year) %>% mutate(Series = "Pre-2000 trend")
post_df <- sst_ds %>% filter(Year >= cut_year) %>% mutate(Series = "Post-2000 trend")

ggplot(sst_ds, aes(x = Date)) +
  # Raw SST (scaled onto anomaly axis so it’s visible) + plotted as a grey line
  geom_line(data=raw_df, aes(x=Date, y=y, colour = Series),
            linewidth = 0.7, alpha=0.7)+
  # Deseasonalised SST anomaly (this is the main signal you trend-fit)
  geom_line(data=ds_df, aes(x = Date, y=y, colour= Series),
            linewidth = 1.0)+
  # Pre-2000 linear trend (dashed red) on deseasonalised SST
  geom_smooth(data = pre_df, aes(x = Date, y = SST_deseason, colour = Series),
              method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.0) +
  # Post-2000 linear trend (dashed green) on deseasonalised SST
  geom_smooth(data = post_df, aes(x = Date, y = SST_deseason, colour = Series),
              method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.0) +
  # Left axis = deseasonalised SST; Right axis = original SST (converted back correctly)
  scale_y_continuous(name = "Deseasonalised SST (°C)",
    sec.axis = sec_axis(~ . / sf + sst_mean, name = "SST (°C)")) +
  labs(title = "SST and Deseasonalised SST with Pre/Post-2000 Trends",
    x = "Year") +
  #Manual colors for legend
  scale_colour_manual(name = NULL,values = c("Raw SST" = "grey70","Deseasonalised SST" = "black",
      "Pre-2000 trend" = "red","Post-2000 trend" = "blue")) +
  theme_classic()


library(dplyr)
library(lubridate)

temp_200m <- bats_temp_surface %>%
  filter(depth_m <= 200) %>%
  group_by(cast_ID, decimal_year) %>%   # one value per cast
  summarise(Temp_0_200 = mean(temperature_C, na.rm = TRUE),
    .groups = "drop")
temp_200m <- temp_200m %>%
  mutate(Year = floor(decimal_year),
    Day = round((decimal_year - Year) * 365.25),
    Date = as.Date(Day, origin = paste0(Year, "-01-01")))
temp_monthly <- temp_200m %>%
  filter(!is.na(Date)) %>%
  mutate(MonthDate = floor_date(Date, "month")) %>%
  group_by(MonthDate) %>%
  summarise(Temp = mean(Temp_0_200, na.rm = TRUE), .groups = "drop") %>%
  mutate(Month = month(MonthDate, label = TRUE, abbr = TRUE))

ggplot(temp_monthly, aes(x = Month, y = Temp)) +
  geom_boxplot(fill = "pink", colour = "darkred", outlier.alpha = 0.2) +
  labs(title = "Seasonal cycle of upper-ocean temperature (0–200 m)",
       x = "Month",
       y = "Mean temperature (°C)") +
  theme_classic()

####################### EXTRA ############################################### ----
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
