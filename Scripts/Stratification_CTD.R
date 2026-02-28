# Importing and cleaning CTD data (similar to CTD_Handling script) for stratification use ----
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

# Diagnosing column misalignment issues
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
  select(any_of(c("cast_ID", "decimal_year","latitude", "longitude",
                    "depth_m", "temperature_C","salinity_PSS78"))) %>%
   arrange(desc(depth_m)) %>%
    na.omit()
  
  df_surface$file_name <- basename(file)
  return(df_surface)
}

#Apply the function to every CTD file
bats_CTD <- lapply(ctd_files, read_and_extract_surface)

# Combine into one big data frame
bats_CTD <- bind_rows(bats_CTD)

# Inspecting new dataset - bats_temp_surface and checking everything is correct 
View(bats_CTD)
summary(bats_CTD$depth_m) # checking range


# Calculating potential density from CTD data - using TEOS-10 Gibbs Seawater (GSW) package ----
install.packages("gsw")
library(gsw)
library(dplyr)

# Calculate density at each depth
ctd <- bats_CTD %>%
  mutate(
    SA = gsw_SA_from_SP(salinity_PSS78, depth_m,latitude,longitude),  # SA = Absolute salinity
    CT = gsw_CT_from_t(SA, temperature_C, depth_m),                 # CT = Conservative temp
    Sigma0 = gsw_sigma0(SA, CT)                                 # Sigma0= Potential density
  )

# Inspecting stratification index per date - desnity difference between surface (10m) and deep (50m)
# setting depth bins to capture the main density contrast that resists mixing.
strat_index <- ctd %>%
  filter(depth_m <= 200) %>%
  mutate(depth_bin = case_when(
      depth_m <= 15 ~ "surface",
      depth_m >= 50 & depth_m <= 200 ~ "deep",
      TRUE ~ NA_character_)) %>%
  filter(!is.na(depth_bin)) %>%
  group_by(decimal_year, depth_bin) %>% #to look at average density within each layer as CTD casts sample many depths
  summarise(mean_sigma = mean(Sigma0, na.rm = TRUE),
    .groups = "drop") %>%
  tidyr::pivot_wider(names_from = depth_bin,values_from = mean_sigma) %>% #putting surface and deep densities side by side
  mutate(Stratification = deep - surface)


ggplot(strat_index, aes(x = decimal_year, y = Stratification)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# Save stratification index for later use
write_csv(strat_index, "Data/bats_stratification_index.csv")
# Now this is a stratification index dataset with one value per CTD cast date




# More Strat visualisations 

# Seasonal Cycle 
library(dplyr)
library(lubridate)
library(ggplot2)

strat_index2 <- strat_index %>%
  mutate(Date = as.Date((decimal_year - 1970) * 365.25, origin = "1970-01-01"),
         MonthDate = floor_date(Date, "month"))

strat_monthly <- strat_index2 %>%
  group_by(MonthDate) %>%
  summarise(Strat = mean(Stratification, na.rm = TRUE), .groups="drop")

ggplot(strat_monthly, aes(x = MonthDate, y = Strat)) +
  geom_line() +
  geom_point(size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE) +
  labs(title="Stratification index (monthly mean)", x="Year", y="Deep − surface density (σθ)") +
  theme_classic()

strat_season <- strat_index2 %>%
  mutate(Month = month(Date, label = TRUE))

ggplot(strat_season, aes(x = Month, y = Stratification)) +
  geom_boxplot(outlier.alpha = 0.2, colour ="navy", fill="lightblue") +
  labs(title="Seasonal cycle of stratification", x="Month", y="Deep − surface density (σθ)") +
  theme_classic()
