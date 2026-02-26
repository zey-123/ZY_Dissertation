# Attempting to see outputs of alternative method to calculate indices.
# Method used an average of top 3 depth bins instead of depth integrating and normalizing across the upper 200m (results from this method are under PhenologicalIndicesCalc script)

# Load libraries & data
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

phyto_top3_clean <- read.csv("Data/phyto_top3_clean.csv")
temperature <- read_csv("Data/BATS_temp_FINAL.csv") #SST

phyto_top3_clean


# Calculating Phenological Indices (Ji et al., 2010) ----


# 1) Bloom start date: date when chlorophyll concentration first exceeds a threshold value (e.g., mean + 1 standard deviation of the annual cycle) and remains above it for a specified duration (e.g., 7 days).----
### Phytoplankton: Year day when biomass rise above certain threshold value; often used for indexing phytoplankton.	----
# The function finds the first day each year when biomass exceeds a year-specific threshold for at least a set number of consecutive days.
calculate_bloom_start_phytoplankton <- function(phyto_top3_clean, threshold_multiplier = 1, duration = 2) { # threshold_multiplier: number of standard deviations above mean; duration: number of consecutive days (“Due to the temporal resolution of the dataset, a minimum duration of two consecutive observations was required…)
  data <- phyto_top3_clean %>%
    mutate(Year = as.numeric(format(Date, "%Y")), # labelling each observation with 1)which year it belongs to and 2)which day of the year it is.
           DayOfYear = as.numeric(format(Date, "%j")))
  bloom_starts <- data %>% 
    group_by(Year) %>% #Bloom timing is calculated separately for each year.
    arrange(Date) %>% #So “consecutive days” actually means consecutive in time.
    mutate(Threshold = mean(Biomass, na.rm = TRUE) + threshold_multiplier * sd(Biomass, na.rm = TRUE), #This creates one threshold per year.
           AboveThreshold = Biomass > Threshold, #TRUE → bloom-level biomass, FALSE → not bloom-level
           RunID = cumsum(AboveThreshold != lag(AboveThreshold, default = first(AboveThreshold)))) %>%   # Create a run ID that increases every time AboveThreshold changes (so looks at today vs yesterday if T/F changes, starts a new run BUT if T/F stays the same, continues run)
    group_by(Year, RunID) %>% #counting how long each run lasts 
    mutate(RunLength = n()) %>% #number of days in each run
    ungroup() %>% 
    filter( AboveThreshold, RunLength >= duration) %>% #only keep days above threshold 
    group_by(Year) %>% 
    slice(1) %>% # gives the first day of the first valid bloom each year.
    select(Year, BloomStartDay = DayOfYear)
  
  return(bloom_starts)
}

bloom_start_phytoplankton <- calculate_bloom_start_phytoplankton(phyto_top3_clean %>% rename(Date = Date,Biomass = Chl_top3))
# View results
print(bloom_start_phytoplankton)
#visualize
ggplot(bloom_start_phytoplankton, aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "forestgreen") +
  geom_point() +
  geom_smooth(method = "lm", color = "forestgreen", linetype = "dashed",se = FALSE) +
  labs(title = "Phytoplankton Bloom Start Day Over Years",
       x = "Year",
       y = "Bloom Start Day of Year") +
  theme_classic()


#The zooplankton method works cleanly in dplyr because it is based on cumulative thresholds, 
# while phytoplankton bloom detection requires identifying sustained consecutive periods, 
#which is why run-detection logic (and not simple mutate()) is needed.


# Median based threshold - year/day where Chl levels first rise a small threshold above median values (Siegel 2002) - could be an alternative method for phytoplankton bloom start detection.
calculate_bloom_start_phyto_medianthresh <- function(data, frac = 0.1, duration = 3) { # frac: fraction above median to define threshold; duration: number of consecutive days
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
bloom_start_phyto_medianthresh <- calculate_bloom_start_phyto_medianthresh(phyto_top3_clean %>% rename(Date = Date, Biomass = Chl_top3))
# View results
print(bloom_start_phyto_medianthresh)
#visualize
ggplot(bloom_start_phyto_medianthresh, aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "lightblue") +
  geom_point() +
  geom_smooth(method = "lm", color = "lightblue", linetype = "dashed",se = FALSE) +
  labs(title = "Phytoplankton Bloom Start Day (Median Threshold Method) Over Years",
       x = "Year",
       y = "Bloom Start Day of Year") +
  theme_classic()

# 2) Bloom peak: date when chlorophyll concentration reaches its maximum value during the bloom period.----

### Phytoplankton: Year day with highest biomass at a defined period; Used for indexing phytoplankton and zooplankton.	----
# Function to calculate bloom peak date for phytoplankton
calculate_bloom_peak_phytoplankton <- function(phyto_top3_clean, percentile = 75) {
  data <- phyto_top3_clean %>%
    mutate(Year = as.numeric(format(Date, "%Y")),
           DayOfYear = as.numeric(format(Date, "%j"))) %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(Threshold = quantile(Biomass, probs = percentile / 100, na.rm = TRUE),
           AboveThreshold = Biomass >= Threshold) %>%
    filter(AboveThreshold) %>%
    slice_max(Biomass, n = 1, with_ties = FALSE) %>%
    select(Year, BloomPeakDay = DayOfYear) %>%
    ungroup()
}

bloom_peak_phytoplankton <- calculate_bloom_peak_phytoplankton(phyto_top3_clean %>% rename(Date = Date, Biomass = Chl_top3))
# View results
print(bloom_peak_phytoplankton)
#visualize
ggplot(bloom_peak_phytoplankton, aes(x = Year, y = BloomPeakDay)) +
  geom_line(color = "forestgreen", na.rm = TRUE) +
  geom_point() +
  geom_smooth(method = "lm", color = "forestgreen", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Peak Day Over Years",
       x = "Year",
       y = "Bloom Peak Day of Year") +
  theme_classic()


# 3) Bloom duration: number of days between bloom start and bloom end (date when chlorophyll concentration falls below the threshold value again).----
# Number of days between bloom “start” and “end” of season percentile thresholds.	

### Phytoplankton: Function to calculate bloom duration for phytoplankton ----
calculate_bloom_duration_phytoplankton <- function(phyto_top3_clean, percentile = 75) {
  
  data %>%
    mutate(Year = as.numeric(format(Date, "%Y")),
           DayOfYear = as.numeric(format(Date, "%j"))) %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(Threshold = quantile(Biomass, probs = percentile / 100, na.rm = TRUE),
           AboveThreshold = Biomass >= Threshold) %>%
    filter(AboveThreshold) %>%
    summarise(BloomStartDay = min(DayOfYear),
              BloomEndDay   = max(DayOfYear),
              BloomDuration = BloomEndDay - BloomStartDay) %>%
    ungroup()
}

bloom_duration_phytoplankton <- calculate_bloom_duration_phytoplankton(phyto_top3_clean %>% rename(Biomass = Chl_top3))
# View results
print(bloom_duration_phytoplankton)
#visualize
ggplot(bloom_duration_phytoplankton, aes(x = Year, y = BloomDuration)) +
  geom_line(color = "forestgreen") +
  geom_point() +
  geom_smooth(method = "lm", color = "forestgreen", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Duration Over Years",
       x = "Year",
       y = "Bloom Duration (days)") +
  theme_classic()


# 4) Bloom Magnitude: -----

### Phytoplankton:  ----

# calculating time-integrated biomass only during the bloom period (start–end window) (integrate chl-a concentration  between the bloom initiation and termination dates.)

calculate_bloom_magnitude_trapz <- function(phyto_top3_clean,
                                            percentile = 75,
                                            date_col = "Date",
                                            biomass_col = "Biomass") {
  df <- phyto_top3_clean %>%   # Preparing a clean working table
    mutate(Date = as.Date(.data[[date_col]]),
           Biomass = .data[[biomass_col]],
           Year = year(Date),
           DayOfYear = yday(Date),
           t = as.numeric(Date)   # numeric time in days for trapz
    ) %>%
    filter(!is.na(Date), !is.na(Biomass)) %>%
    arrange(Year, Date)
  
  # --- Step 1: get bloom initiation/termination per year (percentile threshold)
  bloom_periods <- df %>%
    group_by(Year) %>%
    mutate(Threshold = quantile(Biomass, probs = percentile/100, na.rm = TRUE),
           Above = Biomass >= Threshold) %>%
    filter(Above) %>%
    summarise(BloomStartDay = min(DayOfYear),
              BloomEndDay   = max(DayOfYear),
              BloomStartDate = min(Date),
              BloomEndDate   = max(Date),
              n_above = n(),
              .groups = "drop")
  
  # --- Step 2: integrate chl-a between those dates using trapezoids (trapz)
  bloom_mag <- df %>%
    inner_join(bloom_periods %>% select(Year, BloomStartDay, BloomEndDay), by = "Year") %>%
    filter(DayOfYear >= BloomStartDay, DayOfYear <= BloomEndDay) %>%
    arrange(Year, t) %>%
    group_by(Year) %>%
    summarise(BloomMagnitude = ifelse(
      n() < 2, NA_real_,  # need at least 2 points to integrate
      sum(diff(t) * (head(Biomass, -1) + tail(Biomass, -1)) / 2, na.rm = TRUE)),
      n_obs_in_window = n(),
      .groups = "drop")
  
  # Return one table with both the bloom window and magnitude
  bloom_periods %>%
    left_join(bloom_mag, by = "Year")}

bloom_magnitude_phytoplankton <- calculate_bloom_magnitude_trapz(
  phyto_top3_clean %>% transmute(Date = Date, Biomass = Chl_top3),
  percentile = 75)

ggplot(bloom_magnitude_phytoplankton, aes(x = Year, y = BloomMagnitude)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", color = 'forestgreen', se = FALSE) +
  labs(title = "Phytoplankton Bloom Magnitude (Time-integrated Chl-a)",
       x = "Year",
       y = "Bloom integrated Chl-a (mg m^-3 · day)") +
  theme_classic()



# 5) Bloom Amplitude calculation: ----
### Phytoplankton ----
# calculating the maximum chl-a concentration during the bloom period (peak value) minus the baseline chl-a concentration (e.g., mean or median chl-a concentration during the non-bloom period).
calculate_bloom_amplitude <- function(phyto_top3_clean, bloom_windows) {
  
  df <- phyto_top3_clean %>%
    mutate(Date = as.Date(Date),
           Year = lubridate::year(Date),
           DayOfYear = lubridate::yday(Date)) %>%
    filter(!is.na(Biomass))
  
  amplitude <- df %>%
    inner_join(bloom_windows %>% select(Year, BloomStartDay, BloomEndDay), by = "Year") %>%
    filter(DayOfYear >= BloomStartDay, DayOfYear <= BloomEndDay) %>%
    group_by(Year) %>%
    summarise(BloomAmplitude = max(Biomass, na.rm = TRUE), #splitting bloom period data into years and taking max biomass  in each year (this is amplitude)
              .groups = "drop")
  
  bloom_windows %>%
    left_join(amplitude, by = "Year")}

# run it
phyto_amplitude <- calculate_bloom_amplitude(
  phyto_top3_clean %>% transmute(Date = Date, Biomass = Chl_top3),
  bloom_duration_phytoplankton)

# visualize
ggplot(phyto_amplitude, aes(x = Year, y = BloomAmplitude)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", color='forestgreen', se = FALSE) +
  labs(title = "Phytoplankton Bloom Amplitude (Peak Chl-a)",
       x = "Year",
       y = "Bloom Peak Chl-a (mg m^-3)") +
  theme_classic()







