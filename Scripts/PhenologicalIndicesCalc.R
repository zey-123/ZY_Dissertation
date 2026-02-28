# Importing datasets & libraries ----
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

norm_chl <- read_csv("Data/norm_chl.csv") #phyto
zoop_daily <- read_csv("Data/zoop_daily.csv") #zooplankton
temperature <- read_csv("Data/BATS_temp_FINAL.csv") #SST


# PLotting phyto against zoop before phenology calculations ----

#### General plot over time using secondary axes 
ggplot() +
  geom_line(data = norm_chl, aes(x = Date, y = Chl_mean_200m_mg_m3, color = "Phytoplankton (Chlorophyll)")) +
  geom_line(data = zoop_daily, aes(x = date, y = DryBiomass / 100, color = "Zooplankton (Dry Biomass)")) + # scaling zoop biomass for better visualization
  scale_y_continuous(name = "Phytoplankton (Chlorophyll mg/m³)",
    sec.axis = sec_axis(~ . * 100, name = "Zooplankton (Dry Biomass µg/L)") # scaling back for secondary axis
  ) +
  scale_color_manual(values = c("Phytoplankton (Chlorophyll)" = "blue", "Zooplankton (Dry Biomass)" = "forestgreen")) +
  labs(title = "Phytoplankton and Zooplankton Biomass Over Time",
       x = "Date",
       color = "Legend") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "forestgreen"))

### PLotting phytoplankton against zooplankton with points and trend lines
ggplot() +
  geom_point(data = norm_chl, aes(x = Date, y = Chl_mean_200m_mg_m3, color = "Phytoplankton (Chlorophyll)"), alpha = 0.5) +
  geom_smooth(data = norm_chl, aes(x = Date, y = Chl_mean_200m_mg_m3, color = "Phytoplankton (Chlorophyll)"), method = "lm", se = FALSE) +
  geom_point(data = zoop_daily, aes(x = date, y = DryBiomass / 100, color = "Zooplankton (Dry Biomass)"), alpha = 0.5) + # scaling zoop biomass for better visualization
  geom_smooth(data = zoop_daily, aes(x = date, y = DryBiomass / 100, color = "Zooplankton (Dry Biomass)"), method = "lm", se = FALSE) +
  scale_y_continuous(name = "Phytoplankton (Chlorophyll mg/m³)",
    limits = c(0,5),
    sec.axis = sec_axis(~ . * 100, name = "Zooplankton (Dry Biomass µg/L)") # scaling back for secondary axis
  ) +
  scale_color_manual(values = c("Phytoplankton (Chlorophyll)" = "blue", "Zooplankton (Dry Biomass)" = "forestgreen")) +
  labs(title = "Phytoplankton and Zooplankton Biomass Over Time with Trend Lines", x = "Date",color = "Legend") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "forestgreen"))



# Calculating Phenological Indices (Ji et al., 2010) ----


# 1) Bloom start date: date when chlorophyll concentration first exceeds a threshold value (e.g., mean + 1 standard deviation of the annual cycle) and remains above it for a specified duration (e.g., 7 days).----
### Phytoplankton: Year day when biomass rise above certain threshold value; often used for indexing phytoplankton.	----
# The function finds the first day each year when biomass exceeds a year-specific threshold for at least a set number of consecutive days.
calculate_bloom_start_phytoplankton <- function(data, threshold_multiplier = 0.5, duration = 2, min_obs_year = 6) { # threshold_multiplier: number of standard deviations above mean; duration: number of consecutive days (“Due to the temporal resolution of the dataset, a minimum duration of two consecutive observations was required…)
  data <- data %>%
    mutate(Year = as.numeric(format(Date, "%Y")), # labelling each observation with 1)which year it belongs to and 2)which day of the year it is.
           DayOfYear = as.numeric(format(Date, "%j")))
  bloom_starts <- data %>% 
    group_by(Year) %>% #Bloom timing is calculated separately for each year.
    filter(n() >= min_obs_year) %>% #Filteryears with too few observations. 
    arrange(Date) %>% #So “consecutive days” actually means consecutive in time.
    mutate(Threshold = mean(Biomass, na.rm = TRUE) + threshold_multiplier * sd(Biomass, na.rm = TRUE), #This creates one threshold per year.
           AboveThreshold = Biomass >= Threshold, #TRUE → bloom-level biomass, FALSE → not bloom-level
           RunID = cumsum(AboveThreshold != lag(AboveThreshold, default = first(AboveThreshold)))) %>%   # Create a run ID that increases every time AboveThreshold changes (so looks at today vs yesterday if T/F changes, starts a new run BUT if T/F stays the same, continues run)
    group_by(Year, RunID) %>% #counting how long each run lasts 
    mutate(RunLength = n()) %>% #number of days in each run
    ungroup() %>% 
    filter( AboveThreshold, RunLength >= duration) %>% #only keep days above threshold 
    group_by(Year) %>% 
    slice(1) %>% # gives the first day of the first valid bloom each year.
    select(Year, BloomStartDay = DayOfYear)
  
  return(bloom_starts)}
bloom_start_phytoplankton <- calculate_bloom_start_phytoplankton(norm_chl %>% rename(Date = Date,Biomass = Chl_mean_200m_mg_m3),
                                                                 threshold_multiplier = 0.5, duration = 2,min_obs_year = 6)

#visualize
ggplot(bloom_start_phytoplankton %>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "navy") +
  geom_point() +
  geom_smooth(method = "lm", color = "navy", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Start Day ",
       x = "Year",y = "Bloom Start Day of Year") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
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


### Alternative Phytoplankton: Year day of maximum instantaneous growth rate within a defined period; used for indexing phytoplankton.	----
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


### Zooplankton: Year day when a lower threshold percentile (e.g. 25th percentile) of annual or seasonal cumulative biomass or abundance is reached; used for indexing zooplankton.	----

calculate_bloom_start_zooplankton <- function(data, percentile = 25) { # percentile: the lower percentile threshold
  data <- data %>%
    mutate(Year = as.numeric(format(Date, "%Y")),
           DayOfYear = as.numeric(format(Date, "%j")))
  
  bloom_starts <- data %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(CumulativeBiomass = cumsum(Biomass),
           TotalBiomass = sum(Biomass, na.rm = TRUE),
           PercentileThreshold = quantile(CumulativeBiomass, probs = percentile / 100, na.rm = TRUE)) %>% # Calculate the percentile threshold
    filter(CumulativeBiomass >= PercentileThreshold) %>% # Find first day exceeding the threshold
    slice(1) %>% # Take the first occurrence
    select(Year, BloomStartDay = DayOfYear)
  
  return(bloom_starts) 
}
bloom_start_zooplankton <- calculate_bloom_start_zooplankton(zoop_daily %>% rename(Date = date, Biomass = DryBiomass)) # Rename columns to match function

# visualize
ggplot(bloom_start_zooplankton %>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "purple") +
  geom_point() + 
  geom_smooth(method = "lm", color = "purple", linetype = "dashed",se = FALSE) +
  labs(title = "Zooplankton Bloom Start Day Over Years",
       x = "Year", y = "Bloom Start Day of Year") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()

# 2) Bloom peak: date when chlorophyll concentration reaches its maximum value during the bloom period.----

### Phytoplankton: Year day with highest biomass at a defined period; Used for indexing phytoplankton and zooplankton.	----
# Function to calculate bloom peak date for phytoplankton
calculate_bloom_peak_phytoplankton <- function(data, percentile = 75) {
  data <- data %>%
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
  
bloom_peak_phytoplankton <- calculate_bloom_peak_phytoplankton(norm_chl %>% rename(Date = Date, Biomass = Chl_mean_200m_mg_m3))

#visualize
ggplot(bloom_peak_phytoplankton%>% filter(Year >= 1995, Year <= 2022), 
       aes(x = Year, y = BloomPeakDay)) +
  geom_line(color = "navy", na.rm = TRUE) +
  geom_point() +
  geom_smooth(method = "lm", color = "navy", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Peak Day Over Years",
       x = "Year",y = "Bloom Peak Day of Year") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()

### Zooplankton: Function to calculate bloom peak date for zooplankton ----
calculate_bloom_peak_zooplankton <- function(data, percentile = 75) {
  data %>%
    mutate( Year = as.numeric(format(Date, "%Y")),
            DayOfYear = as.numeric(format(Date, "%j"))) %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(
      Threshold = quantile(Biomass, probs = percentile / 100, na.rm = TRUE),
      AboveThreshold = Biomass >= Threshold
    )%>%
    filter(AboveThreshold) %>%
    slice_max(Biomass, n = 1, with_ties = FALSE) %>%
    select(Year, BloomPeakDay = DayOfYear) %>%
    ungroup()
}

bloom_peak_zooplankton <- calculate_bloom_peak_zooplankton(zoop_daily %>% rename(Date = date, Biomass = DryBiomass))

#visualize
ggplot(bloom_peak_zooplankton%>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomPeakDay)) +
  geom_line(color = "purple") +
  geom_point() +
  geom_smooth(method = "lm", color = "purple", linetype = "dashed", se = FALSE) +
  labs(title = "Zooplankton Bloom Peak Day Over Years",
       x = "Year",y = "Bloom Peak Day of Year") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()

summary(lm(BloomPeakDay ~ Year, data = bloom_peak_zooplankton))


# 3) Bloom duration: number of days between bloom start and bloom end (date when chlorophyll concentration falls below the threshold value again).----
# Number of days between bloom “start” and “end” of season percentile thresholds.	

### Phytoplankton: Function to calculate bloom duration for phytoplankton ----
calculate_bloom_duration_phytoplankton <- function(data, percentile = 75) {
  
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

bloom_duration_phytoplankton <- calculate_bloom_duration_phytoplankton(norm_chl %>% rename(Biomass = Chl_mean_200m_mg_m3))

#visualize
ggplot(bloom_duration_phytoplankton%>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomDuration)) +
  geom_line(color = "navy") +
  geom_point() +
  geom_smooth(method = "lm", color = "navy", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Duration Over Years",
       x = "Year",y = "Bloom Duration (days)") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()


### Zooplankton: Function to calculate bloom duration for zooplankton ----
calculate_bloom_duration_zooplankton <- function(data, start_percentile = 25, end_percentile = 75) { # start_percentile: lower percentile threshold; end_percentile: upper percentile threshold
  data <- data %>%
    mutate(Year = as.numeric(format(Date, "%Y")),
           DayOfYear = as.numeric(format(Date, "%j")))
  
  bloom_durations <- data %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(CumulativeBiomass = cumsum(Biomass),
           TotalBiomass = sum(Biomass, na.rm = TRUE),
           StartThreshold = quantile(CumulativeBiomass, probs = start_percentile / 100, na.rm = TRUE),
           EndThreshold = quantile(CumulativeBiomass, probs = end_percentile / 100, na.rm = TRUE)) %>%
    summarise(
      BloomStartDay = DayOfYear[which(CumulativeBiomass >= StartThreshold)[1]],
      BloomEndDay = DayOfYear[which(CumulativeBiomass >= EndThreshold)[1]],
      BloomDuration = BloomEndDay - BloomStartDay) %>%
    ungroup()
  
  return(bloom_durations)
}
bloom_duration_zooplankton <- calculate_bloom_duration_zooplankton(zoop_daily %>% rename(Date = date, Biomass = DryBiomass))

#visualize
ggplot(bloom_duration_zooplankton%>% filter(Year >= 1995, Year <= 2022), 
       aes(x = Year, y = BloomDuration)) +
  geom_line(color = "purple") +
  geom_point() +
  geom_smooth(method = "lm", color = "purple", linetype = "dashed", se = FALSE) +
  labs(title = "Zooplankton Bloom Duration Over Years",
       x = "Year",y = "Bloom Duration (days)") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()

# 4) Bloom Magnitude: -----

### Phytoplankton:  ----

# calculating time-integrated biomass only during the bloom period (start–end window) (integrate chl-a concentration  between the bloom initiation and termination dates.)

calculate_bloom_magnitude_trapz <- function(norm_chl,
                                            percentile = 75,
                                            date_col = "Date",
                                            biomass_col = "Biomass") {
  df <- norm_chl %>%   # Preparing a clean working table
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
  norm_chl %>% transmute(Date = Date, Biomass = Chl_mean_200m_mg_m3),
  percentile = 75)

ggplot(bloom_magnitude_phytoplankton%>%filter(Year>=1995, Year<=2022), aes(x = Year, y = BloomMagnitude)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Magnitude (Time-integrated Chl-a)",
    x = "Year",
    y = "Bloom integrated Chl-a (mg m^-3 · day)") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()

#looking at bloom magnitude against temperature
bloom_magnitude_temp <- bloom_magnitude_phytoplankton %>%
  inner_join(bats_temp_FINAL, by = "Year")

ggplot(bloom_magnitude_temp%>% filter(Year >= 1995, Year <= 2022), 
       aes(x = MeanTemp, y = BloomMagnitude, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Phytoplankton Bloom Magnitude vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Bloom integrated Chl-a (mg m^-3 · day)",
       color = "Year") +
  theme_classic()

summary (lm(BloomMagnitude ~ MeanTemp + Year, data = bloom_magnitude_temp))
cor(bloom_magnitude_temp$MeanTemp, bloom_magnitude_temp$Year)

### Zooplankton
bloom_duration_zooplankton <- calculate_bloom_duration_zooplankton(
  zoop_daily %>% rename(Date = date, Biomass = DryBiomass),
  start_percentile = 25,
  end_percentile = 75)

calculate_bloom_magnitude_zooplankton_trapz <- function(zoop_daily, bloom_windows) {
df <- zoop_daily %>% # data has Date, Biomass
    mutate(Date = as.Date(Date),
      Year = year(Date),
      DayOfYear = yday(Date),
      t = as.numeric(Date)) %>%
    filter(!is.na(Date), !is.na(Biomass)) %>%
    arrange(Year, Date)
  
  out <- df %>%
    inner_join(bloom_windows %>% select(Year, BloomStartDay, BloomEndDay), by = "Year") %>%
    filter(DayOfYear >= BloomStartDay, DayOfYear <= BloomEndDay) %>%
    arrange(Year, t) %>%
    group_by(Year) %>%
    summarise(BloomMagnitude = ifelse(
        n() < 2, NA_real_,
        sum(diff(t) * (head(Biomass, -1) + tail(Biomass, -1)) / 2, na.rm = TRUE)),
      n_obs_in_window = n(),
      BloomStartDate = min(Date),
      BloomEndDate = max(Date),
      .groups = "drop")
  
  # merge back so you keep duration too
  bloom_windows %>%
    left_join(out, by = "Year")}

bloom_magnitude_zooplankton <- calculate_bloom_magnitude_zooplankton_trapz(
  zoop_daily %>% rename(Date = date, Biomass = DryBiomass),
  bloom_duration_zooplankton)

ggplot(bloom_magnitude_zooplankton%>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomMagnitude)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  labs(title = "Zooplankton Bloom Magnitude (Time-integrated Biomass)",
    x = "Year",
    y = "Bloom integrated biomass (units · day)") +
  theme_classic()

bloom_magnitude_temp_zoop <- bloom_magnitude_zooplankton %>%
  inner_join(bats_temp_FINAL, by = "Year")

ggplot(bloom_magnitude_temp_zoop, aes(x = MeanTemp, y = BloomMagnitude, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Zooplankton Bloom Magnitude vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Bloom integrated biomass (units · day)",
       color = "Year") +
  theme_classic()

summary (lm(BloomMagnitude ~ MeanTemp + Year, data = bloom_magnitude_temp_zoop))



# 5) Bloom Amplitude calculation: ----
### Phytoplankton ----
# calculating the maximum chl-a concentration during the bloom period (peak value) minus the baseline chl-a concentration (e.g., mean or median chl-a concentration during the non-bloom period).
calculate_bloom_amplitude <- function(norm_chl, bloom_windows) {
  
  df <- norm_chl %>%
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
  norm_chl %>% transmute(Date = Date, Biomass = Chl_mean_200m_mg_m3),
  bloom_duration_phytoplankton)

# visualize
ggplot(phyto_amplitude%>% filter(Year >= 1995, Year <= 2022), 
       aes(x = Year, y = BloomAmplitude)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Amplitude (Peak Chl-a)",
       x = "Year",
       y = "Bloom Peak Chl-a (mg m^-3)") +
  theme_classic()


### Zooplankton ----
zoop_amplitude <- calculate_bloom_amplitude(
  zoop_daily %>% rename(Date = date, Biomass = DryBiomass),
  bloom_duration_zooplankton)

#visualize
ggplot(zoop_amplitude%>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomAmplitude)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  labs(title = "Zooplankton Bloom Amplitude (Peak Biomass)",
       x = "Year",
       y = "Bloom Peak Biomass (µg/L)") +
  theme_classic()





# More Phenological Graphs -----

# Phytoplankton Bloom Start vs Zooplankton Bloom Start
phytoplankton_zooplankton_start <- bloom_start_phytoplankton %>%
  inner_join(bloom_start_zooplankton, by = "Year", suffix = c("_Phytoplankton", "_Zooplankton"))
# Scatter plot
ggplot(phytoplankton_zooplankton_start, aes(x = BloomStartDay_Phytoplankton, y = BloomStartDay_Zooplankton)) +
  geom_point(color = "navy") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Phytoplankton vs Zooplankton Bloom Start Days",
       x = "Phytoplankton Bloom Start Day of Year",
       y = "Zooplankton Bloom Start Day of Year") +
  theme_classic()

# Phytoplankton Bloom Peak vs Zooplankton Bloom Peak
phytoplankton_zooplankton_peak <- bloom_peak_phytoplankton %>%
  inner_join(bloom_peak_zooplankton, by = "Year", suffix = c("_Phytoplankton", "_Zooplankton"))
# Scatter plot
ggplot(phytoplankton_zooplankton_peak, aes(x = BloomPeakDay_Phytoplankton, y = BloomPeakDay_Zooplankton)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
  labs(title = "Phytoplankton vs Zooplankton Bloom Peak Days",
       x = "Phytoplankton Bloom Peak Day of Year",
       y = "Zooplankton Bloom Peak Day of Year") +
  theme_classic()

# Phytoplankton Bloom Duration vs Zooplankton Bloom Duration
phytoplankton_zooplankton_duration <- bloom_duration_phytoplankton %>%
  inner_join(bloom_duration_zooplankton, by = "Year", suffix = c("_Phytoplankton", "_Zooplankton"))
# Scatter plot
ggplot(phytoplankton_zooplankton_duration, aes(x = BloomDuration_Phytoplankton, y = BloomDuration_Zooplankton)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "Phytoplankton vs Zooplankton Bloom Durations",
       x = "Phytoplankton Bloom Duration (days)",
       y = "Zooplankton Bloom Duration (days)") +
  theme_classic()


# Phenology vs Temperature ----
library(readr)
bats_temp_FINAL <- read_csv("Data/bats_temp_FINAL.csv")
View(bats_temp_FINAL)

# Fixing the temperature dataframe and making it easier to work with 
bats_temp_FINAL <- bats_temp_FINAL %>%
  rename(Year = decimal_year_whole,
         MeanTemp = mean_temp)

### BLOOM START ----
# Phytoplankton Bloom Start vs Temperature (Year color gradient)
phyto_temp_plot <- bloom_start_phytoplankton %>%
  inner_join(bats_temp_FINAL, by = "Year")

ggplot(phyto_temp_plot,
       aes(x = MeanTemp,
           y = BloomStartDay,
           color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Phytoplankton Bloom Start vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Bloom Start Day of Year",
       color = "Year") +
  theme_classic()

# Zooplankton Bloom Start vs Temperature (Year color gradient)
zoop_temp_plot <- bloom_start_zooplankton %>%
  inner_join(bats_temp_FINAL, by = "Year")

ggplot(zoop_temp_plot,
       aes(x = MeanTemp,
           y = BloomStartDay,
           color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Zooplankton Bloom Start vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Bloom Start Day of Year",
       color = "Year") +
  theme_classic()

### BLOOM PEAK ----
#Phytoplankton Bloom Peak vs Temperature (Year color gradient)
phyto_peak_temp_plot <- bloom_peak_phytoplankton %>%
  inner_join(bats_temp_FINAL, by = "Year")
ggplot(phyto_peak_temp_plot,
       aes(x = MeanTemp,
           y = BloomPeakDay,
           color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Phytoplankton Bloom Peak vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Bloom Peak Day of Year",
       color = "Year") +
  theme_classic()
#Zooplankton Bloom Peak vs Temperature (Year color gradient)
zoop_peak_temp_plot <- bloom_peak_zooplankton %>%
  inner_join(bats_temp_FINAL, by = "Year")
ggplot(zoop_peak_temp_plot,
       aes(x = MeanTemp,
           y = BloomPeakDay,
           color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Zooplankton Bloom Peak vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Bloom Peak Day of Year",
       color = "Year") +
  theme_classic()

### BLOOM DURATION ----
#Phytoplankton Bloom Duration vs Temperature (Year color gradient)
phyto_duration_temp_plot <- bloom_duration_phytoplankton %>%
  inner_join(bats_temp_FINAL, by = "Year")
ggplot(phyto_duration_temp_plot,
       aes(x = MeanTemp,
           y = BloomDuration,
           color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Phytoplankton Bloom Duration vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Bloom Duration (days)",
       color = "Year") +
  theme_classic()
#Zooplankton Bloom Duration vs Temperature (Year color gradient)
zoop_duration_temp_plot <- bloom_duration_zooplankton %>%
  inner_join(bats_temp_FINAL, by = "Year")
ggplot(zoop_duration_temp_plot,
       aes(x = MeanTemp,
           y = BloomDuration,
           color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Zooplankton Bloom Duration vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Bloom Duration (days)",
       color = "Year") +
  theme_classic()

# Looking at potential mismatch ----
phyto_start <- bloom_start_phytoplankton %>%
  rename(PhytoStart = BloomStartDay)

zoo_start <- bloom_start_zooplankton %>%
  rename(ZooStart = BloomStartDay)

lag_df <- phyto_start %>%
  inner_join(zoo_start, by = "Year") %>%
  mutate(LagDays = ZooStart - PhytoStart)

ggplot(lag_df, aes(x = Year, y = LagDays)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Lag Between Phytoplankton and Zooplankton Bloom Start",
       x = "Year",
       y = "Lag (days): Zoo − Phyto") +
  theme_classic()

lag_temp_df <- lag_df %>%
  inner_join(bats_temp_FINAL, by = "Year")

ggplot(lag_temp_df,
       aes(x = MeanTemp, y = LagDays, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Phytoplankton–Zooplankton Bloom Lag vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Lag (days)") +
  theme_classic()

# Phenology vs Stratification ----
# Import stratification data
strat_index <- read_csv("Data/bats_stratification_index.csv")
#convert decimal year to whole
strat_index <- strat_index %>%
  mutate(Year = as.numeric(floor(decimal_year)))

#take yearly averages for stratification
strat_yearly <- strat_index %>%
  group_by(Year) %>%
  summarise(MeanStratification = mean(Stratification, na.rm = TRUE)) %>%
  ungroup()
# Phytoplankton Bloom Start vs Stratification
phyto_strat_plot <- bloom_start_phytoplankton %>%
  inner_join(strat_yearly, by = "Year")
ggplot(phyto_strat_plot,
       aes(x = MeanStratification,
           y = BloomStartDay,
           color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Phytoplankton Bloom Start vs Stratification",
       x = "Mean Annual Stratification (kg/m³)",
       y = "Bloom Start Day of Year",
       color = "Year") +
  theme_classic()

# Zooplankton Bloom Start vs Stratification
zoop_strat_plot <- bloom_start_zooplankton %>%
  inner_join(strat_yearly, by = "Year")
ggplot(zoop_strat_plot,
       aes(x = MeanStratification,
           y = BloomStartDay,
           color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Zooplankton Bloom Start vs Stratification",
       x = "Mean Annual Stratification (kg/m³)",
       y = "Bloom Start Day of Year",
       color = "Year") +
  theme_classic()

# Making individual datasets for zooplankton and phytoplankton, containing all info on indicies and temp. ----
### Phytoplankton dataset - combine all indices and temperature into one dataframe (column names in order of Year, MeanTemp, BloomStartDay, BloomPeakDay, BloomDuration) ----

bloom_start_phytoplankton <- bloom_start_phytoplankton %>%
  select(Year, BloomStartDay)%>%
  filter(Year >= 1995, Year <= 2022)
bloom_peak_phytoplankton <- bloom_peak_phytoplankton %>%
  select(Year, BloomPeakDay)%>%
  filter(Year >= 1995, Year <= 2022)
bloom_duration_phytoplankton <- bloom_duration_phytoplankton %>%
  select(Year, BloomDuration)%>%
  filter(Year >= 1995, Year <= 2022)

phytoplankton_phenology <- bats_temp_FINAL %>%
  select(Year, MeanTemp) %>%
  filter(Year >= 1995, Year <= 2022) %>%
  left_join(bloom_start_phytoplankton, by = "Year") %>%
  left_join(bloom_peak_phytoplankton, by = "Year") %>%
  left_join(bloom_duration_phytoplankton, by = "Year") %>%
  left_join(strat_yearly %>% select(Year, MeanStratification),
    by = "Year")
colnames(phytoplankton_phenology)

#get rid of NA's 
phytoplankton_phenology_clean <- phytoplankton_phenology %>%
  filter(!is.na(BloomStartDay) & !is.na(BloomPeakDay) & !is.na(BloomDuration))

write_csv(phytoplankton_phenology_clean, "Data/phytoplankton_phenology_clean.csv")

### Zooplankton dataset - containing info on BloomStartDay, BloomPeakDay, BloomDuration, MeanTemp ----
bloom_start_zooplankton <- bloom_start_zooplankton %>%
  select(Year, BloomStartDay)
bloom_peak_zooplankton <- bloom_peak_zooplankton %>%
  select(Year, BloomPeakDay)
bloom_duration_zooplankton <- bloom_duration_zooplankton %>%
  select(Year, BloomDuration)


zooplankton_phenology <- bats_temp_FINAL %>%
  select(Year, MeanTemp) %>%
  filter(Year >= 1995, Year <= 2022) %>%
  left_join(bloom_start_zooplankton, by = "Year") %>%
  left_join(bloom_peak_zooplankton, by = "Year") %>%
  left_join(bloom_duration_zooplankton, by = "Year")%>%
  left_join(strat_yearly %>% select(Year, MeanStratification),
    by = "Year")
colnames(zooplankton_phenology)

# get rid of NA's
zooplankton_phenology_clean <- zooplankton_phenology %>%
  filter(!is.na(BloomStartDay) & !is.na(BloomPeakDay) & !is.na(BloomDuration))

write_csv(zooplankton_phenology_clean, "Data/zooplankton_phenology_clean.csv")



## MORE VISUALISATION ## ----


#double axes plot with stratification and temperature against year
ggplot() +
  geom_point(data = bats_temp_FINAL, aes(x = Year, y = MeanTemp), color = "blue") + #add trendline 
  geom_smooth(data = bats_temp_FINAL, aes(x = Year, y = MeanTemp), method = "lm", se = FALSE, color = "lightblue") +
  geom_point(data = strat_yearly, aes(x = Year, y = MeanStratification), color = "red") +
  geom_smooth(data = strat_yearly, aes(x=Year, y=MeanStratification), method = "lm", se=FALSE, color= "pink")+
  scale_y_continuous( name = "Mean Temperature (°C)",
    sec.axis = sec_axis(~ ., name = "Mean Stratification (kg/m³)")) +
  labs(title = "Mean Temperature and Stratification Over Years") +
  theme_classic() +
  theme(axis.title.y.left = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))


# temperature vs year and temp vs strat parfrow 
par(mfrow = c(1,2))
plot(bats_temp_FINAL$Year, bats_temp_FINAL$MeanTemp, main = "Temperature vs Year", xlab = "Year", ylab = "Mean Temperature (°C)", col = "blue", type ="l")
plot(strat_yearly$Year, strat_yearly$MeanStratification, main = "Stratification vs Year", xlab = "Year", ylab = "Mean Stratification (kg/m³)", col = "red", type ="l")


#Visualising overall trends & creating combined dataset (long format) to visualise----
library(tidyverse)

# For zooplankton: wide to long format 
zooplankton_long <- zooplankton_phenology_clean %>%
  pivot_longer(cols = c(BloomStartDay, BloomPeakDay, BloomDuration),
    names_to = "PhenologyIndex",
    values_to = "Value") %>% #
  mutate(Group = "Zooplankton")

# For phytoplankton: wide to long format
phytoplankton_long <- phytoplankton_phenology_clean %>%
  pivot_longer(cols = c(BloomStartDay, BloomPeakDay, BloomDuration),
    names_to = "PhenologyIndex",
    values_to = "Value") %>%
  mutate(Group = "Phytoplankton")

# Combine both datasets
combined_data <- bind_rows(zooplankton_long, phytoplankton_long)

# Temperature vs Indice
ggplot(combined_data, aes(x = MeanTemp, y = Value, color = Group)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~PhenologyIndex, scales = "free_y") +
  theme_minimal() +
  labs(title = "Phenological Indices vs Temperature (Zooplankton & Phytoplankton)",
    x = "Mean Temperature",
    y = "Phenological Value") +
  scale_color_brewer(palette = "Set1")+
  theme_classic()

# Stratification vs Indice
ggplot(combined_data, aes(x = MeanStratification, y = Value, color = Group)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~PhenologyIndex, scales = "free_y") +
  theme_minimal() +
  labs(title = "Phenological Indices vs Stratification (Zooplankton & Phytoplankton)",
    x = "Mean Stratification",
    y = "Phenological Value") +
  scale_color_brewer(palette = "Set2")+
  theme_classic()

# Year vs Indice
ggplot(combined_data, aes(x = Year, y = Value, color = Group)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~PhenologyIndex, scales = "free_y") +
  theme_minimal() +
  labs(title = "Phenological Indices vs Year (Zooplankton & Phytoplankton)",
    x = "Year", y = "Phenological Value") +
  scale_color_brewer(palette = "Paired")+
  theme_classic()

