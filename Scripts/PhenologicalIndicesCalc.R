# Importing datasets & libraries ----
library(readr)
norm_chl <- read_csv("Data/norm_chl.csv")
zoop_daily <- read_csv("Data/zoop_daily.csv")

# Calculating Phenological Indices (Ji et al., 2010)

# 1) Bloom start date: date when chlorophyll concentration first exceeds a threshold value (e.g., mean + 1 standard deviation of the annual cycle) and remains above it for a specified duration (e.g., 7 days).----
### Phytoplankton: Year day when biomass rise above certain threshold value; often used for indexing phytoplankton.	----

calculate_bloom_start_phytoplankton <- function(data, threshold_multiplier = 1, duration = 7) { # threshold_multiplier: number of standard deviations above mean; duration: number of consecutive days
  data <- data %>%
    mutate(Year = as.numeric(format(Date, "%Y")),
           DayOfYear = as.numeric(format(Date, "%j")))
  
  bloom_starts <- data %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(Threshold = mean(Biomass, na.rm = TRUE) + threshold_multiplier * sd(Biomass, na.rm = TRUE),
           AboveThreshold = Biomass > Threshold,
           ConsecutiveDays = rle(AboveThreshold)$lengths * rle(AboveThreshold)$values) %>%
    filter(ConsecutiveDays >= duration & AboveThreshold) %>%
    slice(1) %>%
    select(Year, BloomStartDay = DayOfYear)
  
  return(bloom_starts)
}
bloom_start_phytoplankton <- calculate_bloom_start_phytoplankton(norm_chl %>% rename(Date = yyyymmd, Biomass = Chl_mean_200m))
# View results
print(bloom_start_phytoplankton)
#visualize
ggplot(bloom_start_phytoplankton, aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "navy") +
  geom_point() +
  labs(title = "Phytoplankton Bloom Start Day Over Years",
       x = "Year",
       y = "Bloom Start Day of Year") +
  theme_minimal()
  



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
# View results
print(bloom_start_zooplankton)
# visualize
ggplot(bloom_start_zooplankton, aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "purple") +
  geom_point() + 
  geom_smooth(method = "lm", color = "purple", linetype = "dashed",se = FALSE) +
  labs(title = "Zooplankton Bloom Start Day Over Years",
       x = "Year",
       y = "Bloom Start Day of Year") +
  theme_classic()

# 2) Bloom peak: date when chlorophyll concentration reaches its maximum value during the bloom period.----

### Phytoplankton: Year day with highest biomass at a defined period; Used for indexing phytoplankton and zooplankton.	----
# Function to calculate bloom peak date for phytoplankton
calculate_bloom_peak_phytoplankton <- function(data) {
  data <- data %>%
    mutate(Year = as.numeric(format(Date, "%Y")),
           DayOfYear = as.numeric(format(Date, "%j")))
  
  bloom_peaks <- data %>%
    group_by(Year) %>%
    filter(Biomass == max(Biomass, na.rm = TRUE)) %>%
    select(Year, BloomPeakDay = DayOfYear)
  
  return(bloom_peaks)
}
bloom_peak_phytoplankton <- calculate_bloom_peak_phytoplankton(norm_chl %>% rename(Date = yyyymmd, Biomass = Chl_mean_200m))
# View results
print(bloom_peak_phytoplankton)
#visualize
ggplot(bloom_peak_phytoplankton, aes(x = Year, y = BloomPeakDay)) +
  geom_line(color = "navy") +
  geom_point() +
  geom_smooth(method = "lm", color = "navy", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Peak Day Over Years",
       x = "Year",
       y = "Bloom Peak Day of Year") +
  theme_classic()

### Zooplankton: Function to calculate bloom peak date for zooplankton ----
calculate_bloom_peak_zooplankton <- function(data) { # Function to calculate bloom peak date for zooplankton
  data <- data %>% # Rename columns to match function
    mutate(Year = as.numeric(format(Date, "%Y")), # Extract year
           DayOfYear = as.numeric(format(Date, "%j"))) # Extract day of year
  
  bloom_peaks <- data %>% # Calculate bloom peak date
    group_by(Year) %>% # Group by year
    filter(Biomass == max(Biomass, na.rm = TRUE)) %>% # Find the day with maximum biomass
    select(Year, BloomPeakDay = DayOfYear) # Select relevant columns
  
  return(bloom_peaks)
}
bloom_peak_zooplankton <- calculate_bloom_peak_zooplankton(zoop_daily %>% rename(Date = date, Biomass = DryBiomass))
# View results
print(bloom_peak_zooplankton)

#visualize
ggplot(bloom_peak_zooplankton, aes(x = Year, y = BloomPeakDay)) +
  geom_line(color = "purple") +
  geom_point() +
  geom_smooth(method = "lm", color = "purple", linetype = "dashed", se = FALSE) +
  labs(title = "Zooplankton Bloom Peak Day Over Years",
       x = "Year",
       y = "Bloom Peak Day of Year") +
  theme_classic()

summary(lm(BloomPeakDay ~ Year, data = bloom_peak_zooplankton))


# 3) Bloom duration: number of days between bloom start and bloom end (date when chlorophyll concentration falls below the threshold value again).----
# Number of days between bloom “start” and “end” of season percentile thresholds.	

### Phytoplankton: Function to calculate bloom duration for phytoplankton ----


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
      BloomDuration = BloomEndDay - BloomStartDay
    ) %>%
    ungroup()
  
  return(bloom_durations)
}
bloom_duration_zooplankton <- calculate_bloom_duration_zooplankton(zoop_daily %>% rename(Date = date, Biomass = DryBiomass))
# View results
print(bloom_duration_zooplankton)
#visualize
ggplot(bloom_duration_zooplankton, aes(x = Year, y = BloomDuration)) +
  geom_line(color = "purple") +
  geom_point() +
  geom_smooth(method = "lm", color = "purple", linetype = "dashed", se = FALSE) +
  labs(title = "Zooplankton Bloom Duration Over Years",
       x = "Year",
       y = "Bloom Duration (days)") +
  theme_classic()
