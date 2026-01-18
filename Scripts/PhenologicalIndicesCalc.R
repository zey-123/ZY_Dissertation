# Importing datasets & libraries ----
library(readr)

norm_chl <- read_csv("Data/norm_chl.csv")
zoop_daily <- read_csv("Data/zoop_daily.csv")
temperature <- read_csv("Data/BATS_temp_FINAL.csv")

# Calculating Phenological Indices (Ji et al., 2010)


# 1) Bloom start date: date when chlorophyll concentration first exceeds a threshold value (e.g., mean + 1 standard deviation of the annual cycle) and remains above it for a specified duration (e.g., 7 days).----
### Phytoplankton: Year day when biomass rise above certain threshold value; often used for indexing phytoplankton.	----
# The function finds the first day each year when biomass exceeds a year-specific threshold for at least a set number of consecutive days.
calculate_bloom_start_phytoplankton <- function(data, threshold_multiplier = 1, duration = 2) { # threshold_multiplier: number of standard deviations above mean; duration: number of consecutive days (“Due to the temporal resolution of the dataset, a minimum duration of two consecutive observations was required…)
  data <- data %>%
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

bloom_start_phytoplankton <- calculate_bloom_start_phytoplankton(norm_chl %>% rename(Date = Date,Biomass = Chl_mean_200m_mg_m3))
# View results
print(bloom_start_phytoplankton)
#visualize
ggplot(bloom_start_phytoplankton, aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "navy") +
  geom_point() +
  geom_smooth(method = "lm", color = "navy", linetype = "dashed",se = FALSE) +
  labs(title = "Phytoplankton Bloom Start Day Over Years",
       x = "Year",
       y = "Bloom Start Day of Year") +
  theme_classic()

  
#The zooplankton method works cleanly in dplyr because it is based on cumulative thresholds, 
# while phytoplankton bloom detection requires identifying sustained consecutive periods, 
#which is why run-detection logic (and not simple mutate()) is needed.


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
# View results
print(bloom_start_growth_rate_phytoplankton)
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
# View results
print(bloom_peak_phytoplankton)
#visualize
ggplot(bloom_peak_phytoplankton, aes(x = Year, y = BloomPeakDay)) +
  geom_line(color = "navy", na.rm = TRUE) +
  geom_point() +
  geom_smooth(method = "lm", color = "navy", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Peak Day Over Years",
       x = "Year",
       y = "Bloom Peak Day of Year") +
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
# View results
print(bloom_duration_phytoplankton)
#visualize
ggplot(bloom_duration_phytoplankton, aes(x = Year, y = BloomDuration)) +
  geom_line(color = "navy") +
  geom_point() +
  geom_smooth(method = "lm", color = "navy", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Duration Over Years",
       x = "Year",
       y = "Bloom Duration (days)") +
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
  select(Year, BloomStartDay)
bloom_peak_phytoplankton <- bloom_peak_phytoplankton %>%
  select(Year, BloomPeakDay)
bloom_duration_phytoplankton <- bloom_duration_phytoplankton %>%
  select(Year, BloomDuration)


phytoplankton_phenology <- bats_temp_FINAL %>%
  select(Year, MeanTemp)
phytoplankton_phenology <- phytoplankton_phenology %>%
  left_join(bloom_start_phytoplankton, by = "Year") %>%
  left_join(bloom_peak_phytoplankton, by = "Year") %>%
  left_join(bloom_duration_phytoplankton, by = "Year")

#get rid of NA's and start from year 1995 for comparability with zooplankton
phytoplankton_phenology_clean <- phytoplankton_phenology %>%
  filter(!is.na(BloomStartDay) & !is.na(BloomPeakDay) & !is.na(BloomDuration)
         & Year >= 1995)



### Zooplankton dataset - containing info on BloomStartDay, BloomPeakDay, BloomDuration, MeanTemp ----
bloom_start_zooplankton <- bloom_start_zooplankton %>%
  select(Year, BloomStartDay)
bloom_peak_zooplankton <- bloom_peak_zooplankton %>%
  select(Year, BloomPeakDay)
bloom_duration_zooplankton <- bloom_duration_zooplankton %>%
  select(Year, BloomDuration)


zooplankton_phenology <- bats_temp_FINAL %>%
  select(Year, MeanTemp)
zooplankton_phenology <- zooplankton_phenology %>%
  left_join(bloom_start_zooplankton, by = "Year") %>%
  left_join(bloom_peak_zooplankton, by = "Year") %>%
  left_join(bloom_duration_zooplankton, by = "Year")

zooplankton_phenology_clean <- zooplankton_phenology %>%
  filter(!is.na(BloomStartDay) & !is.na(BloomPeakDay) & !is.na(BloomDuration))


# Statistical Analyses -----

# Linear models for Phenology Trends vs Year ----
lm_phytoplankton_start <- lm(BloomStartDay  ~ Year, data=bloom_start_phytoplankton)
lm_zooplankton_start <-lm(BloomStartDay  ~ Year, data=bloom_start_zooplankton)

anova(lm_phytoplankton_start)
anova(lm_zooplankton_start)

shapiro.test(resid(lm_phytoplankton_start))
shapiro.test(resid(lm_zooplankton_start))


lm_phytoplankton_peak <-lm(BloomPeakDay ~ Year, data=bloom_peak_phytoplankton)
lm_zooplankton_peak <-lm(BloomPeakDay ~ Year, data=bloom_peak_zooplankton)

lm_phytoplankton_duration <-lm(BloomDuration ~ Year, data=bloom_duration_phytoplankton)
lm_zooplankton_duration <-lm(BloomDuration ~ Year, data=bloom_duration_zooplankton)

### Visualizing linear models and checking assumptions ----
par(mfrow = c(2,2))
plot(lm_phytoplankton_start)
plot(lm_zooplankton_start)

plot(lm_phytoplankton_peak)
plot(lm_zooplankton_peak)

plot(lm_phytoplankton_duration)
plot(lm_zooplankton_duration)

hist(bloom_start_phytoplankton$BloomStartDay,col="skyblue",breaks= 10, xlab="BloomStartDay")
hist(bloom_start_zooplankton$BloomStartDay,col="skyblue",breaks= 10,xlab="BloomStartDay")
shapiro.test(bloom_start_phytoplankton$BloomStartDay)
shapiro.test(bloom_start_zooplankton$BloomStartDay)


hist(bloom_peak_phytoplankton$BloomPeakDay,col="skyblue",breaks= 10,xlab="BloomPeakDay")
hist(bloom_peak_zooplankton$BloomPeakDay,col="skyblue",breaks= 10,xlab="BloomPeakDay")
shapiro.test(bloom_peak_phytoplankton$BloomPeakDay)
shapiro.test(bloom_peak_zooplankton$BloomPeakDay)

hist(bloom_duration_phytoplankton$BloomDuration,col="skyblue",breaks= 10,xlab="BloomDuration")
hist(bloom_duration_zooplankton$BloomDuration,col="skyblue",breaks= 10,xlab="BloomDuration")
shapiro.test(bloom_duration_phytoplankton$BloomDuration)
shapiro.test(bloom_duration_zooplankton$BloomDuration)


# Linear models for Phenology vs Temp -----
# Phytoplankton
lm_phyto_bloomstart_temp <- lm(BloomStartDay ~ MeanTemp, data=phytoplankton_phenology_clean)
lm_phyto_bloompeak_temp <-lm(BloomPeakDay ~ MeanTemp, data=phytoplankton_phenology_clean)
lm_phyto_bloomduration_temp <-lm(BloomDuration ~ MeanTemp, data=phytoplankton_phenology_clean)

plot(lm_phyto_bloomstart_temp)
plot(lm_phyto_bloompeak_temp)
plot(lm_phyto_bloomduration_temp )

shapiro.test(resid(lm_phyto_bloomstart_temp))
shapiro.test(resid(lm_phyto_bloompeak_temp))
shapiro.test(resid(lm_phyto_bloomduration_temp ))

# Zooplankton
lm_zoop_bloomstart_temp <- lm(BloomStartDay ~ MeanTemp, data=zooplankton_phenology_clean)
lm_zoop_bloompeak_temp <- lm(BloomPeakDay ~ MeanTemp, data=zooplankton_phenology_clean)
lm_zoop_bloomduration_temp <- lm(BloomDuration ~ MeanTemp, data=zooplankton_phenology_clean)

plot(lm_zoop_bloomstart_temp)
plot(lm_zoop_bloompeak_temp)
plot(lm_zoop_bloomduration_temp )


shapiro.test(resid(lm_zoop_bloomstart_temp))
shapiro.test(resid(lm_zoop_bloompeak_temp))
shapiro.test(resid(lm_zoop_bloomduration_temp ))


### Checking residual autocorrelation ----
acf(residuals(lm_phytoplankton_start)) #Blue dashed lines = 95% confidence limits, Bars outside those lines = significant autocorrelation
  #Residuals are mostly independent, with weak, short-memory autocorrelation.
pacf(residuals(lm_phytoplankton_start)) 
  #There is some short-lag structure, but it is weak and not persistent.

lmtest::dwtest(lm_phytoplankton_start)
lmtest::dwtest(lm_zooplankton_start)
lmtest::dwtest(lm_phytoplankton_peak)
lmtest::dwtest(lm_zooplankton_peak) #problem DW<2 (positive autocorrelation)
lmtest::dwtest(lm_phytoplankton_duration) #problem DW<2 (positive autocorrelation)
lmtest::dwtest(lm_phytoplankton_duration) #problem DW<2 (positive autocorrelation)


#“Autocorrelation in model residuals was assessed using autocorrelation (ACF) and partial autocorrelation (PACF) functions. 
# Residuals from ordinary least squares models exhibited weak short-lag autocorrelation, particularly at lag 2, indicating partial violation of independence assumptions. 
#To account for temporal dependence, all subsequent analyses were conducted using generalized least squares models with a first-order autoregressive [AR(1)] error structure.”


