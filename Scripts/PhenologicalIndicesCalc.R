# This script calculates phenological indices for phytoplankton and zooplankton using the Ji et al. (2010) method,
# which includes bloom start date, bloom peak date, bloom duration, and bloom magnitude. 
# The script also visualizes these indices over time and explores their relationship with temperature.

# Importing datasets & libraries ----
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)

norm_chl <- read_csv("Data/norm_chl.csv") #phyto
zoop_daily <- read_csv("Data/zoop_daily.csv") #zooplankton
temperature <- read_csv("Data/BATS_temp_FINAL.csv") #SST


# Plotting phyto against zoop before phenology calculations ----

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
    mutate(Threshold = median(Biomass, na.rm = TRUE) + threshold_multiplier * sd(Biomass, na.rm = TRUE), #This creates one threshold per year.
           AboveThreshold = Biomass >= Threshold, #TRUE → bloom-level biomass, FALSE → not bloom-level
           RunID = cumsum(AboveThreshold != lag(AboveThreshold, default = first(AboveThreshold)))) %>%   # Create a run ID that increases every time AboveThreshold changes (so looks at today vs yesterday if T/F changes, starts a new run BUT if T/F stays the same, continues run)
    group_by(Year, RunID) %>% #counting how long each run lasts 
    mutate(RunLength = n()) %>% #number of days in each run
    ungroup() %>% 
    filter( AboveThreshold, RunLength >= duration) %>% #only keep days above threshold 
    group_by(Year) %>% 
    slice_min(Date,n=1,with_ties=FALSE)%>% # gives the first day of the first valid bloom each year. slice_min() is used instead of slice(1) to ensure that if there are multiple runs that meet the criteria, we only take the earliest one. If you use slice(1) after ungrouping, it would just take the first row of the entire dataset, which is not what we want. We want the first valid bloom start for each year, so we need to group by Year again and then take the minimum date within each year.
    #slice(1) %>% # gives the first day of the first valid bloom each year.
    select(Year, BloomStartDay = DayOfYear)
  
  return(bloom_starts)}
bloom_start_phytoplankton <- calculate_bloom_start_phytoplankton(norm_chl %>% rename(Date = Date,Biomass = Chl_mean_200m_mg_m3),
                                                                 threshold_multiplier = 0.5, duration = 2,min_obs_year = 6)

#visualize
ggplot(bloom_start_phytoplankton %>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomStartDay)) +
  geom_line(color = "cornflowerblue") +
  geom_point() +
  geom_smooth(method = "lm", color = "cornflowerblue", linetype = "dashed", se = FALSE) +
  labs(title = "Phytoplankton Bloom Start Day ",
       x = "Year",y = "Bloom Start Day of Year") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()

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

#The zooplankton method works cleanly in dplyr because it is based on cumulative thresholds, 
# while phytoplankton bloom detection requires identifying sustained consecutive periods, 
#which is why run-detection logic (and not simple mutate()) is needed.


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
  geom_line(color = "darkorchid") +
  geom_point() + 
  geom_smooth(method = "lm", color = "darkorchid2",linetype="dashed", se = FALSE) +
  labs(title = "Zooplankton Bloom Start Day Over Years",
       x = "Year", y = "Bloom Start Day of Year") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()


#visualise bloom start for phytoplankton and zooplankton together (dual axes and trendlines for each)
fill_colors<- c("Phytoplankton" = "lightskyblue1","Zooplankton"="lavender")

ggplot() +
  geom_line(data = bloom_start_phytoplankton, aes(x = Year, y = BloomStartDay, color = "Phytoplankton"), size = 0.5) +
  geom_point(data = bloom_start_phytoplankton, aes(x = Year, y = BloomStartDay, color = "Phytoplankton"), size = 2) +
  geom_smooth(data = bloom_start_phytoplankton, aes(x = Year, y = BloomStartDay, color = "Phytoplankton",fill="Phytoplankton"), method = "lm", se = TRUE, linetype = "dashed",size=0.8) +
  geom_line(data = bloom_start_zooplankton, aes(x = Year, y = BloomStartDay, color = "Zooplankton"), size = 0.5) +
  geom_point(data = bloom_start_zooplankton, aes(x = Year, y = BloomStartDay, color = "Zooplankton"), size = 2, shape=15) +
  geom_smooth(data = bloom_start_zooplankton, aes(x = Year, y = BloomStartDay, color = "Zooplankton",fill="Zooplankton"), method = "lm", se = TRUE, linetype = "dashed", size=0.8) +
  scale_color_manual(values = c("Phytoplankton" = "cornflowerblue", "Zooplankton" = "darkorchid")) +
  labs(title = "Bloom Start Day of Year for Phytoplankton and Zooplankton",
       x = "Year",
       y = "Bloom Start Day of Year",
       color = "Organism",
       fill="Organism") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+ 
  scale_fill_manual(values=fill_colors)+
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
  geom_line(color = "cornflowerblue", na.rm = TRUE) +
  geom_point() +
  geom_smooth(method = "lm", color = "cornflowerblue", linetype = "dashed", se = FALSE) +
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
  geom_line(color = "darkorchid") +
  geom_point() +
  geom_smooth(method = "lm", color = "darkorchid", linetype = "dashed", se = FALSE) +
  labs(title = "Zooplankton Bloom Peak Day Over Years",
       x = "Year",y = "Bloom Peak Day of Year") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  scale_y_continuous(breaks = c(100, 150,200,250,300)) +
  theme_classic()

# visualise bloom peak for phytoplankton and zooplankton together (dual axes and trendlines for each)
p2<-ggplot() +
  geom_line(data = bloom_peak_phytoplankton, aes(x = Year, y = BloomPeakDay, color = "Phytoplankton"), size = 0.5) +
  geom_point(data = bloom_peak_phytoplankton, aes(x = Year, y = BloomPeakDay, color = "Phytoplankton"), size = 2) +
  geom_smooth(data = bloom_peak_phytoplankton, aes(x = Year, y = BloomPeakDay, color = "Phytoplankton", fill= "Phytoplankton"), method = "lm", se = TRUE, linetype = "dashed",size=0.8) +
  geom_line(data = bloom_peak_zooplankton, aes(x = Year, y = BloomPeakDay, color = "Zooplankton"), size = 0.5) +
  geom_point(data = bloom_peak_zooplankton, aes(x = Year, y = BloomPeakDay, color = "Zooplankton"), size = 2, shape=15) +
  geom_smooth(data = bloom_peak_zooplankton, aes(x = Year, y = BloomPeakDay, color = "Zooplankton",fill= "Zooplankton"), method = "lm", se = TRUE, linetype = "dashed", size=0.8) +
  scale_color_manual(values = c("Phytoplankton" = "cornflowerblue", "Zooplankton" = "darkorchid")) +
  labs(title = "Bloom Peak Day of Year for Phytoplankton and Zooplankton",
       x = "Year",
       y = "Bloom Peak Day of Year",
       color="Organism",
       fill="Organism") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+ 
  scale_fill_manual(values=fill_colors)+
  theme_classic()


# 3) Bloom duration: number of days between bloom start and bloom end (date when chlorophyll concentration falls below the threshold value again).----
# Number of days between bloom “start” and “end” of season percentile thresholds.	

### Phytoplankton: Function to calculate bloom duration for phytoplankton ----
calculate_bloom_duration_phytoplankton <- function(data, percentile = 75) {
  data %>%
    mutate(Year = as.numeric(format(Date, "%Y")),
           DayOfYear = as.numeric(format(Date, "%j"))) %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(Threshold = quantile(Biomass, probs = percentile/100, na.rm = TRUE),
           Above = Biomass >= Threshold,
           RunID = cumsum(Above != lag(Above, default = first(Above)))) %>% # Create run ID for consecutive above-threshold days
    filter(Above) %>% # Only keep days above threshold
    group_by(Year, RunID) %>%
    summarise(Start = min(DayOfYear), End = max(DayOfYear),
              Duration = End - Start, .groups = "drop") %>%
    group_by(Year) %>%
    slice_max(Duration, n = 1, with_ties = FALSE) %>%
    transmute(Year, BloomStartDay = Start, BloomEndDay = End, BloomDuration = Duration) %>%
    ungroup() }
bloom_duration_phytoplankton <- calculate_bloom_duration_phytoplankton(norm_chl %>% rename(Biomass = Chl_mean_200m_mg_m3))

#visualize
ggplot(bloom_duration_phytoplankton%>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomDuration)) +
  geom_line(color = "cornflowerblue") +
  geom_point() +
  geom_smooth(method = "lm", color = "cornflowerblue", linetype = "dashed", se = FALSE) +
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
    summarise(BloomStartDay = DayOfYear[which(CumulativeBiomass >= StartThreshold)[1]],
      BloomEndDay = DayOfYear[which(CumulativeBiomass >= EndThreshold)[1]],
      BloomDuration = BloomEndDay - BloomStartDay) %>%
    ungroup()
  
  return(bloom_durations)
}
bloom_duration_zooplankton <- calculate_bloom_duration_zooplankton(zoop_daily %>% rename(Date = date, Biomass = DryBiomass))

#visualize
ggplot(bloom_duration_zooplankton%>% filter(Year >= 1995, Year <= 2022), 
       aes(x = Year, y = BloomDuration)) +
  geom_line(color = "darkorchid") +
  geom_point() +
  geom_smooth(method = "lm", color = "darkorchid", linetype = "dashed", se = FALSE) +
  labs(title = "Zooplankton Bloom Duration Over Years",
       x = "Year",y = "Bloom Duration (days)") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()

# visualise bloom duration for phytoplankton and zooplankton together (dual axes and trendlines for each)
p3<-ggplot() +
  geom_line(data = bloom_duration_phytoplankton, aes(x = Year, y = BloomDuration, color = "Phytoplankton"), size = 0.5) +
  geom_point(data = bloom_duration_phytoplankton, aes(x = Year, y = BloomDuration, color = "Phytoplankton"), size = 2) +
  geom_smooth(data = bloom_duration_phytoplankton, aes(x = Year, y = BloomDuration, color = "Phytoplankton",fill = "Phytoplankton"), method = "lm", se = TRUE, linetype = "dashed",size=0.8) +
  geom_line(data = bloom_duration_zooplankton, aes(x = Year, y = BloomDuration, color = "Zooplankton"), size = 0.5) +
  geom_point(data = bloom_duration_zooplankton, aes(x = Year, y = BloomDuration, color = "Zooplankton"), size = 2, shape=15) +
  geom_smooth(data = bloom_duration_zooplankton, aes(x = Year, y = BloomDuration, color = "Zooplankton",fill = "Zooplankton"), method = "lm", se = TRUE, linetype = "dashed", size=0.8) +
  scale_color_manual(values = c("Phytoplankton" = "cornflowerblue", "Zooplankton" = "darkorchid")) +
  labs(title = "Bloom Duration for Phytoplankton and Zooplankton",
       x = "Year",
       y = "Bloom Duration (days)",
       color = "Organism",
       fill="Organism") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+ 
  scale_fill_manual(values=fill_colors)+
  theme_classic()

(p1|p2|p3)

# 4) Bloom Magnitude: -----

### Phytoplankton:  ----
# calculating time-integrated biomass only during the bloom period (start–end window) (integrate chl-a concentration  between the bloom initiation and termination dates.)
bloom_windows_phyto <- bloom_duration_phytoplankton  # has Year, BloomStartDay, BloomEndDay

calculate_bloom_magnitude_trapz <- function(data, bloom_windows_phyto) {
  df <- data %>%   # Preparing a clean working table
    mutate(Date = as.Date(Date),
           Biomass = as.numeric(Biomass),
           Year = lubridate :: year(Date),
           DayOfYear = lubridate :: yday(Date),
           t=as.numeric(Date)) %>%  # numeric time in days for trapz
    filter(!is.na(Date), !is.na(Biomass)) %>%
    arrange(Year,Date)
mag<- df%>%
  inner_join(bloom_windows_phyto %>% select(Year, BloomStartDay, BloomEndDay), by = "Year") %>%
  filter(DayOfYear >= BloomStartDay, DayOfYear <= BloomEndDay) %>%
  arrange(Year,t)%>%
  group_by(Year) %>%
  summarise(BloomMagnitude = ifelse(
      n() < 2, NA_real_,  # need at least 2 points to integrate
      sum(diff(t) * (head(Biomass, -1) + tail(Biomass, -1)) / 2, na.rm = TRUE)),
    n_obs_in_window = n(),
    BloomStartDate = min(Date),
    BloomEndDate = max(Date),
    .groups = "drop")
bloom_windows_phyto %>% left_join(mag, by = "Year")}

phyto_data <-norm_chl %>% transmute(Date = Date, Biomass = Chl_mean_200m_mg_m3)
bloom_windows_phyto <- bloom_duration_phytoplankton
bloom_magnitude_phytoplankton <- calculate_bloom_magnitude_trapz(data=phyto_data, bloom_windows= bloom_windows_phyto)


ggplot(bloom_magnitude_phytoplankton %>%
    filter(Year >= 1995, Year <= 2022) %>%
    filter(!is.na(BloomMagnitude)),
  aes(x = Year, y = BloomMagnitude)) +
  geom_line(color="cornflowerblue") +
  geom_point() +
  geom_smooth(method="lm", linetype="dashed", se=FALSE, color="cornflowerblue") +
  labs(title="Phytoplankton Bloom Magnitude (Time-integrated Chl-a)",
       x="Year",
       y="Bloom integrated Chl-a (mg m^-3 · day)") +
  scale_x_continuous(breaks=c(1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous(breaks=c(5,10,15)) +
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
bloom_windows_zoop <- bloom_duration_zooplankton  # has Year, BloomStartDay, BloomEndDay

calculate_bloom_magnitude_zooplankton_trapz <- function(data, bloom_windows_zoop) {
df <- data %>% # data has Date, Biomass
    mutate(Date = as.Date(Date),
      Year = year(Date),
      Biomass = as.numeric(Biomass),
      DayOfYear = yday(Date),
      t = as.numeric(Date)) %>%
    filter(!is.na(Date), !is.na(Biomass)) %>%
    arrange(Year, Date)
  
  mag_z <- df %>%
    inner_join(bloom_windows_zoop %>% select(Year, BloomStartDay, BloomEndDay), by = "Year") %>%
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
  
  # merge back to keep duration too
  bloom_windows_zoop %>% left_join(mag_z, by = "Year")}

zoop_data<- zoop_daily %>% transmute(Date = date, Biomass = DryBiomass)
bloom_magnitude_zooplankton <- calculate_bloom_magnitude_zooplankton_trapz(data=zoop_data, bloom_windows= bloom_windows_zoop)

ggplot(bloom_magnitude_zooplankton%>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomMagnitude)) +
  geom_line(color='darkorchid') +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = TRUE, color='darkorchid', fill="lavender") +
  labs(title = "Zooplankton Bloom Magnitude (Time-integrated Biomass)",
    x = "Year",
    y = "Bloom integrated biomass (µg/L · day)") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
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

#combine phytoplankton and zooplankton bloom magnitude in one plot

ggplot() +
  geom_line(data = bloom_magnitude_phytoplankton, aes(x = Year, y = BloomMagnitude, color = "Phytoplankton"), size = 0.5) +
  geom_point(data = bloom_magnitude_phytoplankton, aes(x = Year, y = BloomMagnitude, color = "Phytoplankton"), size = 2) +
  geom_smooth(data = bloom_magnitude_phytoplankton, aes(x = Year, y = BloomMagnitude, color = "Phytoplankton"), method = "lm", se = FALSE, linetype = "dashed",size=0.8) +
  geom_line(data = bloom_magnitude_zooplankton, aes(x = Year, y = BloomMagnitude, color = "Zooplankton"), size = 0.5) +
  geom_point(data = bloom_magnitude_zooplankton, aes(x = Year, y = BloomMagnitude, color = "Zooplankton"), size = 2, shape=15) +
  geom_smooth(data = bloom_magnitude_zooplankton, aes(x = Year, y = BloomMagnitude, color = "Zooplankton"), method = "lm", se = FALSE, linetype = "dashed", size=0.8) +
  scale_color_manual(values = c("Phytoplankton" = "cornflowerblue", "Zooplankton" = "darkorchid")) +
  labs(title = "Bloom Magnitude for Phytoplankton and Zooplankton",
       x = "Year",
       y = "Bloom Magnitude (Time-integrated units · day)",
       color = "Organism") +
  scale_x_continuous(breaks=c(1995, 2000, 2005, 2010, 2015, 2020)) +
  theme_classic()

# 5) Bloom Amplitude calculation: ----
### Phytoplankton ----
# calculating the maximum chl-a concentration during the bloom period (peak value) minus the baseline chl-a concentration (e.g., mean or median chl-a concentration during the non-bloom period).
calculate_bloom_amplitude <- function(data, bloom_windows_phyto) {
  df <- data %>%
    mutate(Date = as.Date(Date),
      Year = lubridate::year(Date),
      DayOfYear = lubridate::yday(Date)) %>%
    filter(!is.na(Biomass))
  
  amp_phyto <- df %>%
    inner_join(bloom_windows_phyto %>% select(Year, BloomStartDay, BloomEndDay), by = "Year") %>%
    filter(DayOfYear >= BloomStartDay, DayOfYear <= BloomEndDay) %>%
    group_by(Year) %>%
    summarise(BloomAmplitude = max(Biomass, na.rm = TRUE), #splitting bloom period data into years and taking max biomass  in each year (this is amplitude)
      .groups = "drop")
  
  bloom_windows_phyto %>%left_join(amp_phyto, by = "Year")}

# run it
phyto_amplitude <- calculate_bloom_amplitude(phyto_data,bloom_windows_phyto)

# visualize
ggplot(phyto_amplitude%>% filter(Year >= 1995, Year <= 2022), 
       aes(x = Year, y = BloomAmplitude)) +
  geom_line(color='cornflowerblue') +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = TRUE, color='cornflowerblue', fill="lightskyblue1") +
  labs(title = "Phytoplankton Bloom Amplitude (Peak Chl-a)",
       x = "Year",
       y = "Bloom Peak Chl-a (mg m^-3)") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()


### Zooplankton ----
calculate_bloom_amplitude_zoop <- function(data, bloom_windows_zoop) {
  df<-data%>%
    mutate(Date=as.Date(Date),
           Biomass=as.numeric(Biomass),
           Year = year(Date),
           DayOfYear = yday(Date)) %>%
    filter(!is.na(Date), !is.na(Biomass))
  
amp_zoop<-df%>%
  inner_join(bloom_windows_zoop %>% select(Year, BloomStartDay, BloomEndDay), by = "Year") %>%
  filter(DayOfYear >= BloomStartDay, DayOfYear <= BloomEndDay) %>%
  group_by(Year) %>%
  summarise(BloomAmplitude = max(Biomass, na.rm = TRUE), .groups = "drop")

bloom_windows_zoop %>%left_join(amp_zoop, by = "Year")}

zoop_amplitude <- calculate_bloom_amplitude_zoop(zoop_data,bloom_windows_zoop)

#visualize
ggplot(zoop_amplitude%>% filter(Year >= 1995, Year <= 2022),
       aes(x = Year, y = BloomAmplitude)) +
  geom_line(color='darkorchid') +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = TRUE,color='darkorchid',fill="lavender") +
  labs(title = "Zooplankton Bloom Amplitude (Peak Biomass)",
       x = "Year",
       y = "Bloom Peak Biomass (µg/L)") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
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
bloom_magnitude_phytoplankton <- bloom_magnitude_phytoplankton %>%
  select(Year, BloomMagnitude) %>%
  filter(Year >= 1995, Year <= 2022)
bloom_amplitude_phytoplankton <- phyto_amplitude %>%
  select(Year, BloomAmplitude) %>%
  filter(Year >= 1995, Year <= 2022)

phytoplankton_phenology <- bats_temp_FINAL %>%
  select(Year, MeanTemp) %>%
  filter(Year >= 1995, Year <= 2016) %>%
  left_join(bloom_start_phytoplankton, by = "Year") %>%
  left_join(bloom_peak_phytoplankton, by = "Year") %>%
  left_join(bloom_duration_phytoplankton, by = "Year") %>%
  left_join(bloom_magnitude_phytoplankton, by = "Year") %>%
  left_join(bloom_amplitude_phytoplankton, by = "Year") %>%
  left_join(strat_yearly %>% select(Year, MeanStratification),
            by = "Year")
colnames(phytoplankton_phenology)

#get rid of NA's 
phytoplankton_phenology_clean <- phytoplankton_phenology %>%
  filter(!is.na(BloomStartDay) & !is.na(BloomPeakDay) & !is.na(BloomDuration) & !is.na(BloomMagnitude) & !is.na(BloomAmplitude))

write_csv(phytoplankton_phenology_clean, "Data/phytoplankton_phenology_clean.csv")

### Zooplankton dataset - containing info on BloomStartDay, BloomPeakDay, BloomDuration, MeanTemp ----
bloom_start_zooplankton <- bloom_start_zooplankton %>%
  select(Year, BloomStartDay)
bloom_peak_zooplankton <- bloom_peak_zooplankton %>%
  select(Year, BloomPeakDay)
bloom_duration_zooplankton <- bloom_duration_zooplankton %>%
  select(Year, BloomDuration)
bloom_magnitude_zooplankton <- bloom_magnitude_zooplankton %>%
  select(Year, BloomMagnitude)
bloom_amplitude_zooplankton <- zoop_amplitude %>%
  select(Year, BloomAmplitude)


zooplankton_phenology <- bats_temp_FINAL %>%
  select(Year, MeanTemp) %>%
  filter(Year >= 1995, Year <= 2016) %>%
  left_join(bloom_start_zooplankton, by = "Year") %>%
  left_join(bloom_peak_zooplankton, by = "Year") %>%
  left_join(bloom_duration_zooplankton, by = "Year")%>%
  left_join(bloom_magnitude_zooplankton, by = "Year") %>%
  left_join(bloom_amplitude_zooplankton, by = "Year") %>%
  left_join(strat_yearly %>% select(Year, MeanStratification),
            by = "Year")
colnames(zooplankton_phenology)

# get rid of NA's
zooplankton_phenology_clean <- zooplankton_phenology %>%
  filter(!is.na(BloomStartDay) & !is.na(BloomPeakDay) & !is.na(BloomDuration) & !is.na(BloomMagnitude) & !is.na(BloomAmplitude))

write_csv(zooplankton_phenology_clean, "Data/zooplankton_phenology_clean.csv")

# MISMATCH----
### Looking at mismatch in terms of bloom start ----
phyto_start <- bloom_start_phytoplankton %>%
  rename(PhytoStart = BloomStartDay)
zoo_start <- bloom_start_zooplankton %>%
  rename(ZooStart = BloomStartDay)

lag_start_df <- phyto_start %>%
  inner_join(zoo_start, by = "Year") %>%
  mutate(LagStartDays = ZooStart - PhytoStart)

lag_start_df_env <- lag_start_df %>%
  inner_join(bats_temp_FINAL, by = "Year")%>%
  inner_join(strat_yearly %>% select(Year, MeanStratification), by = "Year")

ggplot(lag_start_df, aes(x = Year, y = LagStartDays)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Lag Between Phytoplankton and Zooplankton Bloom Start",
       x = "Year",
       y = "Lag (days): Zoo − Phyto") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()


ggplot(lag_start_df_env, aes(x = MeanTemp, y = LagStartDays)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Lag Between Phytoplankton and Zooplankton Bloom Peak",
       x = "Mean Temperature",
       y = "Lag (days): Zoo − Phyto") +
  theme_classic()

summary(glm(LagStartDays~MeanTemp, data=lag_start_df_env))

ggplot(lag_start_df_env, aes(x = MeanStratification, y = LagStartDays)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Lag Between Phytoplankton and Zooplankton Bloom Start",
       x = "MeanStrat",
       y = "Lag (days): Zoo − Phyto") +
  theme_classic()


summary(glm(LagStartDays ~ Year,data = lag_start_df))
AIC(glm(LagStartDays ~ MeanTemp,data = lag_start_df_env))
AIC(glm(LagStartDays ~ MeanStratification,data = lag_start_df_env))


AIC(glm(LagStartDays ~ Year + MeanTemp, data = lag_start_df_env))
AIC(glm(LagStartDays ~ Year + MeanStratification, data = lag_start_df_env))

AIC(glm(LagStartDays ~ Year + MeanTemp + MeanStratification, data = lag_start_df_env))
AIC(glm(LagStartDays ~ Year + MeanTemp*MeanStratification, data = lag_start_df_env))
AIC(glm(LagStartDays ~ Year*MeanTemp*MeanStratification, data = lag_start_df_env))


AIC(glm(LagStartDays~MeanTemp, data=lag_start_df_env)) # Best AIC
AIC(glm(LagStartDays~MeanStratification, data=lag_start_df_env))
AIC(glm(LagStartDays~MeanStratification + MeanTemp, data=lag_start_df_env))
summary(glm(LagStartDays~MeanStratification*MeanTemp, data=lag_start_df_env)) #Second best AIC



ggplot(lag_start_df_env,aes(x = MeanTemp, y = LagStartDays, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Phytoplankton–Zooplankton Bloom Lag vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Lag (days)") +
  theme_classic()

###looking at mismatch in terms of peak day (when maximum occurs) ----
# important: also making sure missing dates for one are excluded in the other so we are comparing the same years
phyto_peak <- bloom_peak_phytoplankton %>%
  rename(PhytoPeak = BloomPeakDay)
zoo_peak <- bloom_peak_zooplankton %>%
  rename(ZooPeak = BloomPeakDay)

lag_peak_df <- phyto_peak %>%
  inner_join(zoo_peak, by = "Year") %>% # inner join keeps only years that exist in both datasets so ONLY shared years remain
  mutate(LagPeakDays = ZooPeak - PhytoPeak)
lag_peak_temp_df <- lag_peak_df %>%
  inner_join(bats_temp_FINAL, by = "Year")%>%
  inner_join(strat_yearly %>% select(Year, MeanStratification), by = "Year")

#lag peak x year
ggplot(lag_peak_df, aes(x = Year, y = LagPeakDays)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Lag Between Phytoplankton and Zooplankton Bloom Peak",
       x = "Year",
       y = "Lag (days): Zoo − Phyto") +
  scale_x_continuous(breaks = c(1995, 2000,2005, 2010, 2015, 2020))+
  theme_classic()

#lag peak x temp 
ggplot(lag_peak_temp_df, aes(x = MeanTemp, y = LagPeakDays)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Lag Between Phytoplankton and Zooplankton Bloom Peak",
       x = "Mean Temperature",
       y = "Lag (days): Zoo − Phyto") +
  theme_classic()

summary(glm(LagPeakDays~MeanTemp, data=lag_peak_temp_df))



plot(lm(LagPeakDays ~ Year*MeanTemp, data = lag_peak_temp_df))
summary(lm(LagPeakDays ~ Year, data= lag_peak_df))




summary(glm(LagPeakDays ~ MeanTemp, data = lag_peak_temp_df))
AIC(glm(LagPeakDays ~ Year + MeanTemp, data = lag_peak_temp_df))
AIC(glm(LagPeakDays ~ Year + MeanStratification, data = lag_peak_temp_df))

AIC(glm(LagPeakDays ~ Year + MeanTemp + MeanStratification, data = lag_peak_temp_df))
AIC(glm(LagPeakDays ~ Year + MeanTemp*MeanStratification, data = lag_peak_temp_df))
AIC(glm(LagPeakDays ~ Year*MeanTemp*MeanStratification, data = lag_peak_temp_df))


AIC(glm(LagPeakDays~MeanTemp, data=lag_peak_temp_df)) 
AIC(glm(LagPeakDays~MeanStratification, data=lag_peak_temp_df))
AIC(glm(LagPeakDays~MeanStratification + MeanTemp, data=lag_peak_temp_df))
summary(glm(LagPeakDays~MeanStratification*MeanTemp, data=lag_peak_temp_df)) # Best AIC

# Check the residuals of your best model
best_model <- lm(LagStartDays ~ MeanTemp, data = lag_start_df_env)

# checking autocorrelation 
acf(residuals(best_model), main="ACF of Model Residuals")
pacf(residuals(best_model), main="PACF of Model Residuals")
durbinWatsonTest(best_model)



ggplot(lag_peak_temp_df,
       aes(x = MeanTemp, y = LagPeakDays, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Phytoplankton–Zooplankton Bloom Peak Lag vs Temperature",
       x = "Mean Annual Temperature (°C)",
       y = "Lag (days)") +
  theme_classic()


# looking at relationship between phytoplankton and zooplankton bloom peak days (are they correlated? does one always occur after the other? etc.)
mismatch_peak <- bloom_peak_phytoplankton %>%
  rename(PhytoPeak = BloomPeakDay) %>%
  inner_join(bloom_peak_zooplankton %>%
      rename(ZooPeak = BloomPeakDay),
    by = "Year")

ggplot(mismatch_peak, aes(x = PhytoPeak, y = ZooPeak)) +
  geom_point(size = 3, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method="lm", se=FALSE, color="black")+
  labs(title = "Phytoplankton–Zooplankton Peak Timing Relationship",
    x = "Phytoplankton Peak Day",
    y = "Zooplankton Peak Day") +
  theme_classic()


mismatch_start <- bloom_start_phytoplankton %>%
  rename(PhytoStart = BloomStartDay) %>%
  inner_join(
    bloom_start_zooplankton %>%
      rename(ZooStart = BloomStartDay),
    by = "Year")
ggplot(mismatch_start, aes(x = PhytoStart, y = ZooStart)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method='lm',se=FALSE, color="black")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Phytoplankton–Zooplankton Start Timing Relationship",
    x = "Phytoplankton Start Day",
    y = "Zooplankton Start Day") +
  theme_classic()


summary(lm(ZooStart ~ PhytoStart , data = mismatch_start))
summary(lm(ZooPeak ~ PhytoPeak, data = mismatch_peak))

#adding MeanStratification and MeanTemperature to the start mismatch dataset
mismatch_start_env <- mismatch_start %>%
  inner_join(bats_temp_FINAL, by = "Year") %>%
  inner_join(strat_yearly %>% select(Year, MeanStratification), by = "Year")
summary(glm(ZooStart ~ PhytoStart + MeanStratification, data = mismatch_start_env))
summary(glm(ZooStart ~ PhytoStart* MeanStratification, data = mismatch_start_env))# better aic 

AIC(glm(ZooStart ~ PhytoStart, data = mismatch_start_env))
AIC(glm(ZooStart ~ PhytoStart + MeanStratification, data = mismatch_start_env))
AIC(glm(ZooStart ~ PhytoStart* MeanStratification, data = mismatch_start_env)) #lowest AIC, best model is the one with interaction between PhytoStart and MeanStratification
AIC(glm(ZooStart ~ PhytoStart + Year, data = mismatch_start_env))
AIC(glm(ZooStart ~ PhytoStart + Year + MeanTemp + MeanStratification, data = mismatch_start_env))
AIC(glm(ZooStart ~ PhytoStart + Year*MeanTemp*MeanStratification, data = mismatch_start_env))


#plot interaction between PhytoStart and MeanStratification on ZooStart
ggplot(mismatch_start_env, aes(x = PhytoStart, y = ZooStart, color = MeanStratification)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Interaction of Phytoplankton Start and Stratification on Zooplankton Start",
       x = "Phytoplankton Start Day",
       y = "Zooplankton Start Day",
       color = "Mean Stratification") +
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


# For zooplankton: wide to long format 
zooplankton_long <- zooplankton_phenology_clean %>%
  pivot_longer(cols = c(BloomStartDay, BloomPeakDay, BloomDuration),
    names_to = "PhenologyIndex",
    values_to = "Value") %>% #
  mutate(Organism = "Zooplankton")

zooplankton_long_community <- zooplankton_phenology_clean %>%
  pivot_longer(cols=c(BloomMagnitude,BloomAmplitude),
               names_to="PhenologyIndex",
               values_to="Value")%>%
  mutate(Organism = "Zooplankton")

# For phytoplankton: wide to long format
phytoplankton_long <- phytoplankton_phenology_clean %>%
  pivot_longer(cols = c(BloomStartDay, BloomPeakDay, BloomDuration),
    names_to = "PhenologyIndex",
    values_to = "Value") %>%
  mutate(Organism = "Phytoplankton")

phytoplankton_long_community <- phytoplankton_phenology_clean %>%
  pivot_longer(cols=c(BloomMagnitude,BloomAmplitude),
               names_to="PhenologyIndex",
               values_to="Value")%>%
  mutate(Organism = "Phytoplankton")

# Combine both datasets
combined_data <- bind_rows(zooplankton_long, phytoplankton_long)
combined_data <- combined_data %>%
  mutate(PhenologyIndex = factor(PhenologyIndex,
                                 levels = c("BloomStartDay", "BloomPeakDay", "BloomDuration")))

combined_data_community <- bind_rows(zooplankton_long_community, phytoplankton_long_community)
## Modifying to have both phytoplankton and zooplankton on the same y-axis for the community size indicators (magnitude and amplitude) 
df <- combined_data_community %>% filter(Year >= 1995, Year <= 2022) # choose ranges (computing from filtered dataset)
phy_rng  <- range(df$Value[df$Organism == "Phytoplankton"], na.rm = TRUE) # range of phytoplankton values (min and max) for the community size indicators
zoo_rng  <- range(df$Value[df$Organism == "Zooplankton"],  na.rm = TRUE) # range of zooplankton values (min and max) for the community size indicators
to_phy <- function(z) (z - zoo_rng[1]) / (zoo_rng[2] - zoo_rng[1]) * (phy_rng[2] - phy_rng[1]) + phy_rng[1] # convert zooplankton values to the phytoplankton scale for plotting on the same y-axis
to_zoo <- function(p) (p - phy_rng[1]) / (phy_rng[2] - phy_rng[1]) * (zoo_rng[2] - zoo_rng[1]) + zoo_rng[1] # convert phytoplankton values to the zooplankton scale for plotting on the same y-axis
df2 <- df %>% mutate(Value_plot = ifelse(Organism == "Zooplankton", to_phy(Value), Value)) # create a new column for plotting where zooplankton values are converted to the phytoplankton scale, while phytoplankton values remain unchanged

# Temperature vs Indices ----
# the original 3
Organism_Colors<- c("Phytoplankton" = "lightskyblue1","Zooplankton"="lavender")
ggplot(combined_data%>% filter(Year >= 1995, Year <= 2022), aes(x = MeanTemp, y = Value, color = Organism, fill=Organism)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE,linetype = "dashed") +
  facet_wrap(~PhenologyIndex, scales = "free_y") +
  theme_minimal() +
  labs(title = "Phenological Indices vs Temperature (Zooplankton & Phytoplankton)",
       x = "Mean Temperature",
       y = "Phenological Value") +
  scale_fill_manual(values = Organism_Colors)+
  scale_color_manual(values = c("Phytoplankton" = "cornflowerblue", "Zooplankton" = "darkorchid"))+
  theme_classic()

# community size indicators (magnitude and amplitude)
ggplot(df2, aes(x = MeanTemp, y = Value_plot, color = Organism,fill=Organism)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +
  facet_wrap(~PhenologyIndex, scales = "free_y") +
  scale_y_continuous(name = expression(bold("Phytoplankton")~ "(amplitude:mg m"^{-3} ~ ", magnitude: mg m"^{-3} ~ "\u00B7 day)"),
                     sec.axis = sec_axis(~ to_zoo(.),
                                         name = expression(bold("Zooplankton") ~ "(amplitude:" * mu * "g L"^{-1} ~ ",magnitude:" * mu * "g L"^{-1} ~ "\u00B7 day)"))) +
  labs(title = "Phenological Indices vs Temperature (Zooplankton & Phytoplankton)",
       x = "Mean Temperature") +
  scale_color_manual(values = c("Phytoplankton" = "cornflowerblue","Zooplankton"  = "darkorchid")) +
  scale_fill_manual(values = Organism_Colors)+
  theme_classic()


# Stratification vs Indice ----
# the original 3
p3<-ggplot(combined_data %>%filter(Year>=1995, Year <=2022), aes(x = MeanStratification, y = Value, color = Organism,fill=Organism)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +
  facet_wrap(~PhenologyIndex, scales = "free_y") +
  theme_minimal() +
  labs(title = "Phenological Indices vs Stratification (Zooplankton & Phytoplankton)",
    x = "Mean Stratification",
    y = "Phenological Value") +
  scale_color_manual(values = c("Phytoplankton" = "cornflowerblue","Zooplankton" = "darkorchid"))+
  scale_fill_manual(values = Organism_Colors)+
  theme_classic()

# community size indicators 
ggplot(df2, aes(x = MeanStratification, y = Value_plot, color = Organism, fill=Organism)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +
  facet_wrap(~PhenologyIndex, scales = "free_y") +
  scale_y_continuous(name = expression(bold("Phytoplankton")~ "(amplitude:mg m"^{-3} ~ ", magnitude: mg m"^{-3} ~ "\u00B7 day)"),
    sec.axis = sec_axis(~ to_zoo(.),
                        name = expression(bold("Zooplankton") ~ "(amplitude:" * mu * "g L"^{-1} ~ ",magnitude:" * mu * "g L"^{-1} ~ "\u00B7 day)"))) +
 # scale_y_continuous(name = "Phytoplankton (mg m^-3 :amplitude) (mg m^-3 * day :magnitude)",
  #                   sec.axis = sec_axis(~ to_zoo(.), name = "Zooplankton (µg/L : amplitude) (µg/L * day :magnitude)")) +
  labs(title = "Phenological Indices vs Stratification (Zooplankton & Phytoplankton)",
       x = "Mean Stratification") +
  scale_color_manual(values = c("Phytoplankton" = "cornflowerblue","Zooplankton"  = "darkorchid")) +
  scale_fill_manual(values = Organism_Colors)+
  theme_classic()



(p3 /p4)



 
# Year vs Indice - just to check 
ggplot(combined_data, aes(x = Year, y = Value, color = Organism)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~PhenologyIndex, scales = "free_y") +
  theme_minimal() +
  labs(title = "Phenological Indices vs Year (Zooplankton & Phytoplankton)",
    x = "Year", y = "Phenological Value") +
  scale_color_brewer(palette = "Paired")+
  theme_classic()

