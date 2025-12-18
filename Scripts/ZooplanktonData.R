## Load necessary libraries
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)

# Step 1: Load the zooplankton dataset ----
zooplankton<- read_excel("Data/BATS_zooplankton (1).xlsx",na="-999")
View(zooplankton)

# Step 2: Check column names ----
# Getting rid of the first 34 rows
zooplankton<- zooplankton[-c(1:34), ]

#making row 1 the column headers and getting rid of row 1
colnames(zooplankton) <- as.character(unlist(zooplankton[1, ]))
zooplankton <- zooplankton[-1, ]
colnames(zooplankton)

#Step 3: Convert date to proper Date format ----
#date is written as yyyymmdd e.g. 19940406 on column 2 
zooplankton$date <- as.Date(as.character(zooplankton$date), format="%Y%m%d")

#Step 4: Depth integration - Filter to only depths ≤ 200 m ----
# Convert depth column to numeric
zooplankton$max_depth <- as.numeric(zooplankton$max_depth)
# Filter rows where max depth is less than or equal to 200 m
zooplankton_filtered <- zooplankton %>%
  filter(max_depth <= 200)

# Visualizing ----
# a) Time-series plot of raw time-series — can see seasonal cycles and long-term changes.----

# Aggregation by date for visualization of raw data
#rename column 23 for clarity change from 'total_dry_weight/volume_for_all_size_fractions_200m' to TotalDryNorm200
colnames(zooplankton_filtered)[23] <- "TotalDryNorm200"
colnames(zooplankton_filtered)
# Convert biomass column to numeric
zooplankton_filtered$TotalDryNorm200 <- as.numeric(zooplankton_filtered$TotalDryNorm200)


# Aggregate to one value per date (mean or sum, depending on definition)
zoop_daily <- zooplankton_filtered %>%
  group_by(date) %>% #Take all the rows that have the same date and treat them as one group. 
  summarise(DryBiomass = mean(TotalDryNorm200, na.rm = TRUE)) %>% # average of TotalDryNorm200 for that day.
  ungroup()

# IQR test to statistically check for outliers (without assuming normality) ----
zoop <- zoop_daily$DryBiomass

Q1 <- quantile(zoop, 0.25, na.rm = TRUE)
Q3 <- quantile(zoop, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

upper_bound <- Q3 + 1.5 * IQR_val
lower_bound <- Q1 - 1.5 * IQR_val

outliers_iqr <- zoop_daily %>%
  filter(DryBiomass > upper_bound |
           DryBiomass < lower_bound)
outliers_iqr

#Remove outliers based on IQR
zoop_daily <- zoop_daily %>%
  filter(DryBiomass <= upper_bound &
           DryBiomass >= lower_bound)


plot(zoop_daily$date,zoop_daily$DryBiomass, type="l")
     
ggplot(zoop_daily, aes(x = date, y = DryBiomass)) +
  geom_line(color = "darkblue") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Zooplankton Biomass (0–200m) Over Time at BATS",
       x = "Date",
       y = "Dry weight (mg/m³, normalized to 200m)") +
  theme_classic()

#b) Seasonal pattern (montly averages) -----
zoop_daily$Month <- format(zoop_daily$date, "%m") # Extract month as numeric
zoop_daily$Month <- as.numeric(zoop_daily$Month) # Convert to numeric
zoop_monthly <- zoop_daily %>% 
  group_by(Month) %>% 
  summarise(MonthlyAvgBiomass = mean(DryBiomass, na.rm = TRUE)) %>% 
  ungroup()
# Plot monthly averages
ggplot(zoop_monthly, aes(x = Month, y = MonthlyAvgBiomass)) + # x is month (1-12)
  geom_line(color = "darkgreen") +
  geom_point() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Monthly Mean Zooplankton Biomass (0–200m) at BATS",
       x = "Month",
       y = "Average Dry weight (mg/m³, normalized to 200m)") +
  theme_classic()

# c) Annual pattern (yearly averages) ----
zoop_daily$Year <- format(zoop_daily$date, "%Y") # Extract year
zoop_daily$Year <- as.numeric(zoop_daily$Year) # Convert to numeric
zoop_yearly <- zoop_daily %>% 
  group_by(Year) %>% 
  summarise(YearlyAvgBiomass = mean(DryBiomass, na.rm = TRUE)) %>%  #calculating avg biomass per year
  ungroup()
# Plot yearly averages
ggplot(zoop_yearly, aes(x = Year, y = YearlyAvgBiomass)) +
  geom_line(color = "purple") +
  geom_point() +
  labs(title = "Annual Mean Zooplankton Biomass (0–200m) at BATS",
       x = "Year",
       y = "Average Dry weight (mg/m³, normalized to 200m)") +
  theme_classic()



#save zoop_daily to dataset folder - github
write.csv(zoop_daily, "zoop_daily.csv", row.names = FALSE)

# Depth integrate (sum biomass per sampling event) ----
# Adjust biomass column name accordingly
zooplankton_integrated <- zooplankton_filtered %>%
  group_by(date) %>%
  summarise(TotalBiomass_0_200m = sum(Biomass_mg_m3, na.rm = TRUE)) %>%
  ungroup()

# Save output for later analysis -----
write_csv(zoop_integrated, "zooplankton_integrated_0_200m.csv")
# Now zoop_integrated has one row per date with summed biomass for top 200 m

# Visualizing the data----
ggplot(zoop_integrated, aes(x = Date, y = TotalBiomass_0_200m)) +
  geom_line(color = "blue") +
  labs(title = "Zooplankton Biomass (0-200m) Over Time",
       x = "Date",
       y = "Total Biomass (mg/m³)") +
  theme_minimal()
# This plot shows trends in zooplankton biomass over time in the upper 200 m






#Identify timing of onset, peak, and duration of biomass blooms each year → plot trends across years.----
# Calculate annual metrics
zoop_metrics <- zoop_integrated %>%
  mutate(Year = as.numeric(format(Date, "%Y"))) %>%
  group_by(Year) %>%
  summarise(
    Onset = min(Date[TotalBiomass_0_200m > threshold], na.rm = TRUE), # Define threshold based on data
    Peak = Date[which.max(TotalBiomass_0_200m)],
    Duration = as.numeric(difftime(max(Date[TotalBiomass_0_200m > threshold], na.rm = TRUE), Onset, units = "days"))
  ) %>%
  ungroup()
# Plot trends in onset, peak, and duration
ggplot(zoop_metrics, aes(x = Year)) +
  geom_line(aes(y = as.numeric(Onset), color = "Onset")) +
  geom_line(aes(y = as.numeric(Peak), color = "Peak")) +
  geom_line(aes(y = Duration, color = "Duration")) +
  labs(title = "Annual Zooplankton Bloom Metrics",
       x = "Year",
       y = "Days / Date",
       color = "Metric") +
  theme_minimal()
# Note: Define 'threshold' based on data characteristics for bloom onset detection.
# This code provides a framework; adjust threshold and metrics as needed based on data exploration.
# Further analyses could include correlations with environmental variables, size-fraction specific trends, etc.

