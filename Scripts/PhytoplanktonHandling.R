library(readxl)
library(dplyr)


BATS_pigments_1_ <- read_excel("Data/BATS_pigments (1).xlsx",na="-999")
View(BATS_pigments_1_)

# Cleaning the dataset ----
#get rid of rows 1-49 and make the new row 1 column names
BATS_pigments <- BATS_pigments_1_[-c(1:49), ]
colnames(BATS_pigments) <- BATS_pigments[1, ] #set first row as column names
BATS_pigments <- BATS_pigments[-1, ] #remove the first row which is now column names

View(BATS_pigments)

#using Chl column, standardize depth for chlorophyll a. Need to think of a way to normalize this across the water column as different things have different ranges and need to normalize this.
BATS_chla <- BATS_pigments %>%
  select(yyyymmd, Depth, Chl) %>%
  na.omit()
View(BATS_chla)









#######################################
#convert Depth_m and Chl_a_ug_per_L to numeric
BATS_chla$Depth <- as.numeric(BATS_chla$Depth)
BATS_chla$Chl <- as.numeric(BATS_chla$Chl)

#remove rows with NA values in Depth or Chl
BATS_chla <- BATS_chla %>%
  filter(!is.na(Depth) & !is.na(Chl))


# Depth Integration (z-score) ----
# calculating the mean - Sum all the chlorophyll values in the BATS_chla dataset and divide by the number of data points.
(mean_chla <- mean(BATS_chla$Chl, na.rm = TRUE))
# calculating the standard deviation - Calculate the standard deviation of the chlorophyll values in the BATS_chla dataset.
(sd_chla <- sd(BATS_chla$Chl, na.rm = TRUE))

# calculating the z-score - For each chlorophyll value, subtract the mean and divide by the standard deviation.
BATS_chla <- BATS_chla %>%
  mutate(Chl_zscore = (Chl - mean_chla) / sd_chla)
View(BATS_chla)

#order dataset by depth
BATS_chla <- BATS_chla %>%
  arrange(Depth)
View(BATS_chla)





# Vertical integration
# Compute depth-integrated chlorophyll by integration - zero as bottom of integration 200 as top Chl(z) dz then normalize by dividing integration depth to get an avg
BATS_chla_depth_integrated <- BATS_chla %>%
  group_by(yyyymmd) %>% # group by date
  summarise(Depth_Integrated_Chl = sum(Chl, na.rm = TRUE) * (200 / n())) %>% # integrate and normalize
  ungroup() # ungroup the data
View(BATS_chla_depth_integrated)



# Trying stuff out ----
#Step 1: Decide on reference depth as 200m and Interpolate each cast onto a standard depth grid. ----
standard_depths <- seq(0, 200, by = 1) # Standard depth grid from 0 to 200 m at 1 m intervals
BATS_chla_interpolated <- BATS_chla %>%
  group_by(yyyymmd) %>%
  do({
    interp_chl <- approx(.$Depth, .$Chl, xout = standard_depths, rule = 2)$y
    data.frame(Depth = standard_depths, Chl = interp_chl)
  }) %>%
  ungroup()
View(BATS_chla_interpolated)


#Step 2: Normalize either: By depth-integrated chlorophyll (average over layer) OR Z-score per depth OR Relative to total chlorophyll in upper layer ----
BATS_chla_normalized <- BATS_chla_interpolated %>%
  group_by(yyyymmd) %>%
  mutate(Total_Chl = sum(Chl, na.rm = TRUE),
         Chl_Normalized = Chl / Total_Chl) %>%
  ungroup()
View(BATS_chla_normalized)

#Step 3: Visualize time-series at specific depths or depth-averaged ----
# Example: Time-series at 10m depth
chl_10m <- BATS_chla_normalized %>%
  filter(Depth == 10)
plot(as.Date(chl_10m$yyyymmd, format="%Y%m%d"), chl_10m$Chl_Normalized, type="l",
     xlab="Date", ylab="Normalized Chlorophyll a at 10m",
     main="Time-series of Normalized Chlorophyll a at 10m Depth")
# Example: Depth-averaged normalized chlorophyll
chl_depth_avg <- BATS_chla_normalized %>%
  group_by(yyyymmd) %>%
  summarise(Depth_Avg_Chl = mean(Chl_Normalized, na.rm = TRUE)) %>%
  ungroup()
plot(as.Date(chl_depth_avg$yyyymmd, format="%Y%m%d"), chl_depth_avg$Depth_Avg_Chl, type="l",
     xlab="Date", ylab="Depth-averaged Normalized Chlorophyll a",
     main="Time-series of Depth-averaged Normalized Chlorophyll a")

# Step 4: Seasonal pattern analysis ----

