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

#round from decimal to whole number
BATS_chla$Depth <- round(as.numeric(BATS_chla$Depth))


# Normalizing ----

### Step 1: Interpolation to a Standard Depth Grid (every 5 m from 0 m to 200 m.) ----

# Group by unique depths and average chlorophyll values if duplicates exist

BATS_chla <- BATS_chla %>%
  group_by(yyyymmd, Depth) %>%
  summarise(Chl = mean(Chl, na.rm = TRUE)) %>%
  ungroup()

# Create a standard depth grid
standard_depths <- seq(0, 200, by = 5) # Standard depth grid from 0 to 200 m at 5 m intervals
Chl <- BATS_chla$Chl
Depth <- BATS_chla$Depth

interp_chl <- approx(Depth, Chl, xout=standard_depths, rule=2)$y  # rule=2 => use boundary values for extrapolation


library(pracma)   # for trapz
# linear interpolation
interp_chl <- approx(depth, chl_ug_L, xout=std_grid, rule=2)$y  # rule=2 => use boundary values for extrapolation

# convert µg L^-1 to µg m^-3
chl_ug_m3 <- interp_chl * 1000

# integrate 0-200 m (gives µg m^-2)
chl_ug_m2 <- trapz(std_grid, chl_ug_m3)
chl_mg_m2 <- chl_ug_m2 / 1000

# if cast only to 150 m, option A scale, option B extend with last value
Zmax <- max(depth)
# option A: scale
integral_to_Zmax <- trapz(seq(0,Zmax,by=1), approx(depth, chl_ug_L*1000, xout=seq(0,Zmax,by=1))$y)
scaled_integral_ug_m2 <- integral_to_Zmax * (200 / Zmax)

# option B: extend using last measured value down to 200 m
ext_depths <- c(depth,200)
ext_chl <- c(chl_ug_L, tail(chl_ug_L,1))
ext_interp <- approx(ext_depths, ext_chl, xout=std_grid)$y * 1000
extended_integral_ug_m2 <- trapz(std_grid, ext_interp)



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

