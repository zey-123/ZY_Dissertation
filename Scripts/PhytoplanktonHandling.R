# Handling phytoplankton dataset obtained from BATS_pigments file on their website 

# Importing required libraries 
library(readxl)
library(dplyr)
library(pracma)
library(lubridate)
library(ggplot2)


BATS_pigments_1_ <- read_excel("Data/BATS_pigments (1).xlsx",na="-999")
View(BATS_pigments_1_)

# Cleaning the dataset (BATS_pigments) ----

BATS_pigments <- BATS_pigments_1_[-c(1:49), ] #get rid of rows 1-49 and make the new row 1 column names
colnames(BATS_pigments) <- BATS_pigments[1, ] #set first row as column names
BATS_pigments <- BATS_pigments[-1, ] #remove the first row which is now column names

View(BATS_pigments)

### Creating a new data set (BATS_chla) with only year, depth, turners chlorophyll a data ----
# Remember, aim is: using Chl column, standardize depth for chlorophyll a. Need to think of a way to normalize this across the water column as different things have different ranges and need to normalize this.
BATS_chla <- BATS_pigments %>%
  select(yyyymmd, latN, lonW,Depth, Chl) %>% #select only relevant columns
  na.omit()
View(BATS_chla)

#round from decimal to whole number
BATS_chla$Depth <- round(as.numeric(BATS_chla$Depth))
BATS_chla$Depth <- as.numeric(BATS_chla$Depth)
BATS_chla$Chl   <- as.numeric(BATS_chla$Chl)
#BATS_chla$yyyymmd <- as.numeric(BATS_chla$yyyymmd)

BATS_chla$yyyymmd <- ymd(BATS_chla$yyyymmd)

str(BATS_chla$Chl)
str(BATS_chla$Depth)


#Raw chl data plot
par(mfrow=c(3,1)) # set up a 3-row plotting area

plot(BATS_chla$yyyymmd,BATS_chla$Chl, type="l",
     xlab="Date", ylab="Chlorophyll a (mg/m3)",
     main="Raw Chlorophyll a Data at BATS")

plot(phyto_top3$Date, phyto_top3$Chl_top3, type="l",
     xlab="Date", ylab="Mean Chlorophyll a (mg/m³)",
     main="Surface-weighted Chlorophyll (Top 3 Depth Bins) at BATS")

plot(norm_chl$Date, norm_chl$Chl_mean_200m_mg_m3, type="l",
     xlab="Date", ylab="Depth-normalized Chlorophyll a (mg/m3)",
     main="Time-series of Depth-normalized Chlorophyll a (0-200 m) at BATS")

#Chlorophyll a vs Depth and Time
ggplot(BATS_chla, aes(x=yyyymmd, y=Chl))+
  geom_line(color= "darkblue")+
  geom_smooth(method ='lm', color = "blue")+
  labs( x = "Date", y= "Chl")+
  theme_classic()

########## CHANNING ADVICE ####################################################
# Taking mean of top three depth bins 
phyto_top3 <- BATS_chla %>%
  arrange(yyyymmd, Depth) %>%          # ensure shallow depths come first
  group_by(yyyymmd) %>% 
  slice_min(order_by = Depth, n = 3) %>%# select 3 shallowest depths per date
  summarise(Chl_top3 = mean(Chl, na.rm = TRUE)) %>% # average chlorophyll across these 3 depth bins 
  ungroup() %>%
  rename(Date = yyyymmd) 

# outlier removal 
Q1 <- quantile(phyto_top3$Chl_top3, 0.25, na.rm = TRUE)
Q3 <- quantile(phyto_top3$Chl_top3, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

upper_bound <- Q3 + 1.5 * IQR_val

phyto_top3_clean <- phyto_top3 %>%
  filter(Chl_top3 <= upper_bound)

ggplot(phyto_top3_clean, aes(x = Date, y = Chl_top3)) +
  geom_line(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Surface-weighted Chlorophyll (Top 3 Depth Bins)",
    x = "Date", y = "Mean Chlorophyll a (mg/m³)") +
  theme_classic()

ggplot(norm_chl, aes(x = Date, y = Chl_mean_200m_mg_m3)) +
  geom_line(color = "forestgreen") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Surface-weighted Chlorophyll (Top 3 Depth Bins)",
       x = "Date", y = "Mean Chlorophyll a (mg/m³)") +
  theme_classic()


#save to data folder - github ----
write.csv(phyto_top3_clean , "Data/phyto_top3_clean.csv", row.names = FALSE)


# Trapezoidal integration ----
# If  Depth values don’t start at 0 or end at 200 exactly, trapezoidal integration still handles it correctly.

# Filter depths 0–200 m and arrange
BATS_filtered <- BATS_chla %>%
  filter(Depth >= 0, Depth <= 200) %>%
  arrange(Depth)

# Depth-integrated chlorophyll (mg/m^2)
Chl_int <- trapz(BATS_filtered$Depth, BATS_filtered$Chl)

# FINAL DECISION ----

norm_chl <- BATS_chla %>%
  filter(Depth >= 0, Depth <= 200) %>% # Filter depths 0–200 m
  mutate(Depth = as.numeric(Depth),
    Chl   = as.numeric(Chl)) %>%
  arrange(yyyymmd, Depth) %>%
  group_by(yyyymmd) %>%
  summarise(                      #summarise() collapses all rows for each cast into ONE row.
    Chl_int = trapz(Depth, Chl),       # mg/m2
    Chl_mean_200m = Chl_int / 200      # mg/m3 (depth-normalized)
  )
View(norm_chl)

#date is written as yyyymmdd - fixing date format

#norm_chl$yyyymmd <- as.Date(norm_chl$yyyymmd, format="%Y%m%d")
#norm_chl$yyyymmd <- ymd(norm_chl$yyyymmd)
#norm_chl$yyymmd <- as.numeric (norm_chl$yyyymmd)

#norm_chl$yyyymmd <- as.Date(as.character(norm_chl$yyyymmd), format="%Y%m%d")
#norm_chl$Month <- format(norm_chl$yyyymmd, "%m") # Extract month as numeric
#norm_chl$Year <- format(norm_chl$yyyymmd, "%Y") # Extract year


norm_chl$yyyymmd <- ymd(norm_chl$yyyymmd)

norm_chl$year  <- year(norm_chl$yyyymmd)
norm_chl$month <- month(norm_chl$yyyymmd)
norm_chl$day   <- day(norm_chl$yyyymmd)

#rename column names
colnames(norm_chl)[1] <- "Date"
colnames(norm_chl)[2] <- "Chl_Int_200m_mg_m2"
colnames(norm_chl)[3] <- "Chl_mean_200m_mg_m3"
colnames(norm_chl)[4] <- "Year"
colnames(norm_chl)[5] <- "Month"
colnames(norm_chl)[6] <- "Day"


# Visualizing time-series of depth-normalized chlorophyll ----
plot(norm_chl$Date,norm_chl$Chl_mean_200m_mg_m3, type="l",
     xlab="Date", ylab="Depth-normalized Chlorophyll a (mg/m3)",
    # ylim=c(0,0.5),
     main="Time-series of Depth-normalized Chlorophyll a (0-200 m)")                            

### a) Time-series plot of time-series depth normalized chlorophyll — can see seasonal cycles and long-term changes.----
ggplot(norm_chl, aes(x = Date, y = Chl_mean_200m_mg_m3)) +
  geom_line(color = "darkblue") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Phytoplankton Biomass (0–200m) Over Time at BATS",
       x = "Date",
       y = "Chlorophyll Mean (mg/m³, normalized to 200m)") +
  theme_classic()

### b) Seasonal pattern (montly averages) -----
#norm_chl$Month <- format(norm_chl$yyyymmd, "%m") # Extract month as numeric
#norm_chl$Month <- as.numeric(norm_chl$Month) # Convert to numeric
chl_monthly <- norm_chl %>% 
  group_by(Month) %>% 
  summarise(MonthlyAvgBiomass = mean(Chl_mean_200m_mg_m3, na.rm = TRUE)) %>% 
  ungroup()
# Plot monthly averages
ggplot(chl_monthly, aes(x = Month, y = MonthlyAvgBiomass)) + # x is month (1-12)
  geom_line(color = "darkgreen") +
  geom_point() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Monthly Mean Phytoplankton (Chl) Biomass (0–200m) at BATS",
       x = "Month",
       y = "Average Chl (mg/m³, normalized to 200m)") +
  theme_classic()


### c) Annual pattern (yearly averages) ----
str(chl_yearly)
chl_yearly <- norm_chl %>% 
  group_by(Year) %>% 
  summarise(YearlyAvgBiomass = mean(Chl_mean_200m_mg_m3, na.rm = TRUE)) %>%  #calculating avg per year
  ungroup()
# Plot yearly averages
ggplot(chl_yearly, aes(x = Year, y = YearlyAvgBiomass, group=1)) +
  geom_line(color = "purple") +
  geom_smooth(method = "lm", color = "purple", se = FALSE) +
  geom_point() +
  labs(title = "Annual Mean Phytoplankton (Chl) Biomass (0–200m) at BATS",
       x = "Year",
       y = "Average Chl (mg/m³, normalized to 200m)") +
  theme_classic()



# Dealing with the outliers -----
#identifying points greater than 1 mg/m3
outliers <- norm_chl %>%
  filter(Chl_mean_200m_mg_m3 > 1)
View(outliers)

# IQR test to statistically check for whether these are actually outliers (without assuming normality)
chl <- norm_chl$Chl_mean_200m_mg_m3

Q1 <- quantile(chl, 0.25, na.rm = TRUE)
Q3 <- quantile(chl, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

upper_bound <- Q3 + 1.5 * IQR_val
lower_bound <- Q1 - 1.5 * IQR_val

outliers_iqr <- norm_chl %>%
  filter(Chl_mean_200m_mg_m3 > upper_bound |
           Chl_mean_200m_mg_m3 < lower_bound)
outliers_iqr

#Remove outliers based on IQR
norm_chl <- norm_chl %>%
  filter(Chl_mean_200m_mg_m3 <= upper_bound &
  Chl_mean_200m_mg_m3 >= lower_bound)


#checking for normality ----
hist(norm_chl$Chl_mean_200m_mg_m3, breaks=50,
     main="Histogram of Depth-normalized Chlorophyll a (0-200 m) after Outlier Removal",
     xlab="Depth-normalized Chlorophyll a (mg/m3)",
     col="lightblue")

#check if this is normal 
shapiro.test(norm_chl$Chl_mean_200m_mg_m3) #p-value < 0.05, data is not normally distributed

lm_normchl<-lm(Chl_mean_200m_mg_m3~Year, data=norm_chl)
plot(lm_normchl)


#save norm_chl to dataset folder - github ----
write.csv(norm_chl, "Data/norm_chl.csv", row.names = FALSE)

#using BATS_chla dataset to visualise phytoplankton data using long and lat info plot one for every year 
#import ggplot and maps library
library(ggplot2)
library(maps)
# Get world map data
world_map <- map_data("world")

# make sure lat and lon are numeric
BATS_chla$latN <- as.numeric(BATS_chla$latN)
BATS_chla$lonW <- as.numeric(BATS_chla$lonW)
BATS_chla$Chl  <- as.numeric(BATS_chla$Chl)

# Create a scatter plot of sampling locations for each year
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = BATS_chla, aes(x = lonW, y = latN, color = Chl), size = 2, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "Phytoplankton Chlorophyll a Concentration at BATS",
       x = "Longitude",
       y = "Latitude",
       color = "Chl (µg/L)") +
  theme_minimal() +
  coord_fixed() +
  facet_wrap(~ year(ymd(yyyymmd))) # Facet by year



# Visualising community SIZE of plankton ----

# approach 1: boxplot for first decade cumulative chl-a and last decade cumulative chl-a 
cumulative_chl <- BATS_chla %>%
  group_by(yyyymmd) %>%
  summarise(Cumulative_Chl = sum(Chl, na.rm = TRUE)) %>% # sum all chlorophyll values for each date to get cumulative chlorophyll per date
  ungroup() %>%
  mutate(Decade = case_when(
    year(yyyymmd) >= 1980 & year(yyyymmd) < 1990 ~ "First Decade (1980s)",
    year(yyyymmd) >= 2000 & year(yyyymmd) < 2010 ~ "Last Decade (2000s)",
  ))


ggplot(cumulative_chl, aes(x = Decade, y = Cumulative_Chl)) +
  geom_boxplot() +
  labs(title = "Cumulative Chlorophyll a by Decade",
       x = "Decade",
       y = "Cumulative Chlorophyll a (mg/m³)") +
  ylim(0, 3) + # Adjust y-axis limits as needed, adding a trendline
  #geom_smooth(method = "lm", aes(group = 1), color = "red", se = FALSE) +
  theme_classic()


















# ------------------------ TESTING OTHER APPROACHES -------------------------------------------
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





# Trying stuff out 
### Step 1: Decide on reference depth as 200m and Interpolate each cast onto a standard depth grid. ----
standard_depths <- seq(0, 200, by = 1) # Standard depth grid from 0 to 200 m at 1 m intervals
BATS_chla_interpolated <- BATS_chla %>%
  group_by(yyyymmd) %>%
  do({
    interp_chl <- approx(.$Depth, .$Chl, xout = standard_depths, rule = 2)$y
    data.frame(Depth = standard_depths, Chl = interp_chl)
  }) %>%
  ungroup()
View(BATS_chla_interpolated)


### Step 2: Normalize either: By depth-integrated chlorophyll (average over layer) OR Z-score per depth OR Relative to total chlorophyll in upper layer ----
BATS_chla_normalized <- BATS_chla_interpolated %>%
  group_by(yyyymmd) %>%
  mutate(Total_Chl = sum(Chl, na.rm = TRUE),
         Chl_Normalized = Chl / Total_Chl) %>%
  ungroup()
View(BATS_chla_normalized)

### Step 3: Visualize time-series at specific depths or depth-averaged ----
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

### Step 4: Seasonal pattern analysis ----


### Depth Integration (z-score) ----
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

# ------
### Vertical integration
### Compute depth-integrated chlorophyll by integration - zero as bottom of integration 200 as top Chl(z) dz then normalize by dividing integration depth to get an avg
BATS_chla_depth_integrated <- BATS_chla %>%
  group_by(yyyymmd) %>% # group by date
  summarise(Depth_Integrated_Chl = sum(Chl, na.rm = TRUE) * (200 / n())) %>% # integrate and normalize
  ungroup() # ungroup the data
View(BATS_chla_depth_integrated)




