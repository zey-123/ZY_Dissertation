## Load necessary libraries
library(dplyr)
library(readr)
library(readxl)

# Step 1: Load the zooplankton dataset 
zooplankton<- read_excel("Data/BATS_zooplankton (1).xlsx")
View(BATS_zooplankton_1_)

# Step 2: Check column names ---
# Getting rid of the first 34 rows
zooplankton<- zooplankton[-c(1:34), ]

#making row 1 the column headers
colnames(zooplankton) <- as.character(unlist(zooplankton[1, ]))
names(zooplankton)

zooplankton <- zooplankton %>%
  mutate(Date = as.Date(as.character(date), format = "%Y%m%d"),
         Depth = as.numeric(max_depth)) %>%

# --- Step 3: Convert date to proper Date format ---
#date is written as yyyymmdd e.g. 19940406

zooplankton <- zooplankton %>%
  mutate(Date = as.Date(as.character(date), format = "%Y%m%d"),
         Depth = as.numeric(max_depth)) %>%

# --- Step 4: Filter to only depths ≤ 200 m ---
# zoop_filtered <- zoop %>%
#  filter(Depth <= 200)

# --- Step 5: Depth integrate (sum biomass per sampling event) ---
# Adjust biomass column name accordingly
zoop_integrated <- zoop_filtered %>%
  group_by(Date) %>%
  summarise(TotalBiomass_0_200m = sum(Biomass_mg_m3, na.rm = TRUE)) %>%
  ungroup()

# --- Step 6: Save output for later analysis ---
write_csv(zoop_integrated, "zooplankton_integrated_0_200m.csv")

# Now zoop_integrated has one row per date with summed biomass for top 200 m

# Visualising the data
library(ggplot2)


