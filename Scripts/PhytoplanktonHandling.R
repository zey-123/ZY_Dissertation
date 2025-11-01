library(readxl)
library(dplyr)


BATS_pigments_1_ <- read_excel("Data/BATS_pigments (1).xlsx",na="-999")
View(BATS_pigments_1_)

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

#convert Depth_m and Chl_a_ug_per_L to numeric
BATS_chla$Depth <- as.numeric(BATS_chla$Depth)
BATS_chla$Chl <- as.numeric(BATS_chla$Chl)

#plot Chl_a_ug_per_L vs Depth_m
plot(BATS_chla$Chl, BATS_chla$Depth, type="p", ylim=rev(range(BATS_chla$Depth)),
     xlab="Chlorophyll a (ug/L)", ylab="Depth (m)",
     main="BATS Chlorophyll a Profile")

