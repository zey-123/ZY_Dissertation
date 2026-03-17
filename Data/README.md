Check descriptions in README of 'Scripts' folder to have a better understanding. But brief overview of data is as follows:
* **BATS_pigments:** raw dataset for pigment (chlorophyll etc) values to calculate biomass proxy for phytoplankton.

* **BATS_zooplankton:** raw dataset for zooplankton data (including normalised biomass values)

* **bats_temp_final**: final temperature dataset (made from merging BATS CTD data files) 

* **norm_chl**: phytoplankton finalised dataset - depth integrated and normalized for 200m using turners chlorophyll index values from BATS_pigments excel file (obtained from BATS website).

* **zoop_daily**: zooplankton finalised dataset
* **bats_stratification_index.csv**: final stratification dataset (computed using `GSW` package and raw salinity values from CTD dataset).
