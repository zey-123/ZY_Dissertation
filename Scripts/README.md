## Scripts Folder
This folder contains all the following R-Scripts :

- **CTD_Handling**: Script where raw temperature data from bats Excel file has been transformed, ready for use in following analyses. Output of this script can be found in Data folder, named _'bats_temp_FINAL.csv'_
- **Method2Indicies**: Tests an alternative method to handling phytoplankton depth normalisation using csv named '_phyto_top3_clean_'(from Data folder). This script visualises what indices look like under an alternative method of averaging top 3 depth bins which yields the same results as initial depth integration to 200m method.
- **PhenologicalIndiciesCalc**: Is where all the main phenological indicie calculations and visualisations (both temporal and with environmental parameters) has been conducted, both for phytoplankton and zooplankton. Output datasets for statistical analysis can be found in Data folder named: _phytoplankton_phenology_clean.csv_ and _zooplankton_phenology_clean.csv_
- **PhytoplanktonHandling**: Conversion and transformation of raw pigment data (named: _"BATS_pigments(1).xlsx_ in Data folder) from BATS into a final, ready for use dataset. Output dataset can be found in Data folder named _norm_chl.csv_. 
- **Statistical Modelling**: Script where all statistical analyses (including LMs and GLMs as well as assumption tests, autocorrelation checks and AIC comparisons) have been achieved. This script uses the following datasets created in the PhenologicalIndiciesCalc script from the Data folder, named: _phytoplankton_phenology_clean.csv_ and _zooplankton_phenology_clean.csv_. 
- **Stratification_CTD**: Conversion of raw metrics from CTD deployments into a stratification indicie metric using `GSW` package. Output of the stratification indicies can be found in Data folder, named _bats_stratification_index.csv_
- **UnusedCode**: Script split into sections with names of all the other scripts where unused parts of code have been inserted for the sake of having clean final scripts. Can check this out to see what further tests, work has been done under the respective sections. 
- **ZooplanktonData**: Conversion and transformation of raw zooplankton data (named: _"BATS_zooplankton(1).xlsx_ in Data folder) from BATS into a final, ready for use dataset. Output dataset can be found in Data folder named _zoop_daily.csv_ (or to see dataset that contains outliers - which wasnt used - _zoop_daily_wthoutliers.csv_)

_Created: September, 2025_
