# 24.MoncadaMorales.unpublished
Supplemental codes for "Forecasting High Resolution Precipitation Events with Binary Reservoir Computing" by Lizda Nazdira Moncada Morales, Matthew Bonas, Stefano Castruccio and Paola Crippa.

## Data
Folder containing the latitude and longitude coordinates for the entire region depicted in the manuscript. Due to size constraints, the original PRCP from the WRF output can be found [here](https://www.google.com). Binary converted data for the 10 cities discussed in the manuscript is also included in the file `BinaryHourlyPrecipCities.RData` and is used with the R scripts in the Code folder.

## Code
Folder containing R scripts used to conduct both short- and long-range forecasting using the PRCP data from the WRF output (converted to binary) for the 10 cities disucssed in the manuscript. This folder also contains an R script `all_functions.R` which contains the necessary functions to run the BinESN and is loaded automatically when running the other scripts.
