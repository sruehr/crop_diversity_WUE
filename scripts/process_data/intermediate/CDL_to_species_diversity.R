# Calculate species diversity from previous 6 plantings from CDL (USDA)
# Sophie Ruehr
# December 30, 2024


# Load required packages
library(pacman)
p_load(raster, dplyr, plyr, ggplot2, terra, parallel, here)

# USER INPUTS
RCI_years = c(2018, 2019, 2020, 2021, 2022) # Select years that you want to calculate RCI for
RCI_diff = 5 # Number of years previous to year of interest from which to calculate RCI (e.g., 5 = 2013-2017 for 2018)

# Grid to resample to SIF resolution (500 m)
file <- list.files(here::here('data', 'processed_data', 'SIF'), pattern = '201808', full.names = T)
resample_grid <- raster(file)

# Open CDL raster files and cultivated layer information
cdl_files <- list.files(here::here('data', 'raw_data', 'CDL'), pattern = 'CDL', full.names = T)
cultivated_files <- list.files(here::here('data', 'raw_data', 'CDL'), pattern = 'cultivated', full.names = T)

# Stack data
stacked_raster <- stack(cdl_files)
cult_stack <- stack(cultivated_files)

# Mask out uncultivated areas for each year
cultivated_mask <- cult_stack
r_stack <- mask(stacked_raster, cultivated_mask, maskvalue = 1)

# Dates
years <- substr(cdl_files, nchar(cdl_files)-13, nchar(cdl_files)-10)
names(r_stack) <- years
names(cult_stack) <- years

# Function to calculate species diversity for a given year index
calc_diversity_for_year <- function(year_index) {
  print(paste0('Processing ', RCI_years[year_index]))
  
  # Get stack of year of interest and preceding years
  first_year <- RCI_years[year_index] - RCI_diff
  first_index <- which(as.numeric(years) == first_year)
  last_index <- which(as.numeric(years) == RCI_years[year_index])
  input_stack <- r_stack[[first_index:last_index]]

  # Define RCI calculation function
  calc_diversity <- function(input_stack) {
    
    # 1. Calculate n
    unique_values <- function(x) {length(unique(x[!is.na(x)]))}
    n <- calc(input_stack, unique_values)
    
    return(n)
  }
  
  # Calculate RCI for year of interest
  div_i <- calc_diversity(input_stack)
  # Mask out for cultivated layer
  cult_i <- cult_stack[[last_index]]
  div_i = mask(div_i, cult_i)
  
  # Resample
  div_resampled <- resample(div_i, resample_grid, method = 'bilinear')
  
  # Save masked RCI for year of interest
  output_path <- here::here("data", "processed_data", "CDL",  paste0('diversity_', RCI_years[year_index], '.tif'))
  writeRaster(div_resampled, output_path, overwrite = TRUE)
  

}

# Set up parallel processing
ncores <- detectCores() - 1  # Number of cores to use, leaving one core free
cl <- makeCluster(ncores)
clusterExport(cl, c("r_stack", 'resample_grid', "cult_stack",  
                    "years", "RCI_years", "RCI_diff", "calc_diversity_for_year" #, "calc_rci"
                    ))
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(terra))
clusterEvalQ(cl, library(here))

# Loop to calculate RCI for each year of interest in parallel
RCI <- parLapply(cl, seq_along(RCI_years), calc_diversity_for_year)

# Stop the cluster
stopCluster(cl)
