# Calculate number of legume plantings from previous 6 years from CDL (USDA)
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
cdl_files <- list.files(here::here('data', 'raw_data', 'GEE', 'CDL'), pattern = 'CDL', full.names = T)
cultivated_files <- list.files(here::here('data', 'raw_data', 'GEE', 'CDL'), pattern = 'cultivated', full.names = T)

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

# Legume values
legume_cdl_values <- c(5, 10, 42, 53, 220, 51, 36, 26, 239, 240)

# Find legume values
create_legume_mask <- function(x) {
  mask_stack <- x
  mask_stack[!mask_stack %in% legume_cdl_values] <- NA
  mask_stack[!is.na(mask_stack)] <- 1
  return(mask_stack)
}

# Parallelized calculation
# Set up parallel processing
num_cores <- detectCores() - 1  # Detect available cores and leave one core free
cl <- makeCluster(num_cores)
clusterExport(cl, c("legume_cdl_values", "create_legume_mask", "r_stack", "cult_stack", "years", "RCI_years", "RCI_diff", "resample_grid"))  # Export needed variables and functions

legumes <- clusterR(r_stack, calc, args = list(fun = create_legume_mask), cl = cl)

# Function to calculate legume flag for a given year index
calc_legume_for_year <- function(year_index) {
  # Get stack of year of preceding years
  first_year <- RCI_years[year_index] - RCI_diff
  first_index <- which(as.numeric(years) == first_year)
  last_index <- which(as.numeric(years) == RCI_years[year_index])
  input_stack <- legumes[[first_index:c(last_index - 1)]]
  
  # Calculate number of legume years 
  legume_i <- sum(input_stack, na.rm = TRUE)
  
  # Calculate whether the previous year had legumes
  prev_legume <- legumes[[last_index - 1]]
  
  # Mask out for cultivated layer
  cult_i <- cult_stack[[last_index]]
  
  # Resample
  legume_i_resamp <- resample(legume_i, resample_grid, method = 'bilinear')
  cult_i_resamp <- resample(cult_i, resample_grid, method = 'ngb')
  prev_legume_resamp <- resample(prev_legume, resample_grid, method = 'bilinear')
  
  # Mask
  legume_i_resamp_mask <- mask(legume_i_resamp, cult_i_resamp)
  prev_legume_masked <- mask(prev_legume_resamp, cult_i_resamp)
  
  # Save masked RCI for year of interest
  output_path <- here::here("data", "processed_data", "CDL",  paste0('legume1_', RCI_years[year_index], '.tif'))
  writeRaster(prev_legume_masked, output_path, overwrite = TRUE)
  output_path <- here::here("data", "processed_data", "CDL",  paste0('legume5_', RCI_years[year_index], '.tif'))
  writeRaster(legume_i_resamp_mask, output_path, overwrite = TRUE)
  
  
}

# Set up parallel processing for the calculation function
clusterExport(cl, c("legumes", "resample_grid", "cult_stack", "years", "RCI_years", "RCI_diff", "calc_legume_for_year"))
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(here))

# Loop to calculate RCI for each year of interest in parallel
legumes_out <- parLapply(cl, seq_along(RCI_years), calc_legume_for_year)

# Stop the cluster
stopCluster(cl)
