# May 6 2024
# Sophie Ruehr

# Calculate monthly SIF from TROPOMI 500m product (Turner et al. 2021). Save output.

library(pacman)
p_load(here, data.table, sf, raster, terra, exactextractr, dplyr, plyr, tidyr)
rm(list =ls())

# Set directories for raw data and saving processed data
datawd <- 'your_wd_here_to_daily_SIF_data_from_Turner+_2019+2021' # Change to your directory where SIF data is stored
savewd <- here('data', 'processed_data', 'SIF')

# Specify months
months_of_interest <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

# Get file information
setwd(datawd)
files <- list.files(pattern = '.nc')
dates <- substr(files, 9, 14)
monthyear <- unique(substr(dates, 1, 6))
monthyear <- monthyear[which(substr(monthyear,5,7) %in% months_of_interest)]

# Specify the variable name you want to extract
variable_name <- 'SIF_dc' # daily-corrected SIF
variable_name <- 'GPP' # GPP approximated from SIF based on Turner et al. 2021

# Function to open and write data
monthly_mean_save <- function(monthyear) {

  # Collect files from month/year of interest
  files_monthyear <- files[which(dates == monthyear)] 
  output <- list()
  
  for (i in 1:length(files_monthyear)) {

    # Open NetCDF file
    setwd(datawd)
    nc <- nc_open(files_monthyear[i])
    
    # Extract SIF
    value_r <- raster(files_monthyear[i],  varname = variable_name)
    output[[i]] <- value_r
    
    # Close the NetCDF file
    nc_close(nc)
  }
  
  # Take monthly mean
  monthyear_data <- mean(stack(output), na.rm = T)
  
  # Write monthly SIF raster
  setwd(savewd)
  writeRaster(monthyear_data, paste0(monthyear, '.tif'), overwrite = T)
  print(monthyear)
}

# Make cluster
no_cores <- detectCores() - 1 # Calculate the number of cores
print(no_cores)
cl <- makeCluster(no_cores, type="FORK") # Initiate cluster

# Run function
parLapply(cl, monthyear, monthly_mean_save)
stopCluster(cl)
