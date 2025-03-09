# Sophie Ruehr
# December 30, 2024
# Translate Fan et al. 2013 WTD estimates to a raster format over Central Valley study area


library(pacman)
p_load(raster, sf, rgdal, ncdf4, here)

# Open data
nc_file <- list.files(here::here('data', 'raw_data', 'fan+2013_WTD'), 'annualmean.nc', full.names = T)
study_area <- here::here("data", "raw_data", "study_area", "study_area.shp")
study_area <- st_read(study_area)

# Read the NetCDF file and load the 'WTD' variable as a raster
wtd_raster <- raster(nc_file, varname = "WTD")

# Crop a little bit
CA <- extent(c(-124, -114, 32, 42))
wtd_raster <- crop(wtd_raster, CA)

# Reproject the raster to match the shapefile's CRS
wtd_raster_proj <- projectRaster(wtd_raster, crs = crs(study_area))

# Crop the raster using the shapefile
wtd_cropped <- crop(wtd_raster_proj, extent(study_area))

# Mask the cropped raster to the shapefile
wtd_cropped_masked <- mask(wtd_cropped, study_area)
wtd_cropped_masked <- projectRaster(wtd_cropped_masked, wtd_raster)

# Resample to SIF data
sif_data_file <- list.files(here::here('data', 'processed_data', 'SIF'), pattern = '201808', full.names = T)
sif <- raster(sif_data_file)

wtd_resamp <- resample(wtd_cropped_masked, sif)

output_path <- here::here("data", "processed_data", "WTD",  paste0("WTD_annual_500m.tif"))
writeRaster(wtd_resamp, output_path, overwrite=T)