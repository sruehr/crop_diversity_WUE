# Combine all datasets into one data.table object for analysis
# Sophie Ruehr
# December 30, 2024

library(pacman)
p_load(here, data.table, sf, raster, terra, exactextractr, dplyr, plyr, tidyr)
rm(list = ls())

# Load central valley study area
study_area <- here::here("data", "raw_data", "study_area", "study_area.shp")
study_area <- st_read(study_area) 

# Get file names
# CONSTANT
soil_files <- list.files(here::here('data', 'raw_data', "GEE", 'SOILGRIDS'), pattern = 'tif', full.names = T)
wtd_files <- list.files(here::here('data', 'processed_data', 'WTD'), pattern = 'tif', full.names = T)
annual_pr_file <- list.files(here::here('data', 'processed_data', 'GRIDMET'), pattern = 'annual_pr', full.names= T)

  # ANNUAL
dwr_files <- list.files(here::here('data', 'processed_data',"DWR", 'rasters'), pattern = '.tif', full.names = T)
div_files <- list.files(here::here('data', 'processed_data',"CDL"), pattern = 'diversity', full.names = T)
legume5_files <- list.files(here::here('data', 'processed_data',"CDL"), pattern = 'legume5', full.names = T)  

  # MONTHLY
    # GRIDMET
et_files <- list.files(here::here("data", "raw_data", "GEE", 'OpenET'), pattern = 'tif', full.names = T)
vpd_files <- list.files(here::here('data', 'raw_data', 'GEE', 'GRIDMET'), pattern = 'vpd', full.names = T)
spi_files <- list.files(here::here('data', 'raw_data', 'GEE', 'GRIDMET'), pattern = 'spi', full.names = T)
srad_files <- list.files(here::here('data', 'raw_data', 'GEE', 'GRIDMET'), pattern = 'srad', full.names = T)
pr_files <- list.files(here::here('data', "raw_data", "GEE", 'GRIDMET'), pattern = 'pr', full.names = T)
sif_files <- list.files(here::here('data', 'processed_data', 'SIF'), pattern = '.tif', full.names = T)
cwd_file <- list.files(here::here("data", "processed_data", "for_analysis"), pattern=  'CWD.grd', full.names = T)

# Open rasters stacks, crop to DWR file/study area, and name
# Function to open file
open_stack <- function(files, names, crop_flag = T) {
  stacked <- stack(files)
  names(stacked) <- names
  if (crop_flag==T) {
    stacked <- crop(stacked, dwr)
  }
  return(stacked)
}

# DWR
dwr <- open_stack(dwr_files,
                  names = paste0('dwr_',substr(dwr_files, nchar(dwr_files)-7, nchar(dwr_files)-4)),
                  crop_flag = F)
# SIF needs to be masked
sif <- open_stack(sif_files,
                  names = paste0('gpp_',paste0(substr(sif_files, nchar(sif_files)-9, nchar(sif_files)-6), '.', substr(sif_files, nchar(sif_files)-5, nchar(sif_files)-4))))
study_area_proj <- st_transform(study_area, crs(sif))
sif <- mask(sif, study_area_proj)

# Open monthly
et <- open_stack(et_files,
                 names =  paste0('et_',substr(et_files, nchar(et_files)-10, nchar(et_files)-4)))
pr <- open_stack(pr_files,
                 names = paste0('pr_',substr(pr_files, nchar(pr_files)-10, nchar(pr_files)-4)))
srad <- open_stack(srad_files,
                   names = paste0('srad_',substr(srad_files, nchar(srad_files)-10, nchar(srad_files)-4)))
vpd <- open_stack(vpd_files,
                   names = paste0('vpd_',substr(vpd_files, nchar(vpd_files)-10, nchar(vpd_files)-4)))
spi <- open_stack(spi_files,
                  names = paste0('spi_',substr(spi_files, nchar(spi_files)-10, nchar(spi_files)-4)))
cwd <- brick(cwd_file) # saved a raster brick
names(cwd) <- c(paste0('cwd_', substr(names(cwd), 2, 10)))
cwd <- crop(cwd, dwr)

# Open annual
div <- open_stack(div_files, names = substr(div_files, nchar(div_files)-17, nchar(div_files)-4))
legume5 <- open_stack(legume5_files, names = substr(legume5_files, nchar(legume5_files)-15, nchar(legume5_files)-4))

# Open constant
soils <- open_stack(soil_files, names = 'clay')
wtd <- open_stack(wtd_files, names = 'wtd')
annual_pr <- open_stack(annual_pr_file, names = 'annual_pr')

# Stack into one file
all <- stack(c(sif, et, srad, pr, vpd, spi, cwd,
               dwr, legume5, div,
               soils, wtd, annual_pr))

# Transfer to data table
dt <- as.data.frame(all, xy = T) %>% setDT()
keep <- 'dt'
rm(list = setdiff(ls(all.names = TRUE), keep))
gc()

# Create individual data tables based on temporal res
constant_names <- c('x', 'y', 'clay', 'wtd', 'annual_pr')
constants <- dt[,..constant_names] %>% 
  filter(rowSums(!is.na(dplyr::select(., -x, -y))) > 0)

annual_names <-names(dt)[which(substr(names(dt), 1, 3) %in% c('x', 'y', 'dwr', 'leg', 'div'))]
annuals <- dt[,..annual_names] %>% 
  pivot_longer(!c(x,y)) %>% mutate(year = as.numeric(sub(".*_(\\d{4})$", "\\1", name)),
                                   var = sub("_(\\d{4})$", "", name)) %>% 
  dplyr::select(-name) %>%    
  pivot_wider(names_from = var, values_from = value) %>% 
  filter(!is.na(dwr))

# This takes a while! 
monthly_names <- names(dt)[which(substr(names(dt), 1, 3) %in% c('x', 'y', 'sra', 'vpd', 'pr_', 'spi', 'gpp', 'et_', 'cwd'))]
monthly <- dt[,..monthly_names] %>% 
  pivot_longer(!c(x,y)) %>% na.omit() %>%  mutate(date = sub(".*[_.](\\d{4}[.-]\\d{2})", "\\1", name),
                                                  var =  sub("_.*", "", name)) %>% 
  mutate(year = as.numeric(substr(date, 1, 4)),
         month = as.numeric(substr(date, 6,7)),
         var = tolower(var)) %>% 
  dplyr::select(-date, -name) %>% 
  pivot_wider(names_from = var, values_from = value) %>% 
  filter(!is.na(et))

# Now combine all data by X, Y and date
keep <- c('constants', 'monthly', 'annuals')
rm(list = setdiff(ls(all.names = TRUE), keep))
gc()

outdat <- merge(annuals, constants, by = c('x', 'y'), all.x = T)
outdat <- merge(outdat, monthly, by = c('x', 'y', 'year'), all.x = T)

keep <- c('outdat')
rm(list = setdiff(ls(all.names = TRUE), keep))
gc()

# Save output
output_path <- here::here("data", "processed_data", "for_analysis",  'merged_data.csv')
fwrite(outdat, output_path)

