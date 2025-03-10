# Oct 16 2024
# Calculate total acerage of crops in CA for 2021


library(pacman)
p_load(dplyr, ggplot2, sf, sp,
       plyr, tidyr, here)
rm(list=ls())

year <- c('2022')

# Set working directory for files
save_wd <- here::here('data', 'processed_data', 'DWR')
folder_wd <- here::here('data', 'raw_data', 'DWR', year) 
setwd(folder_wd)

# Open DWR file
file <- setdiff(list.files(pattern = '.shp'), list.files(pattern = '.shp.xml'))
DWR <- read_sf(file) 

vars_of_interest <- c( 'geometry','UCF_ATT', 'COUNTY', 'ACRES', 'CLASS2', 'SUBCLASS2')
DWR_sm <- DWR %>% subset(select = vars_of_interest) 

acerage <- DWR_sm %>% 
  mutate(code = paste0(CLASS2, SUBCLASS2),
         class2 = CLASS2,
         county = COUNTY,
         hectares = 0.404686 * ACRES) %>%  # convert to hectares
  as.data.frame() %>% dplyr::select(code, class2, county, hectares) %>% 
  group_by(class2, code, county) %>% 
  summarise_at(.vars = 'hectares', .fun = sum, na.rm = T) 

# Merge with crop names
crop_names_file <- here::here('data', 'processed_data', 'DWR', 'code_key.xlsx')
crop_names <- readxl::read_xlsx(crop_names_file)
crop <- crop_names %>% filter(variable == 'SUBCLASS') %>% dplyr::select(short_name, code)
acerage <- merge(acerage, crop, by = 'code', all.x = T)  

# Write info
setwd(save_wd)
save_file_name = paste0(year, '_acerage.csv')
write.csv(acerage, save_file_name)

