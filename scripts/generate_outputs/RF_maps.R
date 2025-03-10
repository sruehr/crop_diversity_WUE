# Sophie Ruehr
# Dec 30 2024
# Generate spatial figures and tables for analysis based on random forest output

rm(list = ls())
gc()
library(pacman)
p_load(ggplot2, data.table, here, plyr, dplyr, corrplot,ModelMetrics, plyr, tidyr, forcats,
       foreach, boot, parallel, doParallel, tibble, pdp, ranger, cowplot, readr,
       sf,  rnaturalearth, rnaturalearthdata,raster, patchwork, ggrepel, RColorBrewer)

# For figures
fig_wd <- here::here("outputs", "figures")
colors <- c("#7f3b08", "#e08214", "#fdb863", "#948c7c", "#7c7e99", "#b2abd2", "#8073ac", "#542788", "#2d004b")

# Set general text size
textsize = 10

# Select percentile for change between low and high diversity
quantiles <- c(0.1, 0.9)

# Set threshold to exclude if too few observations (per county & crop)
threshold = 10

# Read in data outputs from RF model --------------------------------------
# 1. WUE
model_filename_wue <- here::here("outputs", "model_output", paste0('RF_model_WUE.rds'))
model_wue <- readRDS(model_filename_wue)

training_filename_wue <- here::here("outputs", "model_output", paste0('RF_model_training_WUE.csv'))
testing_filename_wue <- here::here("outputs", "model_output", paste0('RF_model_testing_WUE.csv'))
testing <- fread(testing_filename_wue)
training <- fread(training_filename_wue)

# 2. GPP
model_filename_gpp <- here::here("outputs", "model_output", paste0('RF_model_GPP.rds'))
model_gpp <- readRDS(model_filename_gpp)

# Read in shapefiles ------------------------------------------------------
# Read shapefiles & reproject
california <- st_read("/Users/sophieruehr/Documents/Academic/Berkeley/Research/final_chapter/data.nosync/raw/TIGER_CA_shapefile/CA_shape/tl_2019_06_cousub.shp")
central_valley <- st_read("/Users/sophieruehr/Documents/Academic/Berkeley/Research/final_chapter/data.nosync/raw/boser+2024/study_area/study_area.shp")
dwr_sh <- st_read('/Users/sophieruehr/Documents/Academic/Berkeley/Research/final_chapter/data.nosync/raw/DWR/i15_Crop_Mapping_2020/i15_Crop_Mapping_2020.shp')
counties <- st_read('/Users/sophieruehr/Documents/Academic/Berkeley/Research/final_chapter/data.nosync/raw/ca_counties/CA_Counties.shp')
ca_counties_wgs84 <- st_transform(counties, crs = 4326)
ca_valley <- st_transform(central_valley, crs = 4326)
ca_counties_wgs84 <- ca_counties_wgs84 %>% 
  dplyr::select(geometry, NAME)

# Subdivide CA counties by central valley outline
subdivided_geometries <- st_intersection(ca_counties_wgs84, ca_valley)
subdivided_sf <- st_as_sf(subdivided_geometries) %>% 
  mutate(county = NAME) %>% dplyr::select(-NAME, -id, -FID, -FID_1)

# Include short names of counties
counties_interest <- data.frame(
  county = c(
    'Contra Costa', 'Butte', 'San Joaquin', 'Kings', 'Colusa', 
    'Sutter', 'Sacramento', 'Yolo', 'Solano', 'Fresno', 
    'Tulare', 'Merced', 'Kern', 'Glenn', 'Madera', 'Stanislaus'
  ),
  label = c(
    'CC', 'BU', 'SJ', 'K', 'CO', 
    'SU', 'SA', 'YO', 'SO', 'FR', 
    'TU', 'ME', 'KE', 'GL', 'M', 'ST'
  )) %>%
  mutate(name = paste0(county, ' (', label, ')')) 

# Get centroids of intersected counties for plotting
centroids <- st_centroid(subdivided_sf) %>% 
  mutate(X = st_coordinates(geometry)[,1],
         Y = st_coordinates(geometry)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(X, Y, county) %>% 
  merge(counties_interest, by = 'county') %>% 
  mutate(X = ifelse(label == 'ME', X - 0.2, X),
         Y = ifelse(label == 'ME', Y - 0.2, Y)) %>%
  mutate(X = ifelse(label == 'FR', X - 0.2, X),
         Y = ifelse(label == 'FR', Y - 0.2, Y)) %>%
  mutate(X = ifelse(label == 'K', X - 0.1, X),
         Y = ifelse(label == 'K', Y - 0.1, Y)) %>%
  mutate(X = ifelse(label == 'KE', X + -0.3, X)) %>%
  mutate(X = ifelse(label == 'ST', X - 0.1, X),
         Y = ifelse(label == 'ST', Y - 0.2, Y))

# Merge with centroid info
subdivided_sf <- subdivided_sf %>% merge(centroids, by = 'county')

# Predict under low & high diversity ----------------------------------------------------
opt_data <- rbind(training, testing)
# Decide high and low diversity based on bins for each crop & county
div_bins <- opt_data %>% 
  group_by(crop, county) %>% 
    # group_by(crop) %>% 
  filter(n() > threshold) %>% 
  summarise_at(.vars = c('diversity'),
               .funs = list(low_div =function(x){quantile(x, quantiles[1])},
                            high_div = function(x){quantile(x, quantiles[2])})) %>% 
  filter(!low_div == high_div) # Remove no data rows


# Input low and high diversity bins
opt_data <- opt_data %>% 
  merge(div_bins, by= c('county', 'crop'))

# Predict WUE under high & low diversity
opt_data$diversity_obs <- opt_data$diversity
opt_data$diversity <- opt_data$low_div
opt_data$wue15 <- predict(model_wue, data = opt_data)$predictions
opt_data$gpp15 <- predict(model_gpp, data = opt_data)$predictions
opt_data$diversity <- opt_data$high_div
opt_data$wue85 <- predict(model_wue, data = opt_data)$predictions
opt_data$gpp85 <- predict(model_gpp, data = opt_data)$predictions

# Convert scaled to original values
data_o <- opt_data %>%
  mutate(
    
    # Scale into original units from z-score
    wue_pred_o = (pred * wue_sd) + wue_mean,
    wue85_o = (wue85 * wue_sd) + wue_mean,
    wue15_o = (wue15 * wue_sd) + wue_mean,
    
    gpp_pred_o = (pred * gpp_sd) + gpp_mean,
    gpp85_o = (gpp85 * gpp_sd) + gpp_mean,
    gpp15_o = (gpp15 * gpp_sd) + gpp_mean,
    
  ) %>% 
  mutate(
    
    # # Calculate % change
    percent_change_wue = ((wue85 - wue15) / abs(wue15)) * 100,
    percent_change_wue_o = ((wue85_o - wue15_o) / abs(wue15_o)) * 100,
    
    percent_change_gpp = ((gpp85 - gpp15) / abs(gpp15)) * 100,
    percent_change_gpp_o = ((gpp85_o - gpp15_o) / abs(gpp15_o)) * 100,
    
    # Calculate water savings by dividing observed GPP by predicted WUE
    et_pred_o = gpp / wue_pred_o,
    et85_o = gpp / wue85_o,
    et15_o = gpp / wue15_o,
    
    # Calculate water savings (difference between low and high diversity)
    water_savings = (et15_o - et85_o) * 30, # Convert from mm per day to mm per month
    water_savings_percent_change = ((et15_o - et85_o) / abs(et15_o)) * 100
  )


# Pick counties to focus on -----------------------------
few_obs <- c('Butte', 'Contra Costa', 'Sacramento', 'Stanislaus', 
             'Madera', 'Solano', 'Kings', 'Colusa')
centroids_of_interest <- centroids %>% filter(!county %in% few_obs) 

# Make maps ---------------------------------------------------------------
textsize = 8
hex_bin <- 70
county_lab_sz <- 2.4

# Function to create maps
make_map <- function(data = data_o, variable, break_seq, break_labs, palette, legend_label_expr,
                     facet_flag = F, add_county_names = T,
    textsizze = textsize, hex_bins = hex_bin, county_lab_size = county_lab_sz, legendsize = 0.2) {
  map_out <- data %>% 
    mutate(percent_change_binned = cut(!!sym(variable),right = FALSE,
                                       breaks = break_seq)) %>%
    ggplot() +
    geom_sf(data = ca_counties_wgs84, fill = "white", color = "#858585", size = 0.4) + # California counties
    geom_sf(data = central_valley, fill = NA, color = 'black', linewidth = 0.5)  + # Central Valley outline
    stat_summary_hex(aes(x=x, y=y, z = !!sym(variable),
                         fill = cut(after_stat(value), 
                                    breaks = break_seq, 
                                    labels = break_labs)), 
                     bins = hex_bins, fun = mean) +   
    scale_fill_manual(values = palette) + 
    geom_sf(data = ca_counties_wgs84, fill = NA, color = "#858585", size = 0.4) + # California counties
    theme_minimal() +
    labs(x='', y= '', fill = legend_label_expr) +
    theme_minimal() +
    theme(
      legend.key.size = unit(legendsize, "cm"),
      axis.text = element_text(size = textsizze, color = 'black'),
      legend.text = element_text(size = textsizze),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 14),
      plot.margin = margin(0, 0, 0, 0),
      legend.title = element_text(size = textsizze + 1, face = 'bold')) +
    scale_y_continuous(labels = c('36N', '38N', '40N'),
                       breaks = c(36, 38, 40),
                       limits = c(34.9, 40.1)) +
    scale_x_continuous(labels = c('122W', '120W'),
                       breaks = c(-122, -120),
                       limits = c(-123, -118.4)) 
  
  if (add_county_names == T) {
    map_out <- map_out +   geom_label_repel(data = centroids_of_interest,
                                            aes(x = X, y = Y, label = name),
                                            size = county_lab_size,
                                            force = 30, # Repulsion strength for the labels
                                            segment.size = 0.5, # Arrow line thickness
                                            point.padding = 0.1,
                                            fontface = 'bold', box.padding = 0.1,
                                            segment.color = 'black',
                                            max.overlaps = 20,
                                            nudge_x = ifelse(centroids_of_interest$label %in% c('YO', 'ME', 'FR', 'KE'),
                                                             -1, ifelse(centroids_of_interest$label == 'TU', 0.7, 
                                                                        ifelse(centroids_of_interest$label == 'SJ', 1.3, 0.8))),
                                            nudge_y = ifelse(centroids_of_interest$label %in% c('SJ', 'GL'), 0.3,
                                                             ifelse(centroids_of_interest$label == 'TU', 0.6, 0)))
  }

  if (facet_flag == T) {
    map_out <- map_out +
      facet_wrap(~crop, nrow = 1)  +
      theme(legend.position = c(0.95, 0.9))
  }
  
  if (facet_flag == F) {
    map_out <- map_out +
      theme(legend.position = c(0.81, 0.87))
  }
  return(map_out)
  
}

# % Change WUE observed by crop
break_seq_wue <- c(-1e6, seq(0, 20, by = 5), 1e100)
break_labs_wue <- c('< 0', '0-5', '5-10', '10-15', '15-20', '> 20')
blue_palette <- c('orange', brewer.pal(length(break_labs_wue), "Blues")[-1])
del_wue <- make_map(variable = 'percent_change_wue_o', facet_flag = F,
         break_seq = break_seq_wue, break_labs = break_labs_wue,
         palette = blue_palette, legend_label_expr = expression(Delta*'WUE (%)'))
del_wue_by_crop <- make_map(variable = 'percent_change_wue_o', facet_flag = T,add_county_names = F,
                    break_seq = break_seq_wue, break_labs = break_labs_wue,
                    palette = blue_palette, legend_label_expr = expression(Delta*'WUE (%)'))

# % Change GPP by crop
break_seq_gpp <- c(-1e6, seq(0, 32, by = 8), 1e100)
break_labs_gpp <- c('< 0', '0-8', '8-16', '16-24', '24-32', '> 32')
green_palette <- c('orange', brewer.pal(length(break_labs_gpp), "Greens")[-1])
del_gpp <- make_map(variable = 'percent_change_gpp_o', facet_flag = F,add_county_names = F,
                    break_seq = break_seq_gpp, break_labs = break_labs_gpp,
                    palette = green_palette, legend_label_expr = expression(Delta*'GPP (%)'))
del_gpp_by_crop <- make_map(variable = 'percent_change_gpp_o', facet_flag = T,add_county_names = F,
                    break_seq = break_seq_gpp, break_labs = break_labs_gpp,
                    palette = green_palette, legend_label_expr = expression(Delta*'GPP (%)'))
  
# Water savings in mm per month by crop
break_seq_et <- c(-1e6, seq(0, 20, by = 5), 1e100)
break_labs_et <- c('< 0', '0-5', '5-10', '10-15','15-20','> 20')
purple_palette <- c('orange', brewer.pal(6, "Purples")[-1])
del_water <- make_map(variable = 'water_savings', facet_flag = F,add_county_names = F,
                    break_seq = break_seq_et, break_labs = break_labs_et,
                    palette = purple_palette, legend_label_expr = expression(Delta*'ET (mm)'))
del_water_by_crop <- make_map(variable = 'water_savings', facet_flag = T,add_county_names = F,
                      break_seq = break_seq_et, break_labs = break_labs_et,
                      palette = purple_palette, legend_label_expr = expression(Delta*'ET (mm)')) 

# Arrange plots
arranged_plots <- cowplot::plot_grid(del_wue, 
                                     del_gpp + theme(axis.text.y = element_blank()), 
                                     del_water+ theme(axis.text.y = element_blank()), 
                                     nrow = 1, align = 'v', labels = 'auto', 
                                     label_size = 16,
                                     label_x = 0.09, label_y = 0.995)

arranged_plots_by_crop <- cowplot::plot_grid(del_wue_by_crop + theme(axis.text.x = element_blank()), 
                                     del_gpp_by_crop + theme(axis.text.x = element_blank(),
                                                             strip.text = element_blank()), 
                                     del_water_by_crop + theme(strip.text = element_blank()), 
                                     ncol = 1, align = 'v', labels = 'auto', label_size = 16,
                                     label_x = 0.03, label_y = 0.15)

# Boxplots ----------------------------------------------------------------
data_boxplot <- centroids %>% dplyr::select(county, name, label) %>% 
  merge(data_o, by = 'county', all = T)

legendsize <- 0.4

make_boxplot <- function(variable, lim, data = data_boxplot, axis_label,
                         counties = unique(centroids_of_interest$county)) {
  data %>% 
    filter(!!sym(variable) > lim[1], !!sym(variable) < lim[2]) %>% 
    filter(county %in% counties) %>% 
    ggplot(aes(x = label, y = !!sym(variable) , col = crop)) +
    geom_boxplot(outlier.shape = NA, width = 0.7, linewidth = 0.3) +
    geom_hline(yintercept = 0, linetype = 'dotted', linewidth = 0.4)+
    theme_bw() +
    xlab('') +ylab(axis_label) + labs(color = '') +
    theme(legend.text = element_text(size = textsize),
          legend.key.size = unit(legendsize, "cm"),
          legend.title = element_blank(),
          axis.text =  element_text(size= textsize, color = 'black'),
          axis.title =  element_text(size= textsize + 2),
          axis.title.x = element_blank(),
          plot.margin = unit(rep(0.1, 4), "cm")) +
    scale_color_manual(values = colors[c(1,3,5,8)]) 

  }

et_box <- make_boxplot(variable = 'water_savings', lim = c(-30, 50), axis_label = expression(Delta*'ET (mm)'))
wue_box <- make_boxplot(variable = 'percent_change_wue_o', lim = c(-40, 40), axis_label = expression(Delta*'WUE (%)'))
gpp_box <- make_boxplot(variable = 'percent_change_gpp_o', lim = c(-35, 80), axis_label = expression(Delta*'GPP (%)'))

box_plots <- cowplot::plot_grid(wue_box + theme(axis.text.x = element_blank(),
                                   axis.ticks.x = element_blank(),
                                   legend.position = c(0.1, 0.2),
                                   legend.background = element_rect(color = 'black', linewidth = 0.3)), 
                   gpp_box + theme(axis.text.x = element_blank(),
                                   axis.ticks.x = element_blank(),
                                   legend.position = 'none'), 
                   et_box + theme(legend.position = 'none'),
                   ncol = 1, align = 'v', rel_heights = c(1, 1, 1.3))

box_plots_both <- cowplot::plot_grid(wue_box + theme(legend.position = c(0.5, 0.1),
                                                     legend.direction = 'horizontal',
                                                     legend.background = element_rect(color = NA, fill = NA)),
                                     gpp_box + theme(legend.position = 'none'), 
                                     et_box + theme(legend.position = 'none'),
                                     nrow = 1, align = 'v', label_size = 14,
                                     labels = c('d', 'e', 'f'), label_x = 0.16, label_y = 0.98)


# Save figures ------------------------------------------------------------
map_and_box <- plot_grid(arranged_plots,
                  box_plots_both, ncol = 1,
                  rel_heights = c(3, 1))

filename_both <- paste0(fig_wd, '/maps.pdf')
ggsave(filename_both, 
       map_and_box, width = 9.4, height = 5.5, units = 'in')

# # Save supplementary maps
maps_filename_by_crop <- paste0(fig_wd, '/supplement/sup_maps.pdf')
ggsave(maps_filename_by_crop, arranged_plots_by_crop, width = 8.8, height = 9.6, units = 'in')


# Estimate median values --------------------------------------------------
mean(data_o$percent_change_wue_o)
sd(data_o$percent_change_wue_o)


max(data_o$percent_change_gpp_o)
sd(data_o$percent_change_gpp_o)

median(data_o$water_savings)
sd(data_o$water_savings)


data_o %>% 
  group_by(crop, county) %>% 
  summarise(med = mean(percent_change_wue_o),
            sd = sd(percent_change_wue_o)) %>% 
  arrange(desc(med))

data_o %>% 
  filter(crop == 'Wheat') %>% 
  group_by(county) %>% 
  summarise(med = mean(percent_change_wue_o),
            sd = sd(percent_change_wue_o)) %>% 
  arrange(desc(med))


data_o %>% 
  group_by(crop, county) %>%
  summarise(med = mean(water_savings),
            sd = sd(water_savings)) %>% 
  arrange(desc(med))

t.test(data_o$water_savings[data_o$crop == 'Cotton'])
sd(data_o$water_savings)
sd(data_o$water_savings)


# Combine with acerage ---------------------------------------------------
acerage <- read.csv(here::here('data', 'processed_data', 'DWR', '2020_acerage.csv'))
total_saved <- acerage %>%
  mutate(crop = ifelse(short_name == 'Tomatoes (processing)', 'Tomatoes', short_name)) %>%
  dplyr::select(county,hectares, crop,class2) %>%
  merge(data_boxplot, by = c('county', 'crop')) %>% 
  filter(water_savings > 0) %>% # Only in areas where diversity is actually a benefit
  group_by(county, crop) %>% 
  mutate(water_savings = mean(water_savings)) %>% 
  mutate(water_saved = water_savings * hectares* 10,# To cubic meters
         total_et = et * hectares * 10 * 30) %>% # To cubic meters & monthly
  group_by(county, crop) %>% 
  summarise_at(.vars = c('water_saved', 'total_et'), .fun = mean) %>%
  mutate(perc_saved = water_saved / total_et * 100)

# If implemented in areas where this benefits, we could see a
sum(total_saved$water_saved) # m3 water savings per month
sum(total_saved$water_saved) / sum(total_saved$total_et) * 100 # percent water saved

# In Fresno, saving about 10% of water...
fresno <- total_saved %>% filter(county == 'Fresno')
total_saved %>% 
  group_by(county, crop) %>% 
  summarise(perc_saved = sum(water_saved) / sum(total_et) * 100,
            sum(water_saved))
sum(fresno$water_saved) / sum(fresno$total_et) * 100

# Save tables -------------------------------------------------------------
# Table
by_crop_county <- data_o %>% 
  dplyr::select(county, water_savings, percent_change_wue_o, percent_change_gpp_o, crop) %>% 
  # filter(county %in% centroids_of_interest$county) %>% 
  group_by(county, crop) %>% 
  summarise(et = mean(water_savings),
            et_sd = sd(water_savings),
            wue = mean(percent_change_wue_o),
            wue_sd = sd(percent_change_wue_o),
            gpp = mean(percent_change_gpp_o),
            gpp_sd = sd(percent_change_gpp_o)) %>% 
  merge(total_saved %>% dplyr::select(water_saved, county, crop), by = c('county', 'crop')) %>% 
  mutate(water_saved_m3_per_month = round(water_saved)) %>% 
  mutate(water_savings_mm_per_month = paste(round(et, 2), 'pm', round(et_sd,2))) %>% 
  mutate(del_gpp_perc = paste(round(gpp, 2), 'pm', round(gpp_sd,2))) %>% 
  mutate(del_wue_perc = paste(round(wue, 2), 'pm', round(wue_sd,2))) %>% 
  dplyr::select(crop, county,del_wue_perc, del_gpp_perc, water_savings_mm_per_month, water_saved_m3_per_month)  %>% 
  arrange(crop, desc(water_saved_m3_per_month))

by_crop_county %>% 
  group_by(crop) %>% 
  summarise_at(.vars = 'water_saved_m3_per_month', .funs = 'sum')

by_crop_county %>% 
  group_by(county) %>% 
  summarise_at(.vars = 'water_saved_m3_per_month', .funs = 'sum')



by_crop <- data_o %>% 
  dplyr::select(county, water_savings, percent_change_wue_o, percent_change_gpp_o, crop) %>% 
  group_by(crop) %>% 
  summarise(et = mean(water_savings),
            et_sd = sd(water_savings),
            wue = mean(percent_change_wue_o),
            wue_sd = sd(percent_change_wue_o),
            gpp = mean(percent_change_gpp_o),
            gpp_sd = sd(percent_change_gpp_o)) %>% 
  mutate(water_savings_mm = paste(round(et, 2), 'pm', round(et_sd,2))) %>% 
  mutate(del_gpp_perc = paste(round(gpp, 2), 'pm', round(gpp_sd,2))) %>% 
  mutate(del_wue_perc = paste(round(wue, 2), 'pm', round(wue_sd,2))) %>% 
  dplyr::select(crop,del_wue_perc, del_gpp_perc, water_savings_mm)

full <- c('Full', 
          paste(round(mean(data_o$percent_change_wue_o),2), 'pm', round(sd(data_o$percent_change_wue_o),2)),
          paste(round(mean(data_o$percent_change_gpp_o),2), 'pm', round(sd(data_o$percent_change_gpp_o),2)),
          paste(round(mean(data_o$water_savings),2), 'pm', round(sd(data_o$water_savings),2)))
by_crop <- rbind(full, by_crop)

by_county <- data_o %>% 
  dplyr::select(county, water_savings, percent_change_wue_o, percent_change_gpp_o, crop) %>% 
  group_by(county) %>% 
  summarise(et = mean(water_savings),
            et_sd = sd(water_savings),
            wue = mean(percent_change_wue_o),
            wue_sd = sd(percent_change_wue_o),
            gpp = mean(percent_change_gpp_o),
            gpp_sd = sd(percent_change_gpp_o)) %>% 
  mutate(water_savings_mm = paste(round(et, 2), 'pm', round(et_sd,2))) %>% 
  mutate(del_gpp_perc = paste(round(gpp, 2), 'pm', round(gpp_sd,2))) %>% 
  mutate(del_wue_perc = paste(round(wue, 2), 'pm', round(wue_sd,2))) %>% 
  dplyr::select(county,del_wue_perc, del_gpp_perc, water_savings_mm)

write_table_wd <- here::here('outputs', 'tables')
write.csv(by_crop, paste0(write_table_wd, '/change_by_crop.csv'))
write.csv(by_crop_county, paste0(write_table_wd, '/change_by_crop_county.csv'))
write.csv(by_county, paste0(write_table_wd, '/change_by_county.csv'))

