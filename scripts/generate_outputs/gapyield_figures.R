# Sophie Ruehr
# Dec 30 2024
# Gap yield figures creation

gc()
rm(list = ls())
library(pacman)
p_load(ggplot2, plyr, dplyr, tidyr, ggpubr,ggpattern, RColorBrewer,
       data.table, here, purrr, mgcv, wesanderson, ggrepel)

# Load data ---------------------------------------------------------------
data_file <- here::here('outputs', 'gapyield_output', 'temporal_averaging.csv')
data <- read.csv(data_file)

# Plot preferences --------------------------------------------------------
# Set figure parameters
colors <- c("#7f3b08", "#e08214", "#fdb863", "#948c7c", "#7c7e99", "#b2abd2", "#8073ac", "#542788", "#2d004b")
textsize <- 12

# Set figure save wd
save_fig_wd <- here::here('outputs', 'figures')

# Set table save wd
save_table_wd <- here::here('outputs', 'tables')

# Load shapefiles for maps ------------------------------------------------
p_load(sf, ggplot2, rnaturalearth, rnaturalearthdata,raster)
central_valley <- st_read("/Users/sophieruehr/Documents/Academic/Berkeley/Research/final_chapter/data.nosync/raw/boser+2024/study_area/study_area.shp")
ca_valley <- st_transform(central_valley, crs = 4326)
counties <- st_read('/Users/sophieruehr/Documents/Academic/Berkeley/Research/final_chapter/data.nosync/raw/ca_counties/CA_Counties.shp')
ca_counties_wgs84 <- st_transform(counties, crs = 4326) %>% dplyr::select(geometry, NAME)
subdivided_geometries <- st_intersection(ca_counties_wgs84, ca_valley)
subdivided_sf <- st_as_sf(subdivided_geometries)

# Include short names of counties
counties_interest <- data.frame(
  county = c('Contra Costa', 'Fresno', 'Kings','Merced','Sacramento',  'San Joaquin' ),
  label = c('CC', 'FR', 'K', 'ME', 'SA', 'SJ')) %>%
  mutate(name = paste0(county, ' (', label, ')')) 

# Get centroids of intersected counties for plotting
centroids <- st_centroid(subdivided_sf) %>% 
  mutate(X = st_coordinates(geometry)[,1],
         Y = st_coordinates(geometry)[,2]) %>% 
  as.data.frame() %>% 
  mutate(county = NAME) %>% 
  dplyr::select(X, Y, county) %>% 
  merge(counties_interest, by = 'county') %>% 
  mutate(X = ifelse(label == 'ME', X - 0.2, X),
         Y = ifelse(label == 'ME', Y - 0.2, Y)) %>%
  mutate(X = ifelse(label == 'FR', X - 0.2, X),
         Y = ifelse(label == 'FR', Y - 0.2, Y)) %>%
  mutate(X = ifelse(label == 'K', X - 0.1, X),
         Y = ifelse(label == 'K', Y - 0.1, Y)) 

# Merge with centroid info
subdivided_sf <- subdivided_sf %>% mutate(county = NAME) %>% 
  merge(centroids, by = 'county', all = T)

# Perform stats tests and modeling ----------------------------------------
# Test for significant differences between observed and null 95-50% distributions by temporal window and county
sig_diff <- data %>%
  filter(win < 5, win > 1) %>%
  group_by(win, county) %>%
  filter(n_distinct(cwe_scaled_spread_n) > 1 & n_distinct(cwe_scaled_spread_o) > 1) %>%
  summarize(
    t_test = list(t.test(cwe_scaled_spread_o, cwe_scaled_spread_n, var.equal = TRUE)),  # Assuming equal variance
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = map_dbl(t_test, ~ .x$statistic),
    p_value = map_dbl(t_test, ~ .x$p.value),
    p_adj = p.adjust(p_value, method = "bonferroni")  # Adjust p-values for multiple tests
  ) %>%
  dplyr::select(win, county, t_stat, p_value, p_adj)  %>% 
  filter(p_value < 0.05) %>% 
  arrange(county) %>% 
  group_by(county) %>% 
  add_count() %>% filter(n > 1) %>% 
  arrange(win)

# Line plots ---------------------------------------------------------------
significance_marker <- sig_diff %>% 
  dplyr::select(win, county) %>% 
  mutate(sig = 'p<0.05')
legendsize <- 0.4

data_summary <- data %>%
  group_by(county, win) %>%
  summarize(
    cwe_o = mean(cwe_scaled_spread_o),
    cwe_o_sd = sd(cwe_scaled_spread_o),
    div_o = mean(diversity_scaled_spread_o),
    div_o_sd = sd(diversity_scaled_spread_o),
    cwe_n = mean(cwe_scaled_spread_n),
    cwe_n_sd = sd(cwe_scaled_spread_n),
    div_n = mean(diversity_scaled_spread_n),
    div_n_sd = sd(diversity_scaled_spread_n),
    .groups = 'drop') %>% 
  na.omit()

data_summary %>% 
  filter(win == 4) %>% 
  summarise(median(cwe_o),
            sd(cwe_o))

lineplot <- data_summary %>% 
  filter(win < 5) %>% 
  merge(significance_marker, by = c('county', 'win'), all = T) %>% 
  mutate(sig = ifelse(is.na(sig), 'p>0.05', sig)) %>% 
  filter(county %in%  sig_diff$county) %>%
  ggplot(aes(x = win)) + 
  geom_ribbon(aes(ymin = div_n, 
                  ymax = div_o,
                  fill = 'Diversity'), alpha = 0.1,
              show.legend = c(fill = F)) +
  geom_path(aes(y = div_n, col = 'Diversity', linetype = 'Null'), alpha = 0.5) +
  geom_path(aes(y = div_o, col = 'Diversity', linetype = 'Observed')) +
  geom_ribbon(aes(ymin = cwe_n, ymax = cwe_o,
                  fill = 'WUE'), alpha = 0.15,
              show.legend = c(fill = F)) +
  geom_path(aes(y = cwe_n, col = 'WUE', linetype = 'Null'), alpha = 0.5) +
  geom_path(aes(y = cwe_o, col = 'WUE', linetype = 'Observed')) +
  
  geom_point(aes(y = div_o, col = 'Diversity', shape = sig)) +
  geom_point(aes(y = cwe_o, col = 'WUE', shape = sig)) +
  
  facet_wrap(~county)  +
  scale_linetype_manual(values = c('dashed', 'solid')) +
  scale_fill_manual(values = c(colors[7], colors[2]))+
  scale_color_manual(values = c(colors[7], colors[2])) +
  theme_bw() +
  theme(#legend.position = 'none',
        plot.margin = unit(rep(0.1, 4), "cm"),
        strip.text = element_blank(),
        legend.key.size = unit(legendsize, "cm"),
        axis.text = element_text(size = textsize, color = 'black'),
        axis.title = element_text(size = textsize)) +
  ylab(expression("WUE'g"~ '&' ~ "DIV'g")) + xlab('Averaging window')  +
  labs(fill = '', linetype = '', shape = '', col = '') +
  facet_wrap(~county, nrow = 2) +
  geom_text(data = sig_diff %>% merge(counties_interest, by = 'county', all = T), aes(x = 2.5, y = Inf, 
                                 vjust = 1.2, label = name), 
            size = textsize * 0.3) +
  scale_shape_manual(values = c(15, 2)) 


# Map plot ---------------------------------------------------------------
legendsize <- 0.4
county_lab_size <- 3
centroids$win <- 2
mapplot <- subdivided_sf %>% 
  mutate(county = NAME) %>% 
  merge(data_summary, by = 'county') %>% 
  merge(significance_marker, by = c('county', 'win'), all = T) %>% 
  mutate(sig = as.factor(ifelse(is.na(sig), 'Non-sig', sig))) %>% 
  filter(win < 5, win > 1) %>%
  # group_by(county) %>% 
  # summarise(cwe_o)
  ggplot() +
  geom_sf(data = ca_counties_wgs84, fill = "white", color = "black", size = 0.5) + # California outline
  geom_sf(data = central_valley, fill = NA, color = '#367dd9', linewidth = 0.7)  +
  labs(x='', y= '', fill = "WUE'g") +
  theme_bw() +
  theme(
    legend.key.size = unit(legendsize, "cm"),
    axis.text = element_text(size = textsize, color = 'black'),
    legend.text = element_text(size = textsize),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(rep(0.1, 4)),
    legend.title = element_text(size = textsize + 1)) +
  scale_y_continuous(labels = c('36N', '38N', '40N'),
                     breaks = c(36, 38, 40),
                     limits = c(34.9, 40.1)) +
  scale_x_continuous(labels = c('122W', '120W'),
                     breaks = c(-122, -120),
                     limits = c(-123, -118.4)) +
  facet_wrap(~win, nrow = 1) +
  geom_sf_pattern(aes(pattern = sig, 
                      fill = cwe_o),
                  pattern_density = 0.01,  
                  pattern_spacing = 0.04,  
                  pattern_color = "black", 
                  show.legend = c(pattern = FALSE, fill = TRUE), 
                  pattern_fill = NA,       
                  linewidth = 0)  +
  scale_fill_viridis_c(option = 'magma') +
  labs(pattern = '') +
  geom_sf(data = ca_counties_wgs84, fill = NA, color = "black", size = 0.5) + # California outline
  scale_pattern_manual(
    values = c('none','circle')) +
  geom_text(data= data_summary %>% filter(win > 1) %>%  distinct(win) , 
            aes(label = paste0(win)), 
            x = Inf, y = Inf, vjust = 1.2, hjust= 1, size = textsize * 0.4) +
  geom_label_repel(data = centroids,
                   aes(x = X, y = Y, label = label),
                   size = county_lab_size,
                   force = 30, # Repulsion strength for the labels
                   segment.size = 0.5, # Arrow line thickness
                   point.padding = 0.1,
                   fontface = 'bold', box.padding = 0.1,
                   segment.color = 'black',
                   max.overlaps = 20,
                   nudge_x = ifelse(centroids$label %in% c('ME', 'FR', 'K', 'CC'),
                                    -1, ifelse(centroids$label %in% c('SA', 'SJ'), 1.2, 0.8)),
                   nudge_y = ifelse(centroids$label %in% c('SJ', 'GL'), 0.3,
                                    ifelse(centroids$label == 'TU', 0.6, 0)))


# Scatter plot ------------------------------------------------------------
get_summary_lm <- function(window) {

  dat <- data_summary %>% 
    filter(win == window)
  lm_out <- summary(lm(dat$cwe_o ~ dat$div_o))
  return_vals <- data.frame(p =  lm_out$coefficients[2, 4],
                            r = lm_out$r.squared,
                            win = window) %>% 
    mutate(p = ifelse(p < 0.01, 'p<0.01', paste0('p=', round(p,2))),
           r = round(r,2))
  return(return_vals)
}

lm_vals <- lapply(c(2, 3, 4), get_summary_lm) %>% 
  bind_rows()

scatterplot <- data_summary %>% 
  filter(win > 1) %>% 
  ggplot(aes(y = cwe_o, 
             x = div_o)) +
  geom_smooth(method = 'lm', col= 'black', 
              alpha = 0.2, linewidth = 0.7) +
  geom_errorbar(data = data_summary %>% filter(!county %in% sig_diff$county,
                                               win > 1),
                aes(ymin = cwe_o - cwe_o_sd,
                    ymax = cwe_o + cwe_o_sd),
                alpha =0.4, col = 'darkgrey') +
  geom_errorbar(data = data_summary %>% filter(!county %in% sig_diff$county,
                                               win > 1),
                aes(xmin = div_o - div_o_sd,
                    xmax = div_o + div_o_sd),
                alpha = 0.4, col = 'darkgrey') +
  geom_point(data = data_summary %>% filter(!county %in% sig_diff$county,
                                            win > 1), 
             col = 'darkgrey',
             size = 0.7) +
  geom_errorbar(data = data_summary %>% filter(county %in% sig_diff$county,
                                               win > 1),
                aes(xmin = div_o - div_o_sd,
                    xmax = div_o + div_o_sd, col = county),
                alpha = 0.4) +
  geom_errorbar(data = data_summary %>% filter(county %in% sig_diff$county,
                                               win > 1),
                aes(ymin = cwe_o - cwe_o_sd,
                    ymax = cwe_o + cwe_o_sd, col = county),
                alpha =0.4) +
  geom_point(data = data_summary %>% filter(county %in% sig_diff$county,
                                            win > 1), 
             aes(col = county),
             size = 0.7) +
  facet_wrap(~win, nrow = 1) +
  theme_bw() +
  labs(x = expression("DIV'g"),
       y = expression("WUE'g"),
       col = '') +
  scale_color_manual(values = colors) +
  theme(
    legend.key.size = unit(legendsize * 0.8, "cm"),
    axis.text = element_text(size = textsize, color = 'black'),
    legend.text = element_text(size = textsize - 2),
    strip.text = element_blank(),
    legend.background = element_blank(),
    plot.margin = margin(rep(0.1, 4))) +
  geom_text(data= data_summary %>% filter(win > 1) %>%  distinct(win) , 
             aes(label = paste0(win)), 
            x = Inf, y = Inf, vjust = 1.2, hjust= 1, size = textsize * 0.4) +
  geom_text(data = lm_vals, aes(label =  sprintf("R²=%.2f\n%s",  r, p)),
            x = 0.2, y = Inf, vjust = 1.2, hjust= 0,size = textsize * 0.25)

# Combine figure panels & save ---------------------------------------------------
combined_plots <- cowplot::plot_grid(mapplot,
                                     scatterplot,
                                     lineplot,
                                     nrow = 3, align = 'hv', axis = 'lr',
                                     rel_heights = c(2, 1.4, 2.1),
                                     labels = 'auto', label_x = 0.085, 
                                     label_y = c(0.25, 0.35, 0.23))

ggsave(paste0(save_fig_wd, '/gapyield.pdf'), 
       combined_plots, device = cairo_pdf, width = 7, height =8.4)


# Save stats --------------------------------------------------------------
sig_diff_save <- subdivided_sf %>% 
  mutate(county = NAME) %>% 
  merge(data_summary, by = 'county') %>% 
  merge(significance_marker, by = c('county', 'win'), all = T) %>% 
  mutate(sig = as.factor(ifelse(is.na(sig), 'Non-sig', sig))) %>% 
  filter(win < 5, win > 1) %>% 
  as.data.frame() %>% 
  mutate(cwe = round(cwe_o, 2),
         cwe_sd = round(cwe_o_sd, 2),
         div = round(div_o, 2),
         div_sd = round(div_o_sd, 2)) %>% 
  mutate(CWEg = paste0(cwe, '$pm$', cwe_sd),
         DIVg = paste0(div, '$pm$', div_sd)) %>% 
  arrange(county) %>% 
  merge(sig_diff, by=c('win', 'county'), all = T) %>% 
  dplyr::select(win, county, CWEg, DIVg, p_value)  %>% 
  mutate(p_value = ifelse(is.na(p_value), 'p$>$0.05', p_value)) %>% 
  filter(win == 3)


write.csv(sig_diff_save, paste0(save_table_wd, '/gap_yield_sig.csv'))

# Median CWE gap
median(data_summary$cwe_o[data_summary$win == 3], na.rm = T)
sd(data_summary$cwe_o[data_summary$win == 3], na.rm = T)

# Print highest gaps
sig_diff_save %>% 
  filter(win == 3) %>% 
  filter(!p_value == 'p$>$0.05') %>%
  mutate(cwe = as.numeric(substr(CWEg, 1, 4))) %>% 
  arrange(desc(cwe)) 


#  Is diversity a significant driver? ------------------------------------------------------------------
# Use models to determine whether diversity is a driver of observed 95-50% spread
model_data <- data %>% filter(win > 1,win < 5) 

# Linear mixed says yes
model <- lm(cwe_scaled_spread_o ~ 
              diversity_scaled_spread_o + 
              annual_pr_spread_o + 
              vpd_scaled_spread_o +
              clay_spread_o +
              wtd_spread_o,
            data = model_data)
summary(model)

  # GAM says yes
k = 4
model <-  gam(cwe_scaled_spread_o ~ 
                s(diversity_scaled_spread_o, k = k) + 
                s(annual_pr_spread_o, k = k - 1) + 
                s(vpd_scaled_spread_o, k = k + 1) +
                s(clay_spread_o, k = k) +
                s(wtd_spread_o, k = k ),
              # te(wtd_spread_o, clay_spread_o, k = k),
              data = model_data)
summary(model)
gam.check(model)
plot(model, pages =1, residuals = T)

# Diversity is the strongest predictor
