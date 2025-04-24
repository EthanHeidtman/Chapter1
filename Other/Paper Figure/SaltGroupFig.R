# Script to create a 3-paneled plot for the salinity group

#### Load Packages ####
library(easypackages)
libraries('tidyverse', 'sf', 'ggplot2', 'dplyr', 'maps', 'mapdata', 'cowplot',
          'rnaturalearth', 'rnaturalearthdata', 'readxl', 'raster', 'viridis', 'cmocean', 
          'RColorBrewer', 'terra', 'ncdf4', 'tidync', 'stars', 'matlab', 'svglite')

#### Load in Data ####
# Set the working directory
setwd('/Users/ethanheidtman/Documents/Penn State/Fall 24/HdG Salinity/Paper Figure')

map <- ne_countries(scale = 'small', returnclass = 'sf')
rivers <- ne_download(scale = 'medium', type = 'rivers_lake_centerlines', returnclass = 'sf', category = 'physical') 
lakes <- ne_download(scale = 'medium', type = 'lakes',  returnclass = 'sf', category = 'physical')

# Panel 1 Saltwater Intrusion Locations with Rivers
      # River Mouths 
      river_mouths <- read_excel('/Users/ethanheidtman/Documents/Penn State/Fall 24/HdG Salinity/Paper Figure/Rivers_2.XLSX')
      river_mouths$Lat <- (river_mouths$...6 + river_mouths$...7 / 60 + river_mouths$...8 / 3600) * river_mouths$...5 # Convert to Decimal degrees
      river_mouths$Lon <- (river_mouths$...10 + river_mouths$...11 / 60 + river_mouths$...12 / 3600) * river_mouths$...9 # Convert to Decimal degrees
      river_mouths <- river_mouths %>%
        dplyr::select(2,13,14) %>%
        mutate_if(is.numeric, round, digits = 4)
      
      # panel1 <- ggplot() +
      #    geom_sf(data = map) +
      #    theme_bw() +
      #    geom_point(data = river_mouths, mapping = aes(x = Lon, y = Lat, color = "#B2182B"), size = 0.9) +
      #    geom_sf(data = st_as_sf(rivers), color = "#3182BD", na.rm = TRUE) +
      #    scale_linewidth_identity(0.5) +
      #    #geom_sf(data = st_as_sf(lakes), color = 'cornflowerblue') +
      #    labs(tag = '(A)') +
      #    coord_sf(expand = FALSE, ylim = c(-60, 90)) +
      #    scale_x_continuous(breaks = c(-180, -150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150, 180)) +
      #    scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90)) +
      #    scale_color_manual(values = c("#B2182B", "#3182BD"), labels = c('Saltwater Intrusion\nLocations', 'Major Rivers')) +
      #    theme(legend.title = element_blank(),
      #          axis.text.x = element_blank(),
      #          axis.title.x = element_blank(),
      #          axis.title.y = element_blank(),
      #          plot.tag = element_text(face = 'bold'),
      #          legend.text = element_text(size = 12),
      #          legend.justification = c(0,0.5)) +
      #    guides(color = guide_legend(override.aes = list(size = 3)))
      
      

# Panel 2: Population Density 
      population <- raster('gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_15_min.asc')
      crs(population) <- 'EPSG:4326'
      population <- as.data.frame(population, xy = TRUE)
      colnames(population) <- c('x', 'y', 'value')
      population <- population %>%
         mutate(PopBinned = cut(value, breaks = c(0, 1, 5, 25, 250, 1000, 30000)))
      population[population == as.numeric('-Inf')] <- NA
      population$value[population$value < 0] <- NA
      
     
      
      
      panel1 <- ggplot() +
         geom_sf(data = map) +
         theme_bw() +
         geom_raster(data = population, aes(x = x, y = y, fill = PopBinned), alpha = 0.8, na.rm = TRUE) +
         geom_point(data = river_mouths, mapping = aes(x = Lon, y = Lat), color =  "#2166AC", size = 3, alpha = 0.85) +
         #scale_color_manual(values = c("#2166AC"), labels = c('River Mouths & Bays')) +
         scale_fill_brewer(palette = 'Oranges', na.value = NA, na.translate = FALSE,
                           labels = c('<1', '1 to 5', '5 to 25', '25 to 250', '250 to 1000', '1000+')) +
         coord_sf(expand = FALSE, ylim = c(-60, 90)) +
         theme_bw() +
         scale_x_continuous(breaks = c(-180, -150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150, 180)) +
         scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90)) +
         labs(fill = 'Population density\n(people per km^2)', tag = 'a', color = 'Saltwater Intrusion\nLocations') +
         theme(axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_text(size = 12),
               plot.tag = element_text(size = 16),
               plot.margin = margin(t = 0.1, r = 0.01),
               legend.justification = c(0,0.5),
               legend.title = element_text(size = 12),
               legend.text = element_text(size = 12)) + 
         guides(color = guide_legend(override.aes = list(size = 3)))

# Panel 2: Sea Level Rise Projections and Future Dry Days Per Year (RCP8.5)
      # Sea Level Change
      slr <- raster('Fig9-28_new_masked.nc')
      crs(slr) <- 'EPSG:4326'
      # mask <- st_buffer(map, 1000, endCapStyle = 'ROUND')
      # slr <- mask(slr, map, inverse = TRUE, updatevalue = NA, updateNA = TRUE)
      slr <- as.data.frame(slr, xy = TRUE)
      colnames(slr) <- c('x', 'y', 'value')
      slr$value[slr$value < -1] <- NA
      slr$value[slr$value <= 0 & slr$value >= -0.2] <- NA
      slr <- slr %>%
         mutate(slrBinned = cut(value, breaks = c(-1, -0.2, -0.00001, 0, 0.2, 0.4, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4)))
     
    
      panel2 <- ggplot() + 
        geom_raster(data = slr, aes(x = x, y = y, fill = slrBinned), alpha = 0.8, na.rm = TRUE) +
        geom_sf(data = map) + 
        theme_bw() +
        scale_fill_manual(values = c( "#B2182B", "#F4A582", 'white', "#DEEBF7", "#D1E5F0", "#C6DBEF", "#92C5DE", "#6BAED6", "#4393C3", "#2166AC", "#08519C", "#053061", 'black'),
                          na.value = NA, na.translate = FALSE,
                          labels = c('-1 to -0.2', '-0.2 to 0', '0 to 0.2', '0.2 to 0.4', '0.4 to 0.6', '0.6 to 0.7', '0.7 to 0.8',  '0.8 to 0.9', '0.9 to 1.0', '1.0 to 1.1', '1.1 to 1.2',
                                     '1.2 to 1.3', '1.3 to 1.4')) +
        coord_sf(expand = FALSE, ylim = c(-60, 90)) +
        scale_x_continuous(breaks = c(-180, -150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150, 180)) +
        scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90)) +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.tag = element_text(size = 16),
              axis.ticks.y = element_line(),
              legend.justification = c(0,0.5),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12)) + 
        labs(tag = 'b', fill = 'Sea-level rise (m)')
      
      
    
      # Dry Days Change
      drydays <- raster('Fig8-15_ocean_masked_meridian.nc')
      crs(drydays) <- 'EPSG:4326'
      drydays <- as.data.frame(drydays, xy = TRUE)
      colnames(drydays) <- c('x', 'y', 'value')
      drydays$value[drydays$value == as.character('-Inf')] <- NA
      drydays <- drydays %>%
         mutate(daysBinned = cut(value, breaks = c(-32, -16, -8, -4, -2, 0, 2, 4, 8, 16, 32, 48)))
      
      sf_use_s2(FALSE)
      panel3 <- ggplot() + 
        geom_sf(data = map) + 
        theme_bw() +
        geom_raster(data = drydays, aes(x = x, y = y, fill = daysBinned), na.rm = TRUE, alpha = 0.8) + 
        scale_fill_brewer(palette = 'BrBG', na.value = NA, na.translate = FALSE, direction = -1, guide = 'legend',
                          labels = c('-32 to -16', '-16 to -8', '-8 to -4', '-4 to -2', '-2 to 0', '0 to 2', '2 to 4', '4 to 8', '8 to 16', '16 to 32', '32+')) + 
        coord_sf(expand = FALSE, ylim = c(-60, 90)) +
        scale_x_continuous(breaks = seq(-180, 180, 30)) +
        scale_y_continuous(breaks = seq(-60, 90, 30)) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 12),
              plot.tag = element_text(size = 16),
              legend.title = element_text(size = 12),
              legend.justification = c(0,0.5),
              legend.text = element_text(size = 12)) +
        labs(tag = 'c', fill = 'Change in dry days')
      
      
      
      plot <- plot_grid(panel1, panel2, panel3, ncol = 1, nrow = 3, align = 'hv')
      
      ggsave(filename = 'UpdatedFigV5.png', width = 15, height = 15, plot, dpi = 600,
             path = '/Users/ethanheidtman/Documents/Penn State/Fall 24/HdG Salinity/Paper Figure')
      

      
      
   

# # Create Panel 1
# panel1 <- ggplot() + 
#    geom_sf(data = map) + 
#    theme_bw() + 
#    geom_point(data = river_mouths, mapping = aes(x = Lon, y = Lat, color = "#E6550D"), size = 0.9) + 
#    geom_sf(data = st_as_sf(rivers), color = "#3182BD", na.rm = TRUE) + 
#    scale_linewidth_identity(0.5) +
#    #geom_sf(data = st_as_sf(lakes), color = 'cornflowerblue') + 
#    labs(tag = '(A)') + 
#    coord_sf(expand = FALSE, ylim = c(-60, 90)) + 
#    scale_x_continuous(breaks = c(-180, -150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150, 180)) + 
#    scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90)) + 
#    scale_color_manual(values = c("#E6550D", "#3182BD"), labels = c('Saltwater Intrusion\nLocations', 'Major Rivers')) + 
#    theme(legend.title = element_blank(),
#          axis.text.x = element_blank(),
#          axis.title.x = element_blank(),
#          axis.title.y = element_blank(),
#          plot.tag = element_text(face = 'bold'),
#          legend.text = element_text(size = 12),
#          legend.justification = c(0,0.5)) + 
#    guides(color = guide_legend(override.aes = list(size = 3)))
# 
# 
# # Create Panel 2 From van Vliet et al 2021 Figure 2
# scarcity <- raster('fig2a_WSq_Tot_ann_global.asc')
# scarcity <- as.data.frame(scarcity, xy = TRUE)
# colnames(scarcity) <- c('x', 'y', 'value')
# scarcity$value[scarcity$value == as.character('-Inf')] <- NA
# scarcity$value[scarcity$value > 100] <- NA
# scarcity <- scarcity %>%
#    mutate(ScarcityBinned = cut(value, breaks = c(0, 0.1, 0.2, 0.4, 1, 100)))
# 
# 
# panel2 <- ggplot() +  
#    geom_sf(data = map) + 
#    geom_raster(data = scarcity, aes(x = x, y = y, fill = ScarcityBinned), alpha = 0.8, na.rm = TRUE) +
#    scale_fill_brewer(palette = 'Oranges', na.value = NA, na.translate = FALSE, 
#                      labels = c('0-0.1', '0.1-0.2', '0.2-0.4', '0.4-1', '1+')) +
#    coord_sf(expand = FALSE, ylim = c(-60, 90)) + 
#    theme_bw() + 
#    scale_x_continuous(breaks = c(-180, -150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150, 180)) + 
#    scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90)) + 
#    labs(fill = 'Water Scarcity (including\nwater quality, unitless ratio)', tag = '(B)') + 
#    theme(axis.title.x = element_blank(),
#          axis.title.y = element_blank(),
#          axis.text.x = element_blank(),
#          plot.tag = element_text(face = 'bold'),
#          plot.margin = margin(t = 0.1),
#          legend.justification = c(0,0.5),
#          legend.title = element_text(size = 12))
# 
# 
# # slr <- raster('slr_map_ref.nc')
# # slr <- as.data.frame(slr, xy = TRUE)
# # colnames(slr) <- c('x', 'y', 'value')
# # slr$value[is.nan(slr$value)] <- NA
# # slr$values_na <- ifelse(slr$value == 0, NA, slr$value)
# # 
# # panel2 <- ggplot() + 
# #    geom_sf(data = map) + 
# #    geom_raster(data = slr, aes(x = x, y = y, fill = value), alpha = 0.8, na.rm = TRUE) + 
# #    scale_fill_viridis(na.value = NA, option = 'rocket') +
# #    coord_sf(expand = FALSE, ylim = c(-60, 90)) + 
# #    theme_bw() + 
# #    scale_x_continuous(breaks = c(-180, -150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150, 180)) + 
# #    scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90)) + 
# #    labs(fill = 'Sea Level Rise\n(mm/yr)', tag = '(B)') + 
# #    theme(axis.text.x = element_blank(),
# #          axis.title.x = element_blank(),
# #          axis.title.y = element_blank(),
# #          plot.tag = element_text(face = 'bold'), 
# #          plot.margin = margin(b = 0.1))
# 
# 
# 
# 
# # Create Panel 3
# population <- raster('gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_15_min.asc')
# population <- as.data.frame(population, xy = TRUE)
# colnames(population) <- c('x', 'y', 'value')
# population <- population %>%
#    mutate(PopBinned = cut(value, breaks = c(0, 1, 5, 25, 250, 1000, 30000))) 
# population[population == as.numeric('-Inf')] <- NA
# population$value[population$value < 0] <- NA
# 
# 
# panel3 <- ggplot() +  
#    geom_sf(data = map) + 
#    geom_raster(data = population, aes(x = x, y = y, fill = PopBinned), alpha = 0.8, na.rm = TRUE) +
#    scale_fill_brewer(palette = 'Blues', na.value = NA, na.translate = FALSE, 
#                      labels = c('<1', '1-5', '5-25', '25-250', '250-1000', '1000+')) +
#    coord_sf(expand = FALSE, ylim = c(-60, 90)) + 
#    theme_bw() + 
#    scale_x_continuous(breaks = c(-180, -150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150, 180)) + 
#    scale_y_continuous(breaks = c(-60, -30, 0, 30, 60, 90)) + 
#    labs(fill = 'Population Density\n(people per km^2)', tag = '(C)') + 
#    theme(axis.title.x = element_blank(),
#          axis.title.y = element_blank(),
#          plot.tag = element_text(face = 'bold'),
#          plot.margin = margin(t = 0.1, r = 0.01),
#          legend.justification = c(0,0.5),
#          legend.title = element_text(size = 12))
# 
# plot_grid(panel1, panel2, panel3,  ncol = 1, nrow = 3, align = 'hv')


# ggsave(filename = 'panel_plot_example4.png', width = 15, height = 15, p, path = path5)










