# =============================================================================
# Script Name:    CaseStudyMap.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Case study figure for the tidal Susquehanna / upper
#                 Chesapeake Bay study area.
#                 Main panel: hillshade + edge-to-edge native bathymetry,
#                 dissolved water mask (Conowingo Pond, Bush River, Bay),
#                 named tributaries clipped to mouths, dam, labels, MD/PA line.
#                 Inset: state fills, fully-outlined bay tributaries, major rivers.
#                 Exports PNG + SVG at 600 dpi.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(sf)
library(terra)
library(nhdplusTools)
library(tigris)
library(elevatr)
library(rnaturalearth)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(ggnewscale)
library(patchwork)

# =============================================================================
# PARAMETERS
# =============================================================================

TARGET_CRS <- 26918   # NAD83 / UTM zone 18N

BATHY_PATH <- 'Data/MapData/chesapeake_bay_M130_2017.nc'
RES_MAIN_M <- 30
BATHY_NATIVE_RES_M <- 10  # Preserves native smooth bathymetry resolution

# Hillshade DEM via elevatr (USGS 3DEP over AWS)
ELEV_ZOOM <- 12
HILLSHADE_ALPHA <- 0.25

LAND_COLOR_MAIN     <- '#d9d4c7'
WATER_OUTLINE_COLOR <- '#002030'
WATER_OUTLINE_WIDTH <- 0.25
STREAM_COLOR        <- WATER_OUTLINE_COLOR   # Standardized to shoreline color
DAM_COLOR           <- '#002030'
ACCENT_COLOR        <- '#f58220'

INSET_BAY_COLOR     <- '#c6dbef'   # Matches main panel bay fill

INSET_RIVER_NAMES <- c('Potomac River', 'Patapsco River', 'Patuxent River',
                       'Susquehanna River', 'Choptank River', 'Nanticoke River',
                       'Rappahannock River', 'York River', 'James River')
INSET_RIVER_MIN_STREAMORDE <- 6

POTOMAC_DC_LAT_CUTOFF <- 38.95

# Additional tributaries to include in the main panel
STREAM_NAMES_INCLUDE_EXTRA <- c('Deer Creek', 'Octoraro Creek', 'Principio Creek', 
                                'Basin Run', 'Little Elk Creek', 'North East Creek')

# Exclude flowlines for waterbodies already represented as 2D area/estuary/reservoir polygons
STREAM_NAMES_EXCLUDE <- c('Susquehanna River', 'North East River', 'Elk River', 
                          'Bohemia River', 'Back River', 'Sassafras River', 'Bush River')

EXTEND_DIST_M    <- 400
GAP_THRESHOLD_M  <- 1000

fig_dir <- 'Outputs/Plots'
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# =============================================================================
# EXTENTS
# =============================================================================

main_bbox <- st_bbox(
   c(xmin = -76.34, xmax = -75.90, ymin = 39.43, ymax = 39.74),
   crs = st_crs(4326)
)

inset_bbox <- st_bbox(
   c(xmin = -78.5, xmax = -74.5, ymin = 37.0, ymax = 40.15),
   crs = st_crs(4326)
)

stopifnot(
   main_bbox["xmin"] >= inset_bbox["xmin"],
   main_bbox["xmax"] <= inset_bbox["xmax"],
   main_bbox["ymin"] >= inset_bbox["ymin"],
   main_bbox["ymax"] <= inset_bbox["ymax"]
)

main_bbox_sf  <- st_as_sfc(main_bbox)
inset_bbox_sf <- st_as_sfc(inset_bbox)

make_utm_box <- function(bbox_wgs84, target_crs, snap_m = 100) {
   proj_bbox <- st_bbox(st_transform(st_as_sfc(bbox_wgs84), target_crs))
   proj_bbox['xmin'] <- floor(proj_bbox['xmin'] / snap_m) * snap_m
   proj_bbox['ymin'] <- floor(proj_bbox['ymin'] / snap_m) * snap_m
   proj_bbox['xmax'] <- ceiling(proj_bbox['xmax'] / snap_m) * snap_m
   proj_bbox['ymax'] <- ceiling(proj_bbox['ymax'] / snap_m) * snap_m
   proj_bbox
}

main_bbox_utm  <- make_utm_box(main_bbox, TARGET_CRS)
inset_bbox_utm <- make_utm_box(inset_bbox, TARGET_CRS)

main_bbox_utm_sf  <- st_as_sfc(main_bbox_utm)
inset_bbox_utm_sf <- st_as_sfc(inset_bbox_utm)

clip_to_utm <- function(x, utm_box_sf, target_crs) {
   x %>%
      st_transform(target_crs) %>%
      st_make_valid() %>%
      st_filter(utm_box_sf) %>%
      st_intersection(utm_box_sf)
}

# =============================================================================
# LOAD DATA -- MAIN PANEL HYDROGRAPHY
# =============================================================================

cat("\nPulling NHDPlus HR for the main panel...\n")

huc_ids <- get_huc(AOI = main_bbox_sf, type = 'huc08')

nhdplushr_dir <- download_nhdplushr(
   nhd_dir = 'Data/MapData/NHDPlusHR',
   hu_list = huc_ids$huc8
)

nhd_gdb <- get_nhdplushr(
   nhdplushr_dir,
   layers = c('NHDArea', 'NHDFlowline', 'NHDWaterbody')
)

# Estuaries (493) and Reservoirs/Lakes (390) > 0.05 km2
nhd_main_waterbody <- nhd_gdb$NHDWaterbody %>%
   st_zm(drop = TRUE, what = 'ZM') %>%
   filter(FTYPE %in% c(493, 390), AreaSqKM > 0.05) %>%
   clip_to_utm(main_bbox_utm_sf, TARGET_CRS)

nhd_main_area <- nhd_gdb$NHDArea %>%
   st_zm(drop = TRUE, what = 'ZM') %>%
   filter(FTYPE == 460, AreaSqKM > 0.05) %>%
   clip_to_utm(main_bbox_utm_sf, TARGET_CRS)

nhd_main_flowline_all <- nhd_gdb$NHDFlowline %>%
   st_zm(drop = TRUE, what = 'ZM') %>%
   clip_to_utm(main_bbox_utm_sf, TARGET_CRS)

STREAM_MIN_ORDER <- 5

nhd_main_streams <- nhd_main_flowline_all %>%
   filter(!is.na(gnis_name), gnis_name != '', !gnis_name %in% STREAM_NAMES_EXCLUDE) %>%
   group_by(gnis_name) %>%
   filter(any(StreamOrde >= STREAM_MIN_ORDER) | first(gnis_name) %in% STREAM_NAMES_INCLUDE_EXTRA) %>%
   ungroup()

# Combine all water geometries and completely dissolve to remove interior seams
water_mask <- st_union(st_geometry(nhd_main_waterbody), st_geometry(nhd_main_area)) %>%
   st_union() %>%
   st_make_valid()

# Extract shorelines and remove border segments touching the frame edge
main_box_boundary <- st_boundary(main_bbox_utm_sf)
water_boundary    <- st_boundary(water_mask)
shoreline_lines   <- st_difference(water_boundary, st_buffer(main_box_boundary, 20))

extend_toward_water <- function(line_sfg, water, extend_dist, gap_threshold) {
   coords <- st_coordinates(line_sfg)[, c('X', 'Y'), drop = FALSE]
   n <- nrow(coords)
   if (n < 2) return(line_sfg)
   
   start_pt <- st_sfc(st_point(coords[1, ]), crs = st_crs(water))
   end_pt   <- st_sfc(st_point(coords[n, ]), crs = st_crs(water))
   start_dist <- as.numeric(st_distance(start_pt, water))
   end_dist   <- as.numeric(st_distance(end_pt, water))
   
   mouth_is_start <- start_dist <= end_dist
   mouth_dist <- if (mouth_is_start) start_dist else end_dist
   if (mouth_dist > gap_threshold) return(line_sfg)
   
   if (mouth_is_start) {
      dir <- coords[1, ] - coords[2, ]
      dir <- dir / sqrt(sum(dir^2))
      new_pt <- coords[1, ] + dir * extend_dist
      coords <- rbind(new_pt, coords)
   } else {
      dir <- coords[n, ] - coords[n - 1, ]
      dir <- dir / sqrt(sum(dir^2))
      new_pt <- coords[n, ] + dir * extend_dist
      coords <- rbind(coords, new_pt)
   }
   st_linestring(coords)
}

nhd_main_streams <- nhd_main_streams %>%
   mutate(geometry = st_sfc(
      lapply(st_geometry(.), extend_toward_water,
             water = water_mask, extend_dist = EXTEND_DIST_M, gap_threshold = GAP_THRESHOLD_M),
      crs = st_crs(.)
   )) %>%
   st_difference(water_mask)

nhd_main_streams <- nhd_main_streams[!st_is_empty(nhd_main_streams), ] %>%
   st_collection_extract('LINESTRING')

states_raw <- tigris::states(cb = TRUE) %>% st_transform(4326)

md_boundary <- states_raw %>% filter(STUSPS == 'MD') %>% st_boundary()
pa_boundary <- states_raw %>% filter(STUSPS == 'PA') %>% st_boundary()

state_line <- st_intersection(md_boundary, pa_boundary) %>%
   clip_to_utm(main_bbox_utm_sf, TARGET_CRS) %>%
   st_simplify(dTolerance = 150) %>%
   st_union() %>%
   st_line_merge()

# =============================================================================
# LOAD DATA -- BATHYMETRY & DEM
# =============================================================================

cat("\nLoading bathymetry raster...\n")

bathy_raw <- rast(BATHY_PATH)

bathy_crop_bbox <- st_bbox(st_buffer(st_as_sfc(main_bbox), 0.05))
bathy_main <- crop(
   bathy_raw,
   ext(bathy_crop_bbox['xmin'], bathy_crop_bbox['xmax'], bathy_crop_bbox['ymin'], bathy_crop_bbox['ymax'])
)

cat("\nQuerying DEM via elevatr...\n")
main_bbox_sf_df <- st_sf(id = 1, geometry = main_bbox_sf)
dem_main_raw <- get_elev_raster(locations = main_bbox_sf_df, z = ELEV_ZOOM, src = 'aws', clip = 'bbox')
dem_main <- rast(dem_main_raw)

# =============================================================================
# LOAD DATA -- INSET
# =============================================================================

cat("\nLoading land base layer for inset...\n")
inset_land <- tigris::states(cb = TRUE) %>%
   filter(STUSPS %in% c('MD', 'VA', 'DE', 'PA', 'NJ', 'WV', 'NC', 'DC')) %>%
   clip_to_utm(inset_bbox_utm_sf, TARGET_CRS)

inset_land_union <- st_union(inset_land)
inset_water <- st_difference(inset_bbox_utm_sf, inset_land_union) %>% st_make_valid()

nhd_inset_rivers_raw <- get_nhdplus(AOI = inset_bbox_sf, realization = 'flowline') %>%
   st_zm(drop = TRUE, what = 'ZM') %>%
   filter(gnis_name %in% INSET_RIVER_NAMES, streamorde >= INSET_RIVER_MIN_STREAMORDE)

potomac_south_box <- st_bbox(
   c(xmin = unname(inset_bbox['xmin']), xmax = unname(inset_bbox['xmax']),
     ymin = unname(inset_bbox['ymin']), ymax = POTOMAC_DC_LAT_CUTOFF),
   crs = 4326
) %>% st_as_sfc()

nhd_inset_rivers_raw <- nhd_inset_rivers_raw %>%
   mutate(.is_potomac = gnis_name == 'Potomac River') %>%
   { bind_rows(
      filter(., !.is_potomac),
      filter(., .is_potomac) %>% st_intersection(potomac_south_box)
   ) } %>%
   select(-.is_potomac)

nhd_inset_rivers <- nhd_inset_rivers_raw %>%
   clip_to_utm(inset_bbox_utm_sf, TARGET_CRS) %>%
   st_difference(inset_water)

nhd_inset_rivers <- nhd_inset_rivers[!st_is_empty(nhd_inset_rivers), ] %>%
   st_collection_extract('LINESTRING')

inset_labels <- tibble::tribble(
   ~name,              ~lon,      ~lat,
   'Baltimore',        -76.6122,  39.2904,
   'Washington, DC',   -77.0369,  38.9072,
   'Philadelphia',     -75.1652,  39.9526
) %>%
   st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
   st_transform(TARGET_CRS)

state_labels <- tibble::tribble(
   ~name, ~lon,  ~lat,
   'PA',  -77.3, 40.0,
   'MD',  -77.2, 39.4,
   'VA',  -77.8, 37.7,
   'DE',  -75.55, 38.9
) %>%
   st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
   st_transform(TARGET_CRS)

bay_label_coords <- st_coordinates(
   st_sfc(st_point(c(-75.05, 37.25)), crs = 4326) %>% st_transform(TARGET_CRS)
)
bay_target_coords <- st_coordinates(
   st_sfc(st_point(c(-76.10, 37.35)), crs = 4326) %>% st_transform(TARGET_CRS)
)

# =============================================================================
# RASTER PROCESSING
# =============================================================================

nhd_main_streams_proj <- nhd_main_streams
state_line_proj       <- state_line
inset_water_proj      <- inset_water
nhd_inset_rivers_proj <- nhd_inset_rivers
inset_land_proj       <- inset_land

project_to_template <- function(source_raster, utm_bbox, target_crs, res_m, method) {
   template <- rast(
      xmin = utm_bbox['xmin'], xmax = utm_bbox['xmax'],
      ymin = utm_bbox['ymin'], ymax = utm_bbox['ymax'],
      resolution = res_m,
      crs = paste0('EPSG:', target_crs)
   )
   project(source_raster, template, method = method)
}

bathy_main_proj <- project_to_template(bathy_main, main_bbox_utm, TARGET_CRS, BATHY_NATIVE_RES_M, 'bilinear')
bathy_main_df   <- as.data.frame(bathy_main_proj, xy = TRUE)
names(bathy_main_df)[3] <- 'depth'

dem_main_proj  <- project_to_template(dem_main, main_bbox_utm, TARGET_CRS, RES_MAIN_M, 'bilinear')
slope_main     <- terrain(dem_main_proj, v = 'slope', unit = 'radians')
aspect_main    <- terrain(dem_main_proj, v = 'aspect', unit = 'radians')
hillshade_main <- shade(slope_main, aspect_main, angle = 45, direction = 315)
hillshade_main_df <- as.data.frame(hillshade_main, xy = TRUE)
names(hillshade_main_df)[3] <- 'shade'
hillshade_main_df <- hillshade_main_df %>%
   mutate(x = round(x / RES_MAIN_M) * RES_MAIN_M,
          y = round(y / RES_MAIN_M) * RES_MAIN_M)

# =============================================================================
# LABELS
# =============================================================================

place_labels <- tibble::tribble(
   ~name,               ~lon,      ~lat,
   'Havre de Grace',    -76.0942,  39.5487,
   'Port Deposit',      -76.1140,  39.6075,
   'Conowingo Dam',     -76.1747,  39.6576
) %>%
   st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
   st_transform(TARGET_CRS)

# =============================================================================
# MAIN PANEL
# =============================================================================

cat("\nBuilding main panel...\n")

label_x <- main_bbox_utm['xmin'] + 2500
state_line_y_at_left <- st_coordinates(state_line_proj)[1, 'Y']
panel_width_m <- as.numeric(main_bbox_utm['xmax'] - main_bbox_utm['xmin'])
label_nudge_m <- panel_width_m * 0.01

p_main <- ggplot() +
   geom_tile(data = hillshade_main_df, aes(x, y, fill = shade),
             show.legend = FALSE, alpha = HILLSHADE_ALPHA) +
   scale_fill_gradient(low = 'grey20', high = 'grey90', guide = 'none') +
   ggnewscale::new_scale_fill() +
   geom_sf(data = water_mask, fill = '#c6dbef', color = NA) +
   geom_raster(data = bathy_main_df, aes(x = x, y = y, fill = depth), 
               interpolate = TRUE, alpha = 0.9) +
   scale_fill_gradient(
      low = '#08306b', high = '#c6dbef', name = 'Depth (m)',
      guide = guide_colorbar(
         direction = 'horizontal', title.position = 'top',
         barwidth = unit(7, 'cm'), barheight = unit(0.5, 'cm')
      )
   ) +
   geom_sf(data = nhd_main_streams_proj, color = STREAM_COLOR, linewidth = 0.4) +
   geom_sf(data = shoreline_lines, color = WATER_OUTLINE_COLOR, linewidth = WATER_OUTLINE_WIDTH) +
   geom_sf(data = state_line_proj, color = 'grey40', linewidth = 0.5) +
   annotate('text', x = label_x, y = state_line_y_at_left + 700,
            label = 'Pennsylvania', size = 5.5, color = 'grey30', fontface = 'italic', hjust = 0) +
   annotate('text', x = label_x, y = state_line_y_at_left - 700,
            label = 'Maryland', size = 5.5, color = 'grey30', fontface = 'italic', hjust = 0) +
   geom_sf(data = place_labels %>% filter(name == 'Port Deposit'),
           size = 1.8, color = 'black') +
   geom_sf(data = place_labels %>% filter(name == 'Havre de Grace'),
           size = 3.5, shape = 8, color = ACCENT_COLOR, stroke = 1.2) +
   geom_text_repel(
      data = place_labels %>% filter(name == 'Havre de Grace'),
      aes(label = name, geometry = geometry), stat = 'sf_coordinates',
      size = 5.5, fontface = 'bold', nudge_x = -label_nudge_m, direction = 'y', hjust = 1,
      segment.color = NA
   ) +
   geom_text_repel(
      data = place_labels %>% filter(name == 'Conowingo Dam'),
      aes(label = name, geometry = geometry), stat = 'sf_coordinates',
      size = 5.5, fontface = 'bold', nudge_x = -label_nudge_m * 1.2, nudge_y = -1200, direction = 'y', hjust = 1,
      min.segment.length = 0, segment.color = 'black', linewidth = 0.5
   ) +
   geom_text_repel(
      data = place_labels %>% filter(name == 'Port Deposit'),
      aes(label = name, geometry = geometry), stat = 'sf_coordinates',
      size = 5.5, fontface = 'bold', nudge_x = label_nudge_m, direction = 'y', hjust = 0,
      segment.color = NA
   ) +
   annotation_scale(
      location = 'bl', pad_x = unit(0.3, 'cm'), pad_y = unit(0.3, 'cm'), width_hint = 0.2
   ) +
   annotation_north_arrow(
      location = 'bl', pad_x = unit(1.2, 'cm'), pad_y = unit(3.2, 'cm'),
      which_north = 'true',
      style = north_arrow_orienteering(text_size = 11),
      height = unit(1.6, 'cm'), width = unit(1.6, 'cm')
   ) +
   coord_sf(
      crs = TARGET_CRS,
      xlim = c(main_bbox_utm['xmin'], main_bbox_utm['xmax']),
      ylim = c(main_bbox_utm['ymin'], main_bbox_utm['ymax']),
      expand = FALSE, datum = TARGET_CRS
   ) +
   theme_void() +
   theme(
      legend.position = 'inside',
      legend.position.inside = c(0.84, 0.87),
      # Translucent background card matching land color with 80% opacity
      legend.background = element_rect(fill = alpha(LAND_COLOR_MAIN, 0.80), color = NA),
      legend.margin = margin(5, 8, 5, 8),
      legend.key = element_blank(),
      legend.title = element_text(size = 11, face = 'bold'),
      legend.text = element_text(size = 9),
      panel.background = element_rect(fill = LAND_COLOR_MAIN, color = NA),
      plot.margin = margin(0, 0, 0, 0)
   )

# =============================================================================
# INSET PANEL
# =============================================================================

cat("\nBuilding inset panel...\n")

main_extent_rect <- st_as_sfc(main_bbox_utm)
INSET_LAND_COLOR <- '#c8d6b9'

p_inset <- ggplot() +
   geom_sf(data = inset_land_proj, fill = INSET_LAND_COLOR, color = 'grey50', linewidth = 0.3) +
   geom_sf(data = inset_water_proj, fill = INSET_BAY_COLOR, color = 'grey50', linewidth = 0.3) +
   geom_sf(data = nhd_inset_rivers_proj, color = 'grey50', linewidth = 0.25) +
   geom_sf(data = main_extent_rect, fill = NA, color = ACCENT_COLOR, linewidth = 0.9) +
   geom_sf(data = inset_labels, size = 1.5, color = 'black') +
   geom_text_repel(
      data = inset_labels, aes(label = name, geometry = geometry),
      stat = 'sf_coordinates', size = 3.0, fontface = 'bold',
      nudge_x = -20000, hjust = 1, direction = 'y',
      box.padding = 0.25, point.padding = 0.2, min.segment.length = 0,
      segment.color = 'grey40', linewidth = 0.3
   ) +
   geom_sf_text(data = state_labels, aes(label = name), size = 3.5,
                fontface = 'italic', color = 'grey30') +
   annotate('text', x = bay_label_coords[1, 'X'], y = bay_label_coords[1, 'Y'],
            label = 'Chesapeake\nBay', size = 3.2, fontface = 'bold.italic', color = 'grey20', hjust = 0.5) +
   annotate('segment', x = bay_label_coords[1, 'X'] - 12000, y = bay_label_coords[1, 'Y'] + 4000,
            xend = bay_target_coords[1, 'X'], yend = bay_target_coords[1, 'Y'],
            color = 'grey30', linewidth = 0.4) +
   coord_sf(
      crs = TARGET_CRS,
      xlim = c(inset_bbox_utm['xmin'], inset_bbox_utm['xmax']),
      ylim = c(inset_bbox_utm['ymin'], inset_bbox_utm['ymax']),
      expand = FALSE, datum = TARGET_CRS
   ) +
   theme_void() +
   theme(
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.6),
      plot.background = element_rect(fill = 'white', color = NA),
      plot.margin = margin(0, 0, 0, 0)
   )

# =============================================================================
# COMPOSITION + EXPORT
# =============================================================================

cat("\nComposing final figure and exporting...\n")

export_bbox_w_m <- main_bbox_utm['xmax'] - main_bbox_utm['xmin']
export_bbox_h_m <- main_bbox_utm['ymax'] - main_bbox_utm['ymin']
export_aspect   <- as.numeric(export_bbox_w_m / export_bbox_h_m)

EXPORT_HEIGHT_IN <- 10
EXPORT_WIDTH_IN  <- EXPORT_HEIGHT_IN * export_aspect

INSET_SCALE <- 0.85

inset_box_h <- (0.70 - 0.26) * 0.90 * INSET_SCALE
inset_aspect <- as.numeric(
   (inset_bbox_utm['xmax'] - inset_bbox_utm['xmin']) /
      (inset_bbox_utm['ymax'] - inset_bbox_utm['ymin'])
)
inset_box_w <- inset_box_h * inset_aspect * (EXPORT_HEIGHT_IN / EXPORT_WIDTH_IN)

inset_left <- 0.02
inset_top  <- 0.58

final_map <- p_main +
   inset_element(p_inset, left = inset_left, bottom = inset_top - inset_box_h,
                 right = inset_left + inset_box_w, top = inset_top)

ggsave(file.path(fig_dir, 'CaseStudyMap.png'), final_map,
       width = EXPORT_WIDTH_IN, height = EXPORT_HEIGHT_IN, dpi = 600, bg = 'white')
ggsave(file.path(fig_dir, 'CaseStudyMap.svg'), final_map,
       width = EXPORT_WIDTH_IN, height = EXPORT_HEIGHT_IN, dpi = 600, bg = 'white')

cat(sprintf("\nExported to %s\n", fig_dir))