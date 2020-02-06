library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(gridExtra)
library(lubridate)

data("storm_winds")
data("ext_tracks_wind_new")
data("ext_tracks_wind")
data("hurr_tracks")
data("county_centers")

## Interpolate storm tracks to every 15 minutes
track_interp <- hurr_tracks %>%
  mutate(date_time = ymd_hm(date)) %>%
  filter(year(date_time) >= 2004) %>% # Just check years with wind radii data
  group_by(storm_id) %>%
  mutate(track_time = difftime(date_time, first(date_time), units = "hours"),
         track_time_simple = as.numeric(track_time)) %>%
  ungroup() %>%
  select(storm_id, latitude, longitude, track_time_simple) %>%
  group_by(storm_id) %>%
  nest() %>%
  mutate(interp_time = purrr::map(data, ~ seq(from = first(.x$track_time_simple),
                                              to = last(.x$track_time_simple),
                                              by = 0.25))) %>%
  # Interpolate latitude and longitude using natural cubic splines
  mutate(interp_lat = map2(data, interp_time,
                           ~ interpolate_spline(x = .x$track_time_simple,
                                                y = .x$latitude,
                                                new_x = .y))) %>%
  mutate(interp_lon = map2(data, interp_time,
                           ~ interpolate_spline(x = .x$track_time_simple,
                                                y = .x$longitude,
                                                new_x = .y))) %>%
  select(-data) %>%
  unnest(interp_time:interp_lon) %>%
  ungroup()

## Set up some background maps
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = 4269)

us_counties <- counties(cb = TRUE, resolution = "20m", class = "sf") %>%
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
  filter(fips %in% county_centers$fips)
us_states <- states(cb = TRUE, resolution = "20m", class = "sf")



## Function to map a single storm's winds and track
map_wr_storm_winds <- function(this_storm_id = "Katrina-2005",
                               radii_based = TRUE, new = TRUE){
  if(radii_based & new){
    wind_df <- ext_tracks_wind_new
  } else if(radii_based & !new) {
    wind_df <- ext_tracks_wind
  } else {
    wind_df <- storm_winds %>%
      # Reset high winds down to 33 m/s to allow a good scale for comparison
      # with values from wind radii
      mutate(vmax_sust = ifelse(vmax_sust > 33, 33, vmax_sust))
  }

  this_storm_winds <- wind_df %>%
    filter(storm_id == this_storm_id)
  this_storm_sf <- katrina_sf <- us_counties %>%
    left_join(this_storm_winds, by = "fips")
  storm_track_sf <- track_interp %>%
    filter(storm_id == this_storm_id)  %>%
    st_as_sf(coords = c("interp_lon", "interp_lat")) %>%
    st_set_crs(4269)
  synoptic_track_sf <- storm_track_sf %>%
    filter(interp_time %% 6 == 0)

  if(radii_based & new){
    subtitle_text <- "Wind radii-based, R version"
  } else if(radii_based & !new) {
    subtitle_text <- "Wind radii-based, FORTRAN version"
  } else {
    subtitle_text <- "Modeled"
  }

  ggplot() +
    geom_sf(data = world) +
    geom_sf(data = us_states, color = NA) +
    geom_sf(data = this_storm_sf,
            aes(fill = vmax_sust,
                color = vmax_sust)) +
    geom_sf(data = us_states, fill = NA, size = 0.1) +
    geom_sf(data = storm_track_sf, color = "red", size = 0.01, alpha = 0.2) +
    geom_sf(data = synoptic_track_sf, color = "darkred", alpha = 0.5) +
    scale_color_gradientn(colours = viridis_pal()(10),
                          limits=c(0, 33),
                          name = "Peak sust. wind (m/s)") +
    scale_fill_gradientn(colours = viridis_pal()(10),
                          limits=c(0, 33),
                          name = "Peak sust. wind (m/s)") +
    coord_sf(xlim = c(-106, -60), ylim = c(20, 47.5)) +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1),
          panel.background = element_rect(fill = "aliceblue"),
          legend.position = "bottom") +
    ggtitle(subtitle_text)
}

map_old_new_and_modeled <- function(this_storm_id = "Katrina-2005"){
  a <- map_wr_storm_winds(this_storm_id)
  b <- map_wr_storm_winds(this_storm_id, new = FALSE)
  c <- map_wr_storm_winds(this_storm_id, radii_based = FALSE)

  d <- grid.arrange(a, b, c, nrow = 1,
                    top = this_storm_id,
                    bottom = "Modeled winds over 33 m/s were reset to 33 m/s for comparison")

  return(d)
}

# Apply to all storms
storms <- unique(track_interp$storm_id)
storm_wind_maps <- purrr::map(storms, map_old_new_and_modeled)

ggsave("data-raw/eda/storm_wr_winds_check_with_fortran.pdf",
       marrangeGrob(grobs = storm_wind_maps, nrow = 1, ncol = 1),
       width = 16, height = 6, units = "in")

