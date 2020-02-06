library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

data("hurr_tracks")
data("county_centers")

hd_interp <- hurr_tracks %>%
  mutate(date_time = ymd_hm(date)) %>%
  filter(year(date_time) >= 1988) %>%
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

## Function to map a single storm track
map_interp_track <- function(this_storm_id = "Katrina-2005"){
  storm_track_sf <- hd_interp %>%
    filter(storm_id == this_storm_id)  %>%
    st_as_sf(coords = c("interp_lon", "interp_lat")) %>%
    st_set_crs(4269)
  synoptic_track_sf <- storm_track_sf %>%
    filter(interp_time %% 6 == 0)

  ggplot() +
    geom_sf(data = world) +
    geom_sf(data = us_states, color = NA) +
    geom_sf(data = us_states, fill = NA, size = 0.1) +
    geom_sf(data = storm_track_sf, color = "red", size = 0.01, alpha = 0.2) +
    geom_sf(data = synoptic_track_sf, color = "darkred", alpha = 0.5) +
    coord_sf(xlim = c(-106, -60), ylim = c(20, 47.5)) +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.1),
          panel.background = element_rect(fill = "aliceblue"),
          legend.position = "bottom") +
    ggtitle(paste(this_storm_id))
}

# Apply to all storms
storms <- unique(hd_interp$storm_id)
storm_track_maps <- purrr::map(storms, map_interp_track)

pdf("data-raw/eda/storm_track_check.pdf")
storm_track_maps
dev.off()
