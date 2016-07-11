library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(geosphere)
library(choroplethr)
library(gridExtra)

# Functions to use to calculate winds

calc_wind_cat <- function(quadrant, distance, winds){
  quad_radii <- winds %>%
    filter(quad == quadrant)
  wind_cat <- cut(distance * 0.539957, # Convert to nautical miles
                  breaks = unique(c(0, rev(quad_radii$wind_radius), 100000)))
  wind_cat <- factor(wind_cat, levels = levels(wind_cat),
                     labels = rev(c("0 to 33 kts",
                                    "34 to 49 kts",
                                    "59 to 63 kts",
                                    "64 or more kts")[1:length(levels(wind_cat))]))
  return(wind_cat)
}

calc_county_wind <- function(storm_point, county_centers){

  winds <- storm_point %>%
    dplyr::select(contains("radius")) %>%
    tidyr::gather(value = "wind_radius") %>%
    dplyr::mutate_(key = ~ gsub("radius_", "", key)) %>%
    tidyr::separate(key, c("wind_speed", "quad"), sep = "_")

  dist_to_storm <- geosphere::distHaversine(p1 = as.matrix(county_centers[ ,
                                                                           c("longitude", "latitude")]),
                                            p2 = as.numeric(c(storm_point$longitude, storm_point$latitude)))
  dist_to_storm <- dist_to_storm / 1000 # convert to kilometers

  bear_from_storm <- geosphere::bearing(storm_point[ , c("longitude", "latitude")],
                                        county_centers[, c("longitude", "latitude")])

  storm_quad <- cut(bear_from_storm, breaks = c(-180, -90, 0, 90, 180),
                    labels = c("sw", "nw", "ne", "se"))

  county_df <- cbind(county_centers, dist_to_storm,
                     bear_from_storm, storm_quad) %>%
    mutate(wind_cat = mapply(FUN = calc_wind_cat,
                             quadrant = storm_quad,
                             distance = dist_to_storm,
                             winds = list(winds)))
  return(county_df)
}

# Mapping functions

gen_radius_points <- function(wind_speed, storm_point){
  wind_radii <- storm_point %>%
    select(contains("radius")) %>%
    gather(value = "wind_radius") %>%
    mutate(key = gsub("radius_", "", key)) %>%
    separate(key, c("wind_speed", "quad"), sep = "_") %>%
    mutate(wind_speed = paste0("wind_", wind_speed)) %>%
    spread(key = wind_speed, value = wind_radius)
  get_wind <- wind_radii[ , c("quad", paste0("wind_", wind_speed))]
  wind_points_radii <- rep(c(get_wind[get_wind$quad == "sw", 2],
                             get_wind[get_wind$quad == "nw", 2],
                             get_wind[get_wind$quad == "ne", 2],
                             get_wind[get_wind$quad == "se", 2]),
                           each = 90)
  wind_points_angle <- -180:179
  radius_points <- destPoint(p = storm_point[ , c("longitude", "latitude")],
                             b = wind_points_angle,
                             d = 1852 * wind_points_radii) %>%
    data.frame() %>% `colnames<-`(c("longitude", "latitude"))
  return(radius_points)
}

add_wind_radii <- function(base_map, storm_point,
                           fill_radii = TRUE, size = 0.5){
  radius_34 <- gen_radius_points(34, storm_point)
  radius_50 <- gen_radius_points(50, storm_point)
  radius_64 <- gen_radius_points(64, storm_point)

  out <- base_map +
    geom_polygon(data = radius_34, aes(x = longitude, y = latitude, group = NA),
                 color = "yellow",  alpha = 0.25, size = size,
                 fill = ifelse(fill_radii, "yellow", NA)) +
    geom_polygon(data = radius_50, aes(x = longitude, y = latitude, group = NA),
                 color = "orange", alpha = 0.25, size = size,
                 fill = ifelse(fill_radii, "orange", NA)) +
    geom_polygon(data = radius_64, aes(x = longitude, y = latitude, group = NA),
                 color = "red", alpha = 0.25, size = size,
                 fill = ifelse(fill_radii, "red", NA)) +
    geom_point(data = storm_point, aes(x = longitude, y = latitude, group = NA),
               size = 2, color = "black")
  return(out)
}

plot_county_wind_obs <- function(storm_point, county_centers, state_zoom){
  storm_wind <- calc_county_wind(storm_point = storm_point,
                                 county_centers = filter(county_centers,
                                                         tolower(state_name) %in%
                                                           tolower(state_zoom)))
  to_plot <- storm_wind %>%
    mutate(region = as.numeric(fips), value = wind_cat) %>%
    select(region, value)
  a <- CountyChoropleth$new(to_plot)
  a$set_zoom(tolower(state_zoom))
  a$ggplot_scale <- scale_fill_manual(name = "Maximum sustained wind speed",
                                      values = c("64 or more kts" = "red",
                                                 "59 to 63 kts" = "orange",
                                                 "34 to 49 kts" = "yellow",
                                                 "0 to 33 kts" = "white"))
  a <- add_wind_radii(a$render(), storm_point, fill_radii = FALSE, size = 1)
  a + geom_point(data = storm_wind,
                 aes(x = longitude, y = latitude, group = NA),
                 size = 0.1, color = "gray")
  return(a)
}

# Code to read in data and calculate winds

# Read in extended hurricane tracks
hurr_tracks <- read.fwf("http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/data/ebtrk_atlc_1988_2015.txt",
                        widths = c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5,
                                   4, 4, 5, 3, 4, 3, 3, 3,
                                   4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1),
                        na.strings = c("-99"))
colnames(hurr_tracks) <- c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final")
format_longitude <- function(lon){
  lon <- as.character(lon)
  names(lon) <- NULL
  lon <- sapply(lon, function (x)  sub("^\\s+", "", x))
  out <- strsplit(lon, " ")
  out <- sapply(out, function(x) x[[1]])
  out <- -1 * as.numeric(out)
  return(out)
}
hurr_tracks <- select(hurr_tracks, -storm_id) %>%
  dplyr::mutate(storm_name = as.character(storm_name),
         storm_name = paste0(substr(storm_name, 1, 1),
                             substr(tolower(storm_name), 2,
                                    nchar(storm_name))),
         storm_id = paste(gsub(" ", "", storm_name), year, sep = "-"),
         longitude = format_longitude(longitude),
         month = sprintf("%02d", month),
         day = sprintf("%02d", day),
         hour = sprintf("%02d", hour),
         minute = "00",
         radius_34_ne = ifelse(radius_34_ne == -99, NA, radius_34_ne),
         radius_50_ne = ifelse(radius_50_ne == -99, NA, radius_50_ne),
         radius_64_ne = ifelse(radius_64_ne == -99, NA, radius_64_ne)) %>%
  select(storm_id, year, month, day, hour, minute, latitude, longitude,
         starts_with("radius"))
hurr_tracks <- tidyr::unite_(hurr_tracks, "date",
                             c("year", "month", "day", "hour", "minute"),
                             sep = "")

data(county_centers)


## Code for checking and mapping

# # Code to check out missing values for wind radii. How often are they relevant?
# # (occur when it might affect maximum wind estimates for US counties)
# pdf("~/tmp/check_missing_wind_radii.pdf")
# map_data <- get_map(location = "Bahamas", zoom = 4)
# for(storm in unique(hurr_tracks$storm_id)){
#   print(storm)
#   to_plot <- filter(hurr_tracks, storm_id == storm) %>%
#     select(date, latitude, longitude, starts_with("radius")) %>%
#     gather(metric, radius, -date, -latitude, -longitude) %>%
#     group_by(date) %>%
#     dplyr::summarize(n_missing = 12 - sum(!is.na(radius)),
#               latitude = first(latitude),
#               longitude = first(longitude))
#
#   print(ggmap(map_data) +
#     geom_point(data = to_plot,
#                aes(x = longitude, y = latitude, color = n_missing)) +
#     geom_path(data = to_plot,
#               aes(x = longitude, y = latitude, color = n_missing)) +
#     ggtitle(storm))
# }
# dev.off()
# # Checks for potential problems:
# # Earlier-- lots of cases with some missing values that could affect
# # assessment, including at or near landfall (e.g., Andrew-1992, Floyd-1999)
# # More recent years-- almost nothing missing (e.g., Katrina-2005, Sandy-2012)

# Check calculation of wind categories for a single storm point

# katrina <- filter(hurr_tracks, storm_id == "Katrina-2005" &
#                     date == "200508291800")
katrina <- filter(hurr_tracks, storm_id == "Sandy-2012" &
                    date == "201210291800")
storm_point <- katrina

katrina_wind <- calc_county_wind(storm_point = katrina,
                                 county_centers = filter(county_centers,
                                              state_name %in%
                                                c("Louisiana",
                                                  "Arkansas",
                                                  "Alabama",
                                                  "Tennessee",
                                                  "Texas",
                                                  "Mississippi",
                                                  "Oklahoma",
                                                  "Florida")))

map_data <- get_map("Louisiana", zoom = 6, maptype = "toner")
ggmap(map_data, extent = "device") +
  geom_point(data = katrina, aes(x = longitude, y = latitude),
             color = "red", shape = 9, size = 3) +
  geom_point(data = katrina_wind, aes(x = longitude, y = latitude,
                                      color = dist_to_storm), size = 0.5) +
  scale_colour_gradientn(colours=rainbow(4))

ggmap(map_data, extent = "device") +
  geom_point(data = katrina, aes(x = longitude, y = latitude),
             color = "red", shape = 9, size = 3) +
  geom_point(data = katrina_wind, aes(x = longitude, y = latitude,
                                      color = bear_from_storm), size = 0.5) +
  scale_colour_gradientn(colours=rainbow(4))

ggmap(map_data, extent = "device") +
  geom_point(data = katrina, aes(x = longitude, y = latitude),
             color = "red", shape = 9, size = 3) +
  geom_point(data = katrina_wind, aes(x = longitude, y = latitude,
                                      color = storm_quad), size = 0.5)

ggmap(map_data, extent = "device") +
  geom_point(data = katrina, aes(x = longitude, y = latitude),
             color = "red", shape = 9, size = 3) +
  geom_point(data = katrina_wind, aes(x = longitude, y = latitude,
                                      color = wind_cat), size = 0.5) +
  scale_color_manual(values = c("red", "orange", "yellow", "lightgray"))

map_data <- get_map("Louisiana", zoom = 6, maptype = "toner")
base_map <- ggmap(map_data, extent = "device")
add_wind_radii(base_map, storm_point)

a <- add_wind_radii(base_map, storm_point = filter(hurr_tracks,
                                                   storm_id == "Katrina-2005" &
                                                     date == "200508290600"))
b <- add_wind_radii(base_map, storm_point = filter(hurr_tracks,
                                                   storm_id == "Katrina-2005" &
                                                     date == "200508291200"))
c <- add_wind_radii(base_map, storm_point = filter(hurr_tracks,
                                                   storm_id == "Katrina-2005" &
                                                     date == "200508291800"))
d <- add_wind_radii(base_map, storm_point = filter(hurr_tracks,
                                                   storm_id == "Katrina-2005" &
                                                     date == "200508300000"))
grid.arrange(a, b, c, d, nrow = 1)

plot_county_wind_obs(storm_point, county_centers,
                    state_zoom = c("Louisiana", "Mississippi", "Alabama"))

a <- plot_county_wind_obs(storm_point = filter(hurr_tracks, storm_id == "Katrina-2005" &
                                                 date == "200508290000"),
                          county_centers,
                          state_zoom = c("Louisiana", "Mississippi", "Alabama",
                                         "Tennessee", "Georgia", "Arkansas")) +
  theme(legend.position="none") +
  ggtitle("00:00 UTC August 29, 2005") + xlim(-95, -80) + ylim(23, 38)
b <- plot_county_wind_obs(storm_point = filter(hurr_tracks, storm_id == "Katrina-2005" &
                                                 date == "200508290600"),
                          county_centers,
                          state_zoom = c("Louisiana", "Mississippi", "Alabama",
                                         "Tennessee", "Georgia", "Arkansas")) +
  theme(legend.position="none") +
  ggtitle("06:00 UTC August 29, 2005") + xlim(-95, -80) + ylim(23, 38)
c <- plot_county_wind_obs(storm_point = filter(hurr_tracks, storm_id == "Katrina-2005" &
                                                 date == "200508291200"),
                          county_centers,
                          state_zoom = c("Louisiana", "Mississippi", "Alabama",
                                         "Tennessee", "Georgia", "Arkansas")) +
  theme(legend.position="none") +
  ggtitle("12:00 UTC August 29, 2005") + xlim(-95, -80) + ylim(23, 38)
d <- plot_county_wind_obs(storm_point = filter(hurr_tracks, storm_id == "Katrina-2005" &
                                                 date == "200508291800"),
                          county_centers,
                          state_zoom = c("Louisiana", "Mississippi", "Alabama",
                                         "Tennessee", "Georgia", "Arkansas")) +
  theme(legend.position="none") +
  ggtitle("18:00 UTC August 29, 2005") + xlim(-95, -80) + ylim(23, 38)
e <- plot_county_wind_obs(storm_point = filter(hurr_tracks, storm_id == "Katrina-2005" &
                                                 date == "200508300000"),
                          county_centers,
                          state_zoom = c("Louisiana", "Mississippi", "Alabama",
                                         "Tennessee", "Georgia", "Arkansas")) +
  theme(legend.position="none") +
  ggtitle("00:00 UTC August 30, 2005") + xlim(-95, -80) + ylim(23, 38)
f <- plot_county_wind_obs(storm_point = filter(hurr_tracks, storm_id == "Katrina-2005" &
                                                 date == "200508300600"),
                          county_centers,
                          state_zoom = c("Louisiana", "Mississippi", "Alabama",
                                         "Tennessee", "Georgia", "Arkansas")) +
  theme(legend.position="none") +
  ggtitle("06:00 UTC August 30, 2005") + xlim(-95, -80) + ylim(23, 38)
grid.arrange(a, b, c, d, e, f,  ncol = 3)

ex <- calc_county_wind(storm_point, county_centers)

pull_wind_cat <- function(storm_point, county_centers){
  wind_cats <- calc_county_wind(storm_point, county_centers) %>%
    mutate(wind_cat = factor(wind_cat,
                              levels = c("0 to 33 kts", "34 to 49 kts",
                                         "59 to 63 kts", "64 or more kts"),
                              labels = c(0:3))) %>%
    mutate(wind_cat = as.numeric(as.character(wind_cat))) %>%
    select(wind_cat) %>%
    as.vector()
  return(wind_cats)
}

find_max_county_wind <- function(storm, county_centers){
  storm_tracks <- hurr_tracks %>%
    filter(storm_id == storm)
  all_wind_cats <- lapply(split(storm_tracks,
                                1:nrow(storm_tracks)),
                          FUN = pull_wind_cat,
                          county_centers = county_centers)
  all_wind_cats <- do.call("cbind", all_wind_cats)
  max_county_wind <- data.frame(fips = county_centers$fips,
                                 wind_cat = apply(all_wind_cats, 1, max)) %>%
    mutate(wind_cat = factor(wind_cat, levels = 0:3,
                             labels = c("0 to 33 kts", "34 to 49 kts",
                                        "59 to 63 kts", "64 or more kts")))
  return(max_county_wind)
}

max_county_wind <- find_max_county_wind("Katrina-2005", county_centers)
to_plot <- max_county_wind %>%
  mutate(region = as.numeric(as.character(fips)), value = wind_cat) %>%
  select(region, value)
a <- CountyChoropleth$new(to_plot)
a$set_zoom(c("louisiana", "mississippi", "alabama", "georgia",
             "arkansas", "tennessee"))
a$ggplot_scale <- scale_fill_manual(name = "Maximum sustained wind speed",
                                   values = c("64 or more kts" = "red",
                                              "59 to 63 kts" = "orange",
                                              "34 to 49 kts" = "yellow",
                                              "0 to 33 kts" = "white"))
a$render()

storm_tracks <- filter(hurr_tracks, storm_id == "Katrina-2005")
eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                    "district of columbia", "florida", "georgia", "illinois",
                    "indiana", "iowa", "kansas", "kentucky", "louisiana",
                    "maine", "maryland", "massachusetts", "michigan",
                    "mississippi", "missouri", "new hampshire", "new jersey",
                    "new york", "north carolina", "ohio", "oklahoma",
                    "pennsylvania", "rhode island", "south carolina",
                    "tennessee", "texas", "vermont", "virginia",
                    "west virginia", "wisconsin")
a <- CountyChoropleth$new(to_plot)
a$set_zoom(eastern_states)
a$ggplot_scale <- scale_fill_manual(name = "Maximum sustained wind speed",
                                    values = c("64 or more kts" = "red",
                                               "59 to 63 kts" = "orange",
                                               "34 to 49 kts" = "yellow",
                                               "0 to 33 kts" = "white"))
a$render() + geom_path(data = storm_tracks,
                       aes(x = longitude, y = latitude, group = NA),
                       color = "navy")

max_county_wind <- find_max_county_wind("Sandy-2012", county_centers)
to_plot <- max_county_wind %>%
  mutate(region = as.numeric(as.character(fips)), value = wind_cat) %>%
  select(region, value)
a <- CountyChoropleth$new(to_plot)
a$set_zoom(eastern_states)
a$ggplot_scale <- scale_fill_manual(name = "Maximum sustained wind speed",
                                    values = c("64 or more kts" = "red",
                                               "59 to 63 kts" = "orange",
                                               "34 to 49 kts" = "yellow",
                                               "0 to 33 kts" = "white"))
storm_tracks <- hurr_tracks %>%
  dplyr::filter(storm_id == "Sandy-2012")
a$render() + geom_path(data = storm_tracks,
                       aes(x = longitude, y = latitude, group = NA),
                       color = "navy")

max_county_wind <- find_max_county_wind("Ike-2008", county_centers)
to_plot <- max_county_wind %>%
  mutate(region = as.numeric(as.character(fips)), value = wind_cat) %>%
  select(region, value)
a <- CountyChoropleth$new(to_plot)
a$set_zoom(eastern_states)
a$ggplot_scale <- scale_fill_manual(name = "Maximum sustained wind speed",
                                    values = c("64 or more kts" = "red",
                                               "59 to 63 kts" = "orange",
                                               "34 to 49 kts" = "yellow",
                                               "0 to 33 kts" = "white"))
storm_tracks <- hurr_tracks %>%
  dplyr::filter(storm_id == "Ike-2008")
a$render() +
  geom_path(data = storm_tracks,
                       aes(x = longitude, y = latitude, group = NA),
                       color = "navy") +
  geom_point(data = storm_tracks,
             aes(x = longitude, y = latitude, group = NA))

max_county_wind <- find_max_county_wind("Ivan-2004", county_centers)
to_plot <- max_county_wind %>%
  mutate(region = as.numeric(as.character(fips)), value = wind_cat) %>%
  select(region, value)
a <- CountyChoropleth$new(to_plot)
a$set_zoom(eastern_states)
a$ggplot_scale <- scale_fill_manual(name = "Maximum sustained wind speed",
                                    values = c("64 or more kts" = "red",
                                               "59 to 63 kts" = "orange",
                                               "34 to 49 kts" = "yellow",
                                               "0 to 33 kts" = "white"))
storm_tracks <- hurr_tracks %>%
  filter(storm_id == "Ivan-2004")
a$render() +
  geom_path(data = storm_tracks,
            aes(x = longitude, y = latitude, group = NA),
            color = "navy") +
  geom_point(data = storm_tracks,
             aes(x = longitude, y = latitude, group = NA))

