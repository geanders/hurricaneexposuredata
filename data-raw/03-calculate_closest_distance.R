## Be sure to re-build package after re-running 01 and 02 and before
## re-running this

library(sp)
library(dplyr)
library(lubridate)
library(hurricaneexposure)

data(county_centers, package = "hurricaneexposuredata")
data(hurr_tracks, package = "hurricaneexposuredata")

library(stormwindmodel)

## Interpolate storm tracks to every 15 minutes
all_tracks <- hurr_tracks %>%
  mutate(date_time = ymd_hm(date)) %>%
  group_by(storm_id, usa_atcf_id) %>%
  mutate(start_time = first(date_time)) %>%
  mutate(track_time = difftime(date_time, first(date_time), units = "hours"),
         track_time_simple = as.numeric(track_time)) %>%
  ungroup() %>%
  group_by(storm_id, usa_atcf_id, start_time) %>%
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
  ungroup() %>%
  mutate(date = start_time + minutes(60 * interp_time)) %>%
  select(storm_id:usa_atcf_id, date, interp_lat:interp_lon) %>%
  rename(tclon = interp_lon,
         tclat = interp_lat)


calc_closest_dist <- function(this_storm = "Katrina-2005"){
        print(this_storm)
        storm_tracks <- subset(all_tracks, storm_id == this_storm)
        this_id <- storm_tracks$usa_atcf_id[1]

        # Calculate distance from county center to storm path
        storm_county_distances <- spDists(
                as.matrix(county_centers[,c("longitude", "latitude")]),
                as.matrix(storm_tracks[,c("tclon", "tclat")]),
                longlat = TRUE) # Return distance in kilometers

        min_locs <- apply(storm_county_distances, 1, which.min)
        min_dists <- apply(storm_county_distances, 1, min)

        study_states <- c("Alabama", "Arkansas", "Connecticut", "Delaware",
                          "District of Columbia", "Florida", "Georgia",
                          "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                          "Louisiana", "Maine", "Maryland", "Massachusetts",
                          "Michigan", "Mississippi", "Missouri",
                          "New Hampshire", "New Jersey", "New York", "North Carolina",
                          "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island",
                          "South Carolina", "Tennessee", "Texas", "Vermont",
                          "Virginia", "West Virginia", "Wisconsin")

        closest_dist <- mutate(county_centers,
                               closest_date = storm_tracks$date[min_locs],
                               storm_lat = storm_tracks$tclat[min_locs],
                               storm_long = storm_tracks$tclon[min_locs],
                               storm_dist = min_dists) %>%
                filter(state_name %in% study_states) %>%
                mutate(closest_date = format(closest_date, "%Y%m%d%H%M"),
                       storm_id = this_storm, usa_atcf_id = this_id) %>%
                select(storm_id, usa_atcf_id, fips, closest_date, storm_dist)

        return(closest_dist)
}

# Apply to all hurricane tracks
hurrs <- as.character(unique(hurr_tracks$storm_id))

closest_dist <- lapply(hurrs, calc_closest_dist)
closest_dist <- do.call("rbind", closest_dist)

library(countytimezones)
closest_dist <- add_local_time(df = closest_dist,
                     datetime_colname = "closest_date",
                     fips = closest_dist$fips,
                     include_tz = FALSE) %>%
        dplyr::rename(closest_time_utc = closest_date,
               closest_date = local_date) %>%
        mutate(closest_time_utc = ymd_hm(closest_time_utc)) %>%
        mutate(closest_time_utc = format(closest_time_utc,
                                         "%Y-%m-%d %H:%M"))

# Limit hurricane tracks to only storms within 250 km of at least one county
us_storms <- closest_dist %>%
  dplyr::group_by(storm_id) %>%
  dplyr::summarize(closest_county = min(storm_dist)) %>%
  dplyr::filter(closest_county <= 250)
excluded_tracks <- hurr_tracks %>%
  dplyr::filter(!(storm_id %in% us_storms$storm_id))
hurr_tracks <- hurr_tracks %>%
  dplyr::filter(storm_id %in% us_storms$storm_id)

usethis::use_data(excluded_tracks, overwrite = TRUE)
usethis::use_data(hurr_tracks, overwrite = TRUE)

closest_dist <- closest_dist %>%
  dplyr::filter(storm_id %in% us_storms$storm_id)

usethis::use_data(closest_dist, overwrite = TRUE)
