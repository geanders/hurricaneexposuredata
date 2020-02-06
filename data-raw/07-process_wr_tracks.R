# Load needed libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(sf)
library(stormwindmodel) ## For now, must use the `add_cpp` branch of the devel version

data(hurr_tracks)

## Read in data from HURDAT2

# Bring in latest version of HURDAT2 (as of Feb. 4, 2020)
hurdat2 <- read_lines("https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2018-120319.txt")

# Split into the rows with storm names (e.g., "AL011851") and those with storm data
hurdat2_names <- hurdat2[str_detect(hurdat2, "AL[0-9]{6}.")]
hurdat2_data <- hurdat2[!str_detect(hurdat2, "AL[0-9]{6}.")]

# Clean storm names data and create columns where each name is repeated for each
# storm observation
clean_names <- hurdat2_names %>%
  matrix(ncol = 1) %>%
  tibble() %>%
  rename(x = ".") %>%
  separate(x, into = c("id", "name", "n_obs", "null"), sep = ",") %>%
  mutate_all(str_trim) %>%
  select(-null) %>%
  mutate(name = str_to_title(name),
         season = str_sub(id, 5, 8),
         season = as.numeric(season)) %>%
  mutate(rep_id = map2(.x = id, .y = n_obs, .f = ~ rep(.x, times = .y))) %>%
  unnest(rep_id) %>%
  select(-n_obs, -rep_id) %>%
  mutate(storm_id = case_when(
    name == c("Unnamed") ~ paste0(str_sub(id, 1, 4), "-", season),
    TRUE ~ paste0(name, "-", season)
  ))

# Clean storm data and add on columns with storm names, IDs, and seasons
clean_data <- hurdat2_data %>%
  matrix(ncol = 1) %>%
  tibble() %>%
  rename(x = ".") %>%
  separate(x,
           into = c("date", "time_utc", "record_id", "status",
                    "lat", "lon", "vmax", "pmin", "wr_34_ne",
                    "wr_34_se", "wr_34_sw", "wr_34_nw", "wr_50_ne",
                    "wr_50_se", "wr_50_sw", "wr_50_nw", "wr_64_ne",
                    "wr_64_se", "wr_64_sw", "wr_64_nw", "null"),
           sep = ",") %>%
  mutate_all(str_trim) %>%
  mutate(date = ymd(date),
         lat_dir = str_extract(lat, "[N|S]"),
         lat = str_remove(lat, "[N|S]"),
         lat = as.numeric(lat),
         lon_dir = str_extract(lon, "[E|W]"),
         lon = str_remove(lon, "[E|W]"),
         lon = as.numeric(lon),
         lon = ifelse(lon_dir == "W", -1 * lon, lon)) %>%
  mutate_at(vars(vmax:wr_64_nw), as.numeric) %>%
  mutate(vmax = ifelse(vmax == -99, NA, vmax)) %>%
  mutate_at(vars(pmin:wr_64_nw), ~ ifelse(. == -999, NA, .)) %>%
  select(-null, -record_id)

clean_hd <- bind_cols(clean_names, clean_data) %>%
  filter(year(date) >= 2004) %>%
  select(-lon_dir, -lat_dir, -season, -status, -pmin) %>%
  mutate(date = format(date, "%Y%m%d")) %>%
  unite(date_time, date, time_utc) %>%
  mutate(date_time = ymd_hm(date_time)) %>%
  group_by(id) %>%
  mutate(track_time = difftime(date_time, first(date_time), units = "hours"),
         track_time_simple = as.numeric(track_time)) %>%
  ungroup()

clean_hd <- clean_hd %>%
  filter(storm_id %in% unique(hurr_tracks$storm_id))

# Interpolation from synoptic times to every 15 minutes
# Uses natural cubic splines for lontitude and latitude and linear splines
# for central maximum wind and wind radii
hd_interp <- clean_hd %>%
  select(-name) %>%
  group_by(id, storm_id) %>%
  nest() %>%
  mutate(interp_time = purrr::map(data, ~ seq(from = first(.x$track_time_simple),
                                       to = last(.x$track_time_simple),
                                       by = 0.25))) %>%
  # Interpolate latitude and longitude using natural cubic splines
  mutate(interp_lat = map2(data, interp_time,
                           ~ interpolate_spline(x = .x$track_time_simple,
                                                y = .x$lat,
                                                new_x = .y))) %>%
  mutate(interp_lon = map2(data, interp_time,
                           ~ interpolate_spline(x = .x$track_time_simple,
                                                y = .x$lon,
                                                new_x = .y))) %>%
  # Interpolate max wind using linear interpolation
  mutate(interp_vmax = map2(data, interp_time,
                           ~ approx(x = .x$track_time_simple,
                                    y = .x$vmax,
                                    xout = .y)$y)) %>%
  # Interpolate wind radii using linear interpolation
  mutate(interp_wr_34_ne = map2(data, interp_time,
                            ~ approx(x = .x$track_time_simple,
                                     y = .x$wr_34_ne,
                                     xout = .y)$y)) %>%
  mutate(interp_wr_34_se = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_34_se,
                                         xout = .y)$y)) %>%
  mutate(interp_wr_34_sw = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_34_sw,
                                         xout = .y)$y)) %>%
  mutate(interp_wr_34_nw = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_34_nw,
                                         xout = .y)$y)) %>%
  mutate(interp_wr_50_ne = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_50_ne,
                                         xout = .y)$y)) %>%
  mutate(interp_wr_50_se = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_50_se,
                                         xout = .y)$y)) %>%
  mutate(interp_wr_50_sw = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_50_sw,
                                         xout = .y)$y)) %>%
  mutate(interp_wr_50_nw = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_50_nw,
                                         xout = .y)$y)) %>%
  mutate(interp_wr_64_ne = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_64_ne,
                                         xout = .y)$y)) %>%
  mutate(interp_wr_64_se = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                        y = .x$wr_64_se,
                                        xout = .y)$y)) %>%
  mutate(interp_wr_64_sw = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_64_sw,
                                         xout = .y)$y)) %>%
  mutate(interp_wr_64_nw = map2(data, interp_time,
                                ~ approx(x = .x$track_time_simple,
                                         y = .x$wr_64_nw,
                                         xout = .y)$y)) %>%
  select(-data) %>%
  unnest(interp_time:interp_wr_64_nw) %>%
  ungroup()

## Load county centers and limit to those in our study states
data("county_centers")
study_states <- c("Alabama", "Arkansas", "Connecticut", "Delaware",
                  "District of Columbia", "Florida", "Georgia",
                  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                  "Louisiana", "Maine", "Maryland", "Massachusetts",
                  "Michigan", "Mississippi", "Missouri",
                  "New Hampshire", "New Jersey", "New York", "North Carolina",
                  "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island",
                  "South Carolina", "Tennessee", "Texas", "Vermont",
                  "Virginia", "West Virginia", "Wisconsin")
county_centers <- county_centers %>%
  filter(state_name %in% study_states)

## Determine county winds based on interpolated storm track and
## county centers. Assumes storm track is interpolated to 15 minutes.
## This is based on a C function saved in the R package's directory
## (in the "src" subdirectory of "data-raw").

# This function uses the `find_storm_wind.c` function.
# To create the shared library, `R CMD SHLIB find_storm_wind.c``

dyn.load("data-raw/src/find_storm_wind.so")
find_storm_wind <- function(storm_track, county_centers){

  slon <- storm_track$interp_lon
  slat <- storm_track$interp_lat
  r_34_ne <- storm_track$interp_wr_34_ne
  r_50_ne <- storm_track$interp_wr_50_ne
  r_64_ne <- storm_track$interp_wr_64_ne
  r_34_se <- storm_track$interp_wr_34_se
  r_50_se <- storm_track$interp_wr_50_se
  r_64_se <- storm_track$interp_wr_64_se
  r_34_sw <- storm_track$interp_wr_34_sw
  r_50_sw <- storm_track$interp_wr_50_sw
  r_64_sw <- storm_track$interp_wr_64_sw
  r_34_nw <- storm_track$interp_wr_34_nw
  r_50_nw <- storm_track$interp_wr_50_nw
  r_64_nw <- storm_track$interp_wr_64_nw
  clon <- county_centers$longitude
  clat <- county_centers$latitude
  fips <- county_centers$fips
  storm_id <- storm_track$storm_id[1]

  storm_obs <- length(slon)
  counties <- length(clon)

  result <- .C("find_storm_wind",
               as.integer(storm_obs), as.double(slon), as.double(slat),
               as.double(r_34_ne), as.double(r_50_ne), as.double(r_64_ne),
               as.double(r_34_se), as.double(r_50_se), as.double(r_64_se),
               as.double(r_34_sw), as.double(r_50_sw), as.double(r_64_sw),
               as.double(r_34_nw), as.double(r_50_nw), as.double(r_64_nw),
               as.integer(counties), as.double(clon), as.double(clat),
               dist = as.double(rep(0.0, storm_obs * counties)),
               quadrant = as.integer(rep(0L, storm_obs * counties)),
               v_sust = as.double(rep(0.0, storm_obs * counties)),
               peak_wind = as.double(rep(0.0, counties)),
               wind_duration = as.double(rep(0.0, counties)))

  ## If needed for checks, you can also pull out from these results the
  ## distance between the storm and county at each point (result[["dist"]]),
  ## the quadrant of the storm each county is in at each point (result[["quadrant"]]),
  ## and the sustained wind speed in the county (based on its distance from and
  ## quadrant of the storm) at each time point (result[["v_sust"]]).

  out <- tibble(fips = fips,
                vmax_gust = result[["peak_wind"]] * 0.5144 * 1.49, ## Get the gust based on gust factor
                vmax_sust = result[["peak_wind"]] * 0.5144, ## Convert from kts to m/s
                sust_dur = result[["wind_duration"]],
                storm_id = storm_id)
}


## Use this function to determine wind radii-based winds for all storms
ext_tracks_wind <- hd_interp %>%
  split(f = .$storm_id) %>%
  purrr::map_df(~ find_storm_wind(.x, county_centers)) %>%
  bind_rows()

usethis::use_data(ext_tracks_wind, overwrite = TRUE)
