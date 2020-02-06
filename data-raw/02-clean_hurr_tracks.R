        library(readr)
        library(dplyr)
        library(stringr)
        library(tidyr)

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
                )) %>%
                select(storm_id, id) %>%
                rename(usa_atcf_id = id)

        # Clean storm data and add on columns with storm names, IDs, and seasons
        clean_data <- hurdat2_data %>%
                matrix(ncol = 1) %>%
                tibble() %>%
                rename(x = ".") %>%
                separate(x,
                         into = c("date", "time_utc", "record_id", "status",
                                  "lat", "lon", "wind", "pmin", "wr_34_ne",
                                  "wr_34_se", "wr_34_sw", "wr_34_nw", "wr_50_ne",
                                  "wr_50_se", "wr_50_sw", "wr_50_nw", "wr_64_ne",
                                  "wr_64_se", "wr_64_sw", "wr_64_nw", "null"),
                         sep = ",") %>%
                mutate_all(str_trim) %>%
                mutate(lat_dir = str_extract(lat, "[N|S]"),
                       lat = str_remove(lat, "[N|S]"),
                       lat = as.numeric(lat),
                       lon_dir = str_extract(lon, "[E|W]"),
                       lon = str_remove(lon, "[E|W]"),
                       lon = as.numeric(lon),
                       lon = ifelse(lon_dir == "W", -1 * lon, lon)) %>%
                mutate(wind = as.numeric(wind)) %>%
                mutate(wind = ifelse(wind == -99, NA, wind)) %>%
                select(date:time_utc, lat:wind)

        hurr_tracks <- bind_cols(clean_names, clean_data) %>%
                filter(as.numeric(str_sub(date, 1, 4)) >= 1988) %>%
                unite(date, date, time_utc, sep = "") %>%
                rename(longitude = lon,
                       latitude = lat)

        usethis::use_data(hurr_tracks, overwrite = TRUE)
