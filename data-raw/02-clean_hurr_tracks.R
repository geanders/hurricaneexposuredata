library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# IBTrACS data
# First two rows given column names and units (if relevant)
hurr_track_units <- read_csv(paste0("https://www.ncei.noaa.gov/data/",
                                    "international-best-track-archive-for-climate-stewardship-ibtracs/",
                                    "v04r00/access/csv/ibtracs.NA.list.v04r00.csv"),
                             n_max = 1, col_names = TRUE)

# Third row and later give values. Use the same column names from above
hurr_tracks <- read_csv(paste0("https://www.ncei.noaa.gov/data/",
                               "international-best-track-archive-for-climate-stewardship-ibtracs/",
                               "v04r00/access/csv/ibtracs.NA.list.v04r00.csv"),
                        skip = 2, col_names = colnames(hurr_track_units)) %>%
        filter(SEASON >= 1988 & SEASON < 2019) %>%
        rename_all(str_to_lower) %>%
        select(usa_atcf_id, season, name, iso_time,
               lat, lon, usa_wind) %>%
        mutate(usa_atcf_id = str_sub(usa_atcf_id, 1, 4), # Take the year out of the ATCF ID
               name = str_to_title(name),
               name = ifelse(name == "Not_named", usa_atcf_id, name)) %>% # For Unnamed storms, used the ATCF ID without the year
        unite(storm_id, c("name", "season"), sep = "-") %>% # Use name plus season as the storm ID
        mutate(date = format(iso_time, "%Y%m%d%H%M")) %>%
        select(storm_id, date, lat, lon, usa_wind) %>%
        rename(latitude = lat,
               longitude = lon,
               wind = usa_wind) %>%
        as.data.frame()

usethis::use_data(hurr_tracks, overwrite = TRUE)
