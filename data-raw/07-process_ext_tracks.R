library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# Make a table matching storm ID from extended tracks (e.g., "AL0188") to
# our storm IDs (e.g., "Alberto-1988")
# Bring in latest version of extended hurricane tracks
hurr_id_table <- read.fwf("http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/data/ebtrk_atlc_1988_2017.txt",
                        widths = c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5,
                                   4, 4, 5, 3, 4, 3, 3, 3,
                                   4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1),
                        na.strings = "-99",
                        stringsAsFactors = FALSE) %>%
  select(V1, V2, V6) %>%
  rename(old_id = V1,
         storm_name = V2,
         year = V6) %>%
  mutate(storm_name = str_to_title(storm_name),
         storm_name = str_trim(storm_name),
         old_id = str_trim(old_id)) %>%
  unite(storm_id, c(storm_name, year), sep = "-") %>%
  distinct()

# Check coverage of extended tracks data
# ext_track_storms <- str_replace(list.files("data-raw/extended_tracks/"),
#                                 "_eb\\.txt", "")
# ext_track_storms[ext_track_storms %in% hurr_id_table$old_id]
# ext_track_storms[!(ext_track_storms %in% hurr_id_table$old_id)]

# Read in extended tracks data and join into one big dataset
ext_tracks_files <- list.files("data-raw/extended_tracks/")
ext_tracks_wind <- lapply(ext_tracks_files, function(x){
  storm_old_id <- str_replace(x, "_eb\\.txt", "")
  file_name <- paste0("data-raw/extended_tracks/", x)
  storm_ext_tracks <- read_csv(file_name) %>%
    mutate(old_id = storm_old_id) %>%
    select(old_id, gridid, max_gust, max_sust, gust_duration,
           sust_duration)
  return(storm_ext_tracks)
}) %>%
  bind_rows() %>%
  inner_join(hurr_id_table, by = "old_id") %>%
  select(gridid, max_gust, max_sust, gust_duration, sust_duration, storm_id) %>%
  rename(fips = gridid,
         vmax_gust = max_gust,
         vmax_sust = max_sust,
         gust_dur = gust_duration,
         sust_dur = sust_duration) %>%
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0"))

ext_tracks_wind <- ext_tracks_wind %>%
  select(-gust_dur)

devtools::use_data(ext_tracks_wind, overwrite = TRUE)
