## Be sure to re-build package after running 02 before running this script

library(noaastormevents)
library(dplyr)

data(hurr_tracks, package = "hurricaneexposuredata")
storm_id_table <- hurr_tracks %>%
  select(storm_id, usa_atcf_id) %>%
  distinct()
storm_years <- gsub(".+-", "", storm_id_table$storm_id)
storms <- storm_id_table$storm_id

storm_events <- vector("list", length(storms))
names(storm_events) <- storms

for(storm_year in unique(storm_years)){
  print(storm_year)
  yearly_storms <- storms[storm_years == storm_year]
  for(storm in yearly_storms){
    print(storm)
    i <- which(storms == storm)
    this_storm_events <- find_events(storm = storm, dist_limit = 500) %>%
      dplyr::rename(type = event_type) %>%
      dplyr::mutate(fips = stringr::str_pad(fips, width = 5, side = "left", pad = "0")) %>%
      dplyr::select(fips, type) %>%
      dplyr::group_by(fips) %>%
      dplyr::summarize(events = list(type))
    storm_events[[i]] <- this_storm_events
  }
  # Remove data after each year (otherwise, this is lots of data)
  rm(noaastormevents_package_env)
}

usethis::use_data(storm_events, overwrite = TRUE)
