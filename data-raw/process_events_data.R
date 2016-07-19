library(noaastormevents)
library(dplyr)

data(hurr_tracks)
storms <- unique(hurr_tracks$storm_id)
storm_years <- gsub(".+-", "", storms)

storm_events <- vector("list", length(storms))
names(storm_events) <- storms

for(storm_year in unique(storm_years)){
  print(storm_year)
  yearly_storms <- storms[storm_years == storm_year]
  for(storm in yearly_storms){
    print(storm)
    i <- which(storms == storm)
    this_storm_events <- find_events(storm = storm, dist_limit = 500) %>%
      dplyr::select(fips, type) %>%
      dplyr::group_by(fips) %>%
      dplyr::summarize(events = list(type))
    storm_events[[i]] <- this_storm_events
  }
  # Remove data after each year (otherwise, this is lots of data)
  rm(noaastormevents_package_env)
}

devtools::use_data(storm_events, overwrite = TRUE)
