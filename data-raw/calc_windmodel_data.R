data(hurr_tracks)
storms <- unique(hurr_tracks$storm_id)

library(stormwindmodel)
data(county_points)

library(devtools)
library(dplyr)

storm_winds <- vector("list",
                      #length = 10)
                      length = length(storms))
for(i in 1:length(storm_winds)){
  print(storms[i])
  storm_track <- subset(hurr_tracks, storm_id == storms[i])
  winds <- get_grid_winds(hurr_track = storm_track,
                          grid_df = county_points) %>%
    dplyr::select(-glat, -glon) %>%
    dplyr::rename(fips = gridid) %>%
    dplyr::mutate(storm_id = storms[i])
  storm_winds[[i]] <- winds
}

storm_winds <- do.call("rbind", storm_winds)
devtools::use_data(storm_winds, overwrite = TRUE)
