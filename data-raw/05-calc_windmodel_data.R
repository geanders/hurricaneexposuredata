## Be sure to re-build package after running 01 and 02 and before
## running this

library(dplyr)

data(hurr_tracks, package = "hurricaneexposuredata")
storms <- unique(hurr_tracks$usa_atcf_id)
storm_id_table <- hurr_tracks %>%
  select(storm_id, usa_atcf_id) %>%
  distinct()

library(stormwindmodel)
data(county_points, package = "stormwindmodel")

library(devtools)
library(dplyr)

storm_winds <- vector("list",
                      #length = 2)
                      length = length(storms))
for(i in 1:length(storm_winds)){
  print(storms[i])
  storm_track <- subset(hurr_tracks, usa_atcf_id == storms[i])
  winds <- get_grid_winds(hurr_track = storm_track,
                          grid_df = county_points) %>%
    dplyr::rename(fips = gridid) %>%
    dplyr::mutate(usa_atcf_id = storms[i],
                  storm_id = storm_id_table$storm_id[storm_id_table$usa_atcf_id == storms[i]])
  storm_winds[[i]] <- winds
}

storm_winds <- do.call("rbind", storm_winds)
usethis::use_data(storm_winds, overwrite = TRUE)
