library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# Make a table matching storm ID from extended tracks (e.g., "AL0188") to
# our storm IDs (e.g., "Alberto-1988")

# Bring in latest version of HURDAT2
hurdat2 <- read_lines("https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2018-051019.txt")

# Split into the rows with storm names (e.g., "AL011851") and those with storm data
hurdat2_names <- hurdat2[str_detect(hurdat2, "AL[0-9]{6}.")]

unnamed_names <- c("Unnamed", "One", "Two", "Five", "Eight", "Nine", "Ten",
                   "Sixteen", "Nineteen", "Twenty-Two", "Alpha", "Beta",
                   "Gamma", "Delta", "Epsilon", "Zeta")

# Clean up the storm names
hurr_id_table <- hurdat2_names %>%
  tibble() %>%
  rename(x = ".") %>%
  separate(x, into = c("old_id", "storm_id", "n_rows"), sep = ", ") %>%
  mutate_all(str_trim) %>%
  select(-n_rows) %>%
  filter(as.numeric(str_sub(old_id, 5, 8)) >= 1988) %>%
  mutate(storm_id = str_to_title(storm_id),
         short_old_id = str_sub(old_id, 1, 4),
         storm_year = str_sub(old_id, 5, 8)) %>%
  mutate(storm_id = ifelse(storm_id %in% unnamed_names,
                           short_old_id, storm_id)) %>%
  mutate(storm_id = paste(storm_id, storm_year, sep = "-"),
         old_id = paste0(str_sub(old_id, 1, 4),
                         str_sub(old_id, 7, 8))) %>%
  select(old_id, storm_id)

# Check coverage of wind radii data
# wr_track_storms <- str_replace(list.files("data-raw/wind_radii/"),
#                                "_hdout\\.txt", "")
# wr_track_storms[wr_track_storms %in% hurr_id_table$old_id]
# wr_track_storms[!(wr_track_storms %in% hurr_id_table$old_id)]

# Read in wind radii data and join into one big dataset
wr_tracks_files <- list.files("data-raw/wind_radii/")
wr_tracks_wind <- lapply(wr_tracks_files, function(x){
  storm_old_id <- str_replace(x, "_hdout\\.txt", "")
  file_name <- paste0("data-raw/wind_radii/", x)
  storm_ext_tracks <- read_csv(file_name) %>%
    mutate(old_id = storm_old_id) %>%
    select(old_id, gridid, vmax_gust, vmax_sust, sust_duration)
  return(storm_ext_tracks)
}) %>%
  bind_rows() %>%
  inner_join(hurr_id_table, by = "old_id") %>%
  select(gridid, vmax_gust, vmax_sust, sust_duration, storm_id) %>%
  rename(fips = gridid) %>%
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0"))

# Rename to fit into existing package structure
ext_tracks_wind <- wr_tracks_wind

usethis::use_data(ext_tracks_wind, overwrite = TRUE)
