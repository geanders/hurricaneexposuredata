data("closest_dist")
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

check_dates <- dplyr::select(closest_dist, -storm_dist) %>%
        dplyr::mutate(closest_date = ymd(closest_date)) %>%
        dplyr::rename(day_0 = closest_date) %>%
        dplyr::mutate(fips = as.integer(fips),
               day_0 = day_0 + days(0),
               day_b1 = day_0 - days(1),
               day_b2 = day_0 - days(2),
               day_b3 = day_0 - days(3),
               day_a1 = day_0 + days(1),
               day_a2 = day_0 + days(2),
               day_a3 = day_0 + days(3)) %>%
        dplyr::select(storm_id, fips, day_b3, day_b2, day_b1,
               day_0, day_a1, day_a2, day_a3) %>%
        tidyr::gather(key = lag, value = day, -storm_id, -fips) %>%
        dplyr::mutate(day = as.numeric(format(day, "%Y%m%d")))

all_dates <- unique(check_dates$day)
all_fips <- unique(check_dates$fips) # has Miami as "12086", and is still in check_dates here
all_fips <- c(all_fips, as.integer(12025))
check_dates[check_dates$fips == 12086, "fips"] <- 12025

## Read and process precipitation data
rain <- data.table::fread("data-raw/nasa_precip_export_2.txt",
                      # nrows = 500000,
                      header = TRUE,
                      select = c("county", "year_month_day", "precip", "precip_max")) %>%
        dplyr::filter(county %in% all_fips,
               year_month_day %in% all_dates) %>%
        dplyr::rename(fips = county, day = year_month_day) %>%
        dplyr::right_join(data.table(check_dates),
                   by = c("fips" = "fips", "day" = "day")) %>%
        dplyr::filter(!is.na(precip) & !is.na(precip_max)) %>%
        dplyr::select(-day) %>%
        dplyr::arrange(storm_id, fips) %>%
        dplyr::select(fips, storm_id, lag, precip, precip_max) %>%
        dplyr::mutate(fips = sprintf("%05d", fips),
               lag = gsub("day_", "", lag),
               lag = gsub("b", "-", lag),
               lag = gsub("a", "", lag),
               lag = as.numeric(lag))
rain[rain$fips == 12025, "fips"] <- 12086
use_data(rain, overwrite = TRUE)
