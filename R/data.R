#' Location of United States county centers of population
#'
#' A dataset containing the latitude and longitude of the
#' center of population of every United State county,
#' based on population as of the 2010 US Census.
#'
#' @details The latitude and longitude of each county are the county's
#' population-weighted mean center of population, based on population as of the
#' 2010 US Census. For more details on the calculation of these mean centers of
#' population, see the reference below.
#'
#' @format A data frame with 3,221 rows and 6 variables:
#' \describe{
#'   \item{fips}{County's 5-digit Federal Information Processing Standard (FIPS)
#'              code}
#'   \item{county_name}{County name}
#'   \item{state_name}{State name}
#'   \item{population}{Population of the county as of the 2010 US Census}
#'   \item{latitude}{Latitude of county's center of population, in decimal
#'                  degrees}
#'   \item{longitude}{Longitude of county's center of population, in decimal
#'                   degrees (note: longitudes are given as negative values for
#'                   longitudes west of the prime meridian)}
#' }
#'
#' @source \url{https://www.census.gov/geo/reference/centersofpop.html}
#'
#' @references
#' Bureau of the Census, Centers of Population Computation for the United
#' States 1950-2010, U.S. Department of Commerce, Bureau of the Census,
#'   Washington, DC, issued 2011. \url{http://www2.census.gov/geo/pdfs/reference/cenpop2010/COP2010_documentation.pdf}
"county_centers"

#' Storm tracks for Atlantic basin storms
#'
#' A dataset containing the storm tracks for Atlantic basin tropical
#' storms between 1988 and 2015, from the Extended Best Track Dataset for
#' the Atlantic basin. Only storms that came within 250 km of at least
#' one US county are included.
#'
#' @format A data frame with 4,351 rows and 5 variables:
#' \describe{
#'   \item{storm_id}{Unique storm identifier with the storm name and year,
#'                  separated by a hyphen(e.g., "Alberto-1988",
#'                  "Katrina-2005")}
#'   \item{date}{Character string with date and time of storm track recording, in
#'               the Universal Time Coordinated (UTC) time zone. This date is formated
#'               as \code{\%Y\%m\%d\%H\%M}.}
#'   \item{latitude}{Latitude of storm center, in decimal degrees}
#'   \item{longitude}{Longitude of storm center, in decimal degrees (note:
#'                    longitudes are given as negative values for
#'                    longitudes west of the prime meridian)}
#'   \item{wind}{1-minute maximum sustained surface wind speed, measured at
#'              10 meters above the ground, in knots (values are rounded to
#'              the nearest 5-knot value)}
#' }
#'
#' @source \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset}
#'
#' @references
#' Demuth J, DeMaria M, Knaff JA, 2006. Improvement of advanced microwave
#' sounder unit tropical cyclone intensity and size estimation algorithms.
#' Journal of Applied Meteorology and Climatology 45:1573-1581
#'
"hurr_tracks"

#' Closest distances between counties and a storm track
#'
#' A dataframe that gives the distance and date-time for the closest
#' approach of each tropical storm to the mean population center of each
#' US county in states in the eastern half of the United States. This data includes
#' all Atlantic-basin storms between 1988 and 2015 that came within at least 250 km
#' of at least one US county.
#'
#' @details The minimum distance was calculated using the Great Circle method,
#' using the \code{spDist} function from the \code{sp} package.
#' The time of the closest approach of the storm to each county was converted from UTC
#' to local time using the \code{countytimezones} package for the \code{local_time}
#' and \code{closest_date} columns.
#'
#' @format A dataframe with 325,856 rows and 4 variables:
#' \describe{
#'   \item{storm_id}{Character string with unique storm identifier with the storm name
#'                   and year, separated by a hyphen (e.g., "Alberto-1988",
#'                   "Katrina-2005")}
#'   \item{fips}{Character string with the county's 5-digit Federal Information
#'               Processing Standard (FIPS) code}
#'  \item{closest_time_utc}{Character string of time, in UTC, of the closest approach of
#'                          the storm to the county's population mean center, based on
#'                          storm tracks linearly interpolated to 15-minute increments.}
#'   \item{storm_dist}{Numeric of the minimum distance (in kilometers) between the storm's
#'                     track and the county's population mean center.}
#'   \item{local_time}{Character string of local time of the closest approach of the storm to the
#'                     county's population mean center, based on storm tracks linearly
#'                     interpolated to 15-minute increments.}
#'   \item{closest_date}{Character string with date (based on local time) of the closest
#'                       approach of the storm to the county's population mean center.}
#' }
"closest_dist"

#' Rainfall for US counties during Atlantic basin tropical storms
#'
#' A dataframe that gives daily rainfall in US counties from five days before to
#' three days after a tropical storm's closest approach to the county. This dataset
#' covers all Atlantic-basin tropical storms between 1988 and 2011 for storms that came
#' within 250 km of at least one US county. Only counties in the eastern half of the US
#' are included.
#'
#' @format A dataframe with 2,673,936 rows and 5 variables:
#' \describe{
#'   \item{fips}{A character string with the county's 5-digit Federal Information
#'               Processing Standard (FIPS) code}
#'   \item{storm_id}{A character string with a unique storm identifier with the storm
#'                  name and year, separated by a hyphen (e.g., "Alberto-1988",
#'                  "Katrina-2005")}
#'   \item{lag}{Number of days from date when storm was closest to the county
#'              (e.g., \code{0} indicates the date the storm was closest to the
#'              county, \code{-2} indicates two days before the date when the
#'              storm was closest to the county)}
#'   \item{precip}{Average daily precipitation, in millimeters, for NLDAS grid
#'    points with the county for the given lag day}
#'   \item{precip_max}{Maximum daily precipitation, in millimeters, across the NLDAS grid
#'    points with the county for the given lag day}
#' }
#'
#' @details This dataset was aggregated from hourly, 1/8 degree gridded data
#'    from North America Land Data Assimilation System Phase 2 (NLDAS-2)
#'    precipitation data files to generate daily, county-level precipitation
#'    measures. The NLDAS-2 data integrates satellite-based and land-based
#'    monitoring and applies a land-surface model to create a reanalysis
#'    dataset that is spatially and temporally complete across the continental
#'    United States. To aggregate to a daily county-level value, we averaged
#'    the data at each grid point to generate a daily averaged and then
#'    averaged this value across all grid points within a county's boundaries.
#'    We used county boundaries based on boundaries at the time of the 1990
#'    US Census. The date-time for each observation at each grid point was
#'    converted to local time (not considering Daylight Savings Time) before
#'    daily averages were generated for the grid point.
#'
#' @note This data set is based on data acquired as part of the mission of
#'    NASA-s Earth Science Division and archived and distributed by the
#'    Goddard Earth Sciences (GES) Data and Information Services Center (DISC).
#'    The county-aggregated daily precipitation values used for this data set
#'    are available for download through the Center for Disease Control's
#'    Wide-ranging Online Data for Epidemiological Research (WONDER) system
#'    (see references).
#'
#' @author
#'    William Crosson \email{bill.crosson@nasa.gov},
#'    Mohammad Al-Hamdan \email{mohammad.alhamdan@nasa.gov}, and
#'    Brooke Anderson \email{brooke.anderson@colostate.edu}
#'
#' @references
#'
#' Al-Hamdan MZ, Crosson WL, Economou SA, Estes MG, Estes SM, Hemmings SN,
#' Kent ST, Puckette M, Quattrochi DA, Rickman DL, Wade GM, McClure LA, 2014.
#' Environmental public health applications using remotely sensed data.
#' Geocarto International 29(1):85-98.
#'
#' North America Land Data Assimilation System (NLDAS) Daily Precipitation
#' years 1979-2011 on CDC WONDER Online Database, released 2012.
#' \url{http://wonder.cdc.gov/wonder/help/Precipitation.html}
#'
#' Rui H, Mocko D, 2014. README Document for North America Land Data
#' Assimilation System Phase 2 (NLDAS-2) Products. Goddard Earth Sciences
#' Data and Information Services Center.
"rain"

#' Modeled county wind speeds for historical storms
#'
#' A dataframe with modeled winds for historical tropical storms in the
#' Atlantic basin for U.S. counties from 1988 to 2015. For each county,
#' the given wind speed is that modeled at the county's population mean
#' center (based on the 2010 U.S. Census).
#'
#' @format A dataframe with 325,856 rows and 6 variables:
#' \describe{
#'   \item{fips}{County's 5-digit Federal Information Processing Standard (FIPS)
#'              code}
#'   \item{vmax_gust}{Maximum modeled gust wind speed in the county during
#'              the storm.}
#'   \item{vmax_sust}{Maximum modeled sustained wind speed in the county during
#'              the storm.}
#'   \item{gust_dur}{Time gust wind was above 20 m / s in the county during
#'              the storm.}
#'   \item{sust_dur}{Time sustained wind was above 20 m / s in the county during
#'              the storm.}
#'   \item{storm_id}{Unique storm identifier with the storm name and year,
#'              separated by a hyphen(e.g., "Alberto-1988",
#'              "Katrina-2005")}
#' }
#'
#' @note These wind speeds were modeled from hurricane best tracks
#'    data using the \code{stormwindmodel} package to implement the
#'    Willoughby wind model. See the "Details" vignette from that package
#'    (available at \url{https://cran.r-project.org/web/packages/stormwindmodel/vignettes/Details.html})
#'    for extensive details on this modeling process.
#'
#' @author
#' Brooke Anderson \email{brooke.anderson@colostate.edu},
#' Andrea Schumacher \email{andrea.schumacher@colostate.edu},
#' Joshua Ferreri \email{joshua.m.ferreri@gmail.com},
#' Seth Guikema \email{sguikema@umich.edu}, and
#' Steven Quiring \email{quiring.10@osu.edu}
"storm_winds"

#' County storm event listings associated with tropical storms
#'
#' A list with any county-level storm event listings from NOAA Storm
#' Events that were near an Atlantic basin tropical storm in location and time.
#' Only storms that came within 250 km of at least one US county were included.
#' This database covered tornadoes and a few other types of events throughout
#' the full period (1988-2015). Some event types were only included in the database
#' for 1996 and later (e.g. flood events). See the documentation at the website
#' listed in the sources for more information on this database.
#'
#' @format A list with 137 elements. Each element is named after a specific
#'    tropical storm and is a dataframe with 2 variables:
#' \describe{
#'   \item{fips}{County's 5-digit Federal Information Processing Standard (FIPS)
#'              code}
#'   \item{events}{List of extreme events listings for a specific county and storm.
#'         Each event is given by event type (e.g., "Tornado", "Flood", "Flash Flood").
#'         See documentation in the website referenced below for the NOAA Storm Events
#'         database for more information.}
#' }
#'
#' @note Listings were generated using the \code{noaastormevents} package. An event
#'    listed in the NOAA Storm Events database was matched to a tropical storm if
#'    its beginning date was within a five-day window of the date the tropical storm
#'    passed closest to the county and if the storm came within 500 km of the county.
#'    See the documentation for the \code{noaastormevents} package (currently
#'    available through GitHub) for more details on this process. All events that were
#'    listed by county were captured for a storm; events that were reported at the
#'    forecast zone were also included if it was possible to link the event to its
#'    appropriate county. If no events in the NOAA Storm Events database were associated
#'    with a tropical storm, that storm is not included in this dataset.
#'
#' @author
#' Brooke Anderson \email{brooke.anderson@colostate.edu} and
#' Ziyu Chen \email{zailchen17@icloud.com}
#'
#' @source
#' Event listings were obtained from the NOAA Storm Events Database, available at:
#' https://www.ncdc.noaa.gov/stormevents/
"storm_events"

#' Extended best tracks county wind speeds for historical storms
#'
#' A dataframe with storm winds based on extended best tracks hurricane
#' data for historical Atlantic basin storms.
#'
#' @format A dataframe with 162,928 rows and 5 variables:
#' \describe{
#'   \item{fips}{County's 5-digit Federal Information Processing Standard (FIPS)
#'              code}
#'   \item{vmax_gust}{Maximum modeled gust wind speed in the county during
#'              the storm.}
#'   \item{vmax_sust}{Maximum modeled sustained wind speed in the county during
#'              the storm.}
#'   \item{sust_dur}{Minutes sustained wind was above 34 knots in the county during
#'              the storm.}
#'   \item{storm_id}{Unique storm identifier with the storm name and year,
#'              separated by a hyphen(e.g., "Alberto-1988",
#'              "Katrina-2005")}
#' }
#'
#' @details To calculate the wind values in this dataset, we used the wind radii from
#'    the Extended Best Tracks dataset for Atlantic-basin tropical storms (see the
#'    reference and source). These wind radii provide the radius in each of four quadrants
#'    (northeast, southeast, southwest, and northwest) from the storm's center for each
#'    of the 6-hour storm observations. We interpolated this data to every 15 minutes
#'    and scaled each radius to 0.85 of its full value (the original radius gives the
#'    maximum extent of 64-knot, 50-knot, and 34-knot winds in each quadrant). For each
#'    15-minute point along the storm's track, we determined which counties fell within
#'    each wind radius. We then determined, for each county, the maximum wind value
#'    over the course of the storm. These wind values are limited to 34 knots, 50 knots,
#'    and 64 knots; if a county is reported as having 64-knot winds in this dataset, it
#'    therefore means that the county fell within 0.85 times the 64-knot maximum wind
#'    radius at least once during the storm, and so can be interpreted as the county
#'    likely experiencing at least 64-knot winds during the course of the storm.
#'
#'    Gust wind speed was calculated using a gust factor of 1.49 applied to the estimated
#'    sustained wind speed (see the "Details" vignette of the \code{stormwindmodel}
#'    package for more details on the choice of this gust factor). Durations were based
#'    on the number of minutes winds were above 34 knots in the county over the course of
#'    the storm.
#'
#' @author
#'    Andrea Schumacher \email{andrea.schumacher@colostate.edu}
#'
#' @source
#' Extended best tracks data were obtained from:
#' \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}
#'
#' @references
#' Demuth J, DeMaria M, Knaff JA, 2006. Improvement of advanced microwave sounder
#' unit tropical cyclone intensity and size estimation algorithms. Journal of Applied
#' Meteorology 45:1573-1581.
"ext_tracks_wind"
