
#' Find Stations
#'
#' @description The following uses postcode lat and long to estimate distances
#' from the home location. This is not exact and generally assumes a post code's
#' central location as the lat/long (i.e. center of each suburb). #Find stations
#' within reference area The following uses postcode lat and long to estimate
#' distances from the home location. This is not exact and generally assumes a
#' post code's central location as the lat/long (i.e. center of each suburb)
#' @export
#' @param home_lat 
#' @param home_long 
#' @param label the reference location to show on the map
#' @param dist_from_home set the maximum distance from reference in kilometers
#' @param radius 
#' @importFrom geosphere distm
#' @import tidyverse
#' @import here here
#' @import dplyr
#'
find_stations <- function(home_lat, 
                          home_long, 
                          label = "Reference Location",
                          dist_from_home,
                          radius = dist_from_home * 1000){
  
  # Set the reference (home) location
  home_location <- data.frame(home_lat = home_lat, 
                              home_long = home_long,
                              label = label,
                              radius = radius)
  
postcodes <- get_post_codes() %>% 
  filter(type == "Delivery Area") %>% 
  # Duplicate the home location across all rows - used to calc distances
  bind_cols(home_location) %>%
  rowwise() %>%
  # Calculate distances based on postcode lat/longs:
  mutate(dist_from_home_km = geosphere::distm(c(Long_precise, Lat_precise), 
                                              c(home_long, home_lat), 
                                              fun = distHaversine)/1000) %>% 
  ungroup()

# Create a list of stations within distance from home ---------------------

post_codes_near_home <<- postcodes %>%
  filter(dist_from_home_km <= dist_from_home) %>% 
  arrange(dist_from_home_km)


# Geocode Lat and Long to Fuel Stations -----------------------------------

if(file.exists(paste0(here::here("data"), "/station_list.RDS"))){
  
  station_list_indexed <- readRDS(paste0(here::here("data"), "/station_list.RDS"))
  
} else { 
# Stations change names, and their addresses can be duplicated
station_list <- fuel_data %>%
  # mutate(ADDRESS = sub(" \\(.*", "", .$ADDRESS)) %>% # Some addresses have brackets with intersections
  group_by(ADDRESS, LOCATION, POSTCODE, TRADING_NAME) %>%
  summarise(max_date = max(PUBLISH_DATE)) %>%
  arrange(desc(max_date)) %>%
  slice(1) %>% # Take the top row
  ungroup()

# Identify remaining duplicates:
n_occur <- data.frame(table(station_list$ADDRESS))
n_occur[n_occur$Freq > 1,]

# Map the most recent station entries with their coded index references:
station_list_indexed <- station_list %>% 
  left_join(fuel_data %>% select(ADDRESS, id) %>% 
              distinct(ADDRESS, 
                       .keep_all = TRUE), 
            by = c("ADDRESS" = "ADDRESS"))

# Identify NA values from indexed data:
length(which(is.na(station_list_indexed$id)))


# Geocode Lat and Long ----------------------------------------------------

station_list_indexed <- station_list_indexed %>% 
  mutate(full_address = paste0(ADDRESS, ", ", LOCATION, ", Western Australia")) 

station_positions <- get_pos(station_list_indexed$full_address)

# Add lat and long to unique station list
station_list_indexed <- cbind(station_list_indexed, station_positions)

station_list_indexed <- station_list_indexed %>% 
  rename(lng = long)

saveRDS(station_list_indexed, paste0(here::here("data"), "/station_list.RDS"))

}

stations_near_home <- station_list_indexed %>% 
  filter(POSTCODE %in% post_codes_near_home$postcode) %>% 
  bind_cols(home_location) %>%
  rowwise() %>%
  # Calculate distances based on postcode lat/longs:
  mutate(dist_from_home_km = geosphere::distm(c(lng, lat), 
                                              c(home_long, home_lat), 
                                              fun = distHaversine)/1000) %>% 
  ungroup() %>% 
  filter(dist_from_home_km <= dist_from_home)


# return(station_list_indexed)
return(stations_near_home)

}