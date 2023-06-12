

options(timeout = 90) # Change the timeout from the default 60 - files take longer to download


#' Download the data
#'
#' @description Download fuel watch data from the website
#'
#'
#' @param start_year start year to download data
#' @param end_year end year to download data
#' @param temp_path path to store data
#' @param base_url fuelwatch website for data extraction (end point)
#'
#' @import lubridate
#' @import dplyr
#' @import here here
#' @import stringr
#'
#' @return downloads csv data from Fuelwatch website and writes a dataframe.
#'   directory
#' @export
#'
#' @examples
#' dontrun{
#' postcodes <- download_fuelwatch()
#' }
download_fuelwatch <- function(start_year, 
                               end_year = Sys.Date(),
                               temp_path = here::here("data"),
                               base_url = 'https://warsydprdstafuelwatch.blob.core.windows.net/historical-reports/FuelWatchRetail-'
) {


# Extract beginning of file name from base url
filename_start <- sub(".*/", "", base_url)
# temp_path <- 'C:/Users/nick.dawe/OneDrive - Pilbara Ports Authority/Documents/R Projects Via Git/Fuel Price Tracking/data'

#  Create years -----------------------------------------------------------

end_year <- Sys.Date()
start_year <- as.Date(start_year)

range <- format(ymd(seq(start_year, end_year, "months")), '%m-%Y')

for(x in 1:length(range)){
  
  # Download each file between the ranges above and save them to the project
  # folder:
  download.file(url = paste0(base_url, 
                             range[x],
                             '.csv'), 
                destfile = paste0(temp_path, 
                                  '/', 
                                  filename_start, 
                                  range[x], '.csv'))
}

# Read in the entire downloaded dataset -----------------------------------

temp = list.files(path = paste0(here::here('data'), '/'), 
                  pattern="*.csv",
                  full.names = T)

myfiles = lapply(temp, 
                 read.csv)

fuel_data <- bind_rows(myfiles, 
                       .id = "column_label")


# Process Data ------------------------------------------------------------


fuel_data <- fuel_data %>% 
  mutate(PUBLISH_DATE = lubridate::parse_date_time(fuel_data[, 2], 
                                                   orders = c("d/m/y"))) 

to_remove <- as.numeric(rownames(fuel_data[is.na(fuel_data$PUBLISH_DATE), ])) # ID NA values due to dates

fuel_data <- fuel_data %>% 
  filter(!row_number() %in% to_remove) # Remove NA values

fuel_data <- fuel_data %>%
  select(-column_label) %>% 
  mutate(ADDRESS = sub(" \\(.*", "", .$ADDRESS), # Some addresses have brackets with intersections
         year = format(as.Date(PUBLISH_DATE), "%Y"),
         year_month = format(as.Date(PUBLISH_DATE), "%Y-%m"),
         year_week = paste0(year(PUBLISH_DATE), "-", sprintf("%02d",
                                                             lubridate::week(PUBLISH_DATE))),
         day_of_week = lubridate::wday(PUBLISH_DATE, label = TRUE),
         day_of_week_num = lubridate::wday(PUBLISH_DATE, label = FALSE),
         PRODUCT_DESCRIPTION = stringr::str_replace(PRODUCT_DESCRIPTION, 
                                                    "Brand Diesel", "Diesel"),
         id = row.names(.)) # Use this for joining station names

# Save new Data -----------------------------------------------------------

saveRDS(fuel_data, file = paste0(here::here("data"), "/fuel_data.RDS"))

return(fuel_data)

}

# Extract Postcode Data ---------------------------------------------------

#' Download Postcode data
#'
#' @return downloads an RDS file from github repo and extracts a dataframe
#' @export
#'
#' @examples
#' dontrun{
#' postcodes <- download_postcodes()
#' }
#' 
download_postcodes <- function(){
  
  post_codes <- "https://raw.githubusercontent.com/matthewproctor/australianpostcodes/master/australian_postcodes.csv"
  
  download.file(url = post_codes,
                destfile = paste0(here::here(),
                                  '/postcodes.csv'))
  postcodes <- read.csv(here::here("postcodes.csv"))
  
  saveRDS(postcodes, paste0(here::here("data"), "/postcodes.RDS"))
  
  return(postcodes)
  
}
