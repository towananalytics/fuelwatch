

#' Get Fuel Data
#' @description Extract the fuel data either from the local project folder or
#'   download from fuel watch.
#'
#' @details This function first checks to see if the RDS file is available and
#'   opens it. It then checks the age of the data, and if more than two days
#'   old, downloads the remaining data up to the current month. \cr \cr If there
#'   is not RDS file, the raw CSV files are checked and opened, and any missing
#'   are downloaded up to the current month. \cr \cr If there are no CSV files,
#'   all data is downloaded from the fuelwatch website. Note that this can take
#'   considerable time and data useage
#'
#'
#' @return a dataframe of fuel price data
#' @export
#' @import lubridate
#' @import here here
#'
#' @examples
#' dontrun{
#' fuel <- get_fuel_data()
#' }
#' 
get_fuel_data <- function(){

# Check existing data -----------------------------------------------------
  if(file.exists(paste0(here::here("data"), "/fuel_data.RDS"))){
    
    fuel_data <- readRDS(paste0(here::here("data"), "/fuel_data.RDS"))
    
    if(as.Date(Sys.Date()) - as.Date(max(fuel_data$PUBLISH_DATE)) < 2){
      
      # Data in .RDS file is less than 2 days old so use RDS file
      print(".RDS data is current, nothing to download.")
      
      return(fuel_data)
      
    } else {
      
      # Data is older than 2 days. so download recent data from Fuel Watch
      
      print("Data is more than 2 days old, 
            downloading recent updates from FuelWatch.")
      
      most_recent_data <- format(as.Date(max(fuel_data$PUBLISH_DATE)), 
                                 "%Y-%m-%d")
      
      fuel_data <- download_fuelwatch(start_year = format(as.Date(most_recent_data),
                                                          "%Y-%m-01"))
      
      return(fuel_data)
      
    }
    
  } else { # RDS fuel date file does not exist in the directory - look for CSV files
    
    csv_name_pattern_date <- "FuelWatchRetail-\\s*(.*?)\\s*.csv" # Extracts the date from filename
    
    csv_name_pattern <- "FuelWatchRetail.*\\.csv$" # has "FuelWatchRetail", followed by 0 or more characters,
                                                    # then ".csv", and then nothing else ($) 
    
    temp <- list.files(paste0(here::here("data")), 
                            pattern = csv_name_pattern, 
                            full.names = TRUE)
    
    if(length(temp > 0)){ # CSV files exist in the directory so compile 
                          # them into one dataframe
      
      # Extract a list of dates from file names:
      result <- regmatches(temp, regexec(csv_name_pattern_date, temp))
      
      # Extract 2nd element of list (date)
      years_months <- sapply(result, '[[', 2) 

      # Find the most recent file name (date)
      most_recent_data <- format(max(lubridate::parse_date_time(years_months,
                                                                 "%m-%Y")),
                                  "%Y-%m-%d")
      
      # Now download the last data set up to the most recent (current) month
      fuel_data <- download_fuelwatch(start_year = format(as.Date(most_recent_data),
                                                          "%Y-%m-01"))
      
      return(fuel_data)

    } else { # no data exists, download all data from FuelWatch website
              # CAUTION - this takes significant time to download 70+ files
      start_date <- as.Date('2017-01-01')
      end_date <- Sys.Date()
      number_files <- length(seq(from=start_date, to=end_date, by='month'))
      file_size <- number_files * 8 # each CSV is approx. 8Mb in size
      
     message(paste0("Download all FuelWatch data from website? There are ", 
                  number_files, " files totalling about ", 
                  file_size, 
                  " megabytes and may take some time to download."))
      
      menu_sel <- menu(c("Yes", "No"), 
                       graphics = FALSE,
                       title = "Continue download?")
      
      if(menu_sel == 1){
        
              fuel_data <- download_fuelwatch(start_year = start_date)
              return(fuel_data)
        
      } else {
        
        message("Download cancelled.", appendLF = T)
        
        }
      
    }
      
    }

}

#' Get Postcodes
#' @description Open the pre-saved postcode RDS file or download it from github
#' if it doesn't exist.
#' 
#' Postcodes are used in geolocation functions of the package.
#'
#' @import here
#' @return a dataframe of postcode data
#' @export
#'
#' @examples
#'
#' dontrun{
#' postcodes <- get_postcodes
#' }
#' 
get_postcodes <- function(){
  
  if(file.exists(paste0(here::here("data"), "/postcodes.RDS"))){
    
    postcodes <- readRDS(paste0(here::here("data"), "/postcodes.RDS"))
    
  } else { # Postcode rds file does exist so download the data from github

    postcodes <- download_postcodes()
    
}

  return(postcodes)
  
}
