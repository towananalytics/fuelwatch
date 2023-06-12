

#' Get Position (Latitude and Longitude)
#' @description Return a Latitude and Longitude for an address. This is a
#' wrapper for the tmaptools package
#'
#'
#' @param full_address
#'
#' @return a dataframe containing the lat and long of the address provided
#' @export
#' @import tmaptools
#'
#' @examples
#' lat_lng <- get_pos("Liberty Island, New York, NY 10004")
#' 

  get_pos <- function(full_address){
    
    p1 <- proc.time()
    
    mat <- matrix(nrow = length(full_address), ncol = 2)
    
    for(i in 1:length(full_address)){  
      
      geo_inf <- geocode_OSM(full_address[i], projection = "WGS84")
      
      if(is.null(geo_inf)) { # Could not find the address
        
        mat[i, 1] <- NA
        mat[i, 2] <- NA  
        
      } else {
        
        mat[i, 1] <- geo_inf$coords[1]
        mat[i, 2] <- geo_inf$coords[2]  
        
      }  
      
    }
    
    # print(proc.time() - p1)
    print(paste0(full_address[i], " - Complete"))
    mat <- as.data.frame(mat)
    names(mat) <- c("long", "lat") 
    return(mat)
    
  }
  