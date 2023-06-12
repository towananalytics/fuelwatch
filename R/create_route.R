
# Code taken from
# https://stackoverflow.com/questions/41061718/calculate-decode-and-plot-routes-on-map-using-leaflet-and-r/73591072#73591072

options(timeout = 60) # Change the timeout from the default 60 - files take longer to run

#' Create Routes
#' 
#' @param df 
#' @param home_lat 
#' @param home_long 
#' @param home_label 
#' @param station_label 
#' @param dist_from_home 
#'
#' @return leaflet map showing points connected via routes
#' @import leaflet stringr bitops htmltools rjson dplyr R.utils
#'
  create_routes <- function(
                            df,
                            home_lat,
                            home_long,
                            home_label,
                            station_label,
                            dist_from_home){
    
    if(is.null(station_label)){ #Nothing supplied for station labels
      
      df$label <- "" # Add a blank column
      
    } else {
      
      df$label <- station_label # insert the names specified
      
    }
      
      home <- data.frame(home_lat =  home_lat,
                         home_long = home_long,
                         home_label = home_label,
                         radius = dist_from_home)
    
    
    # closeAllConnections() # sometimes errors without this
    
    nn <- nrow(df)
    
    # Functions
    # =========
    viaroute <- function(lat1, lng1, lat2, lng2) {
      R.utils::withTimeout({
        repeat {
          res <- try(
            route <- rjson::fromJSON(
              file = paste("http://router.project-osrm.org/route/v1/driving/",
                           lng1, ",", lat1, ";", lng2, ",", lat2,
                           "?overview=full", sep = "", NULL)))
          if (class(res) != "try-error") {
            if (!is.null(res)) {
              break
            }
          }
        }
      }, timeout = 1, onTimeout = "warning")
      return(res)
    }
    
    decode_geom <- function(encoded) {
      scale <- 1e-5
      len = str_length(encoded)
      encoded <- strsplit(encoded, NULL)[[1]]
      index = 1
      N <- 100000
      df.index <- 1
      array = matrix(nrow = N, ncol = 2)
      lat <- dlat <- lng <- dlnt <- b <- shift <- result <- 0
      
      while (index <= len) {
        # if (index == 80) browser()
        shift <- result <- 0
        repeat {
          b = as.integer(charToRaw(encoded[index])) - 63
          index <- index + 1
          result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
          shift = shift + 5
          if (b < 0x20) break
        }
        dlat = ifelse(bitAnd(result, 1),
                      -(result - (bitShiftR(result, 1))),
                      bitShiftR(result, 1))
        lat = lat + dlat;
        
        shift <- result <- b <- 0
        repeat {
          b = as.integer(charToRaw(encoded[index])) - 63
          index <- index + 1
          result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
          shift = shift + 5
          if (b < 0x20) break
        }
        dlng = ifelse(bitAnd(result, 1),
                      -(result - (bitShiftR(result, 1))),
                      bitShiftR(result, 1))
        lng = lng + dlng
        
        array[df.index,] <- c(lat = lat * scale, lng = lng * scale)
        df.index <- df.index + 1
      }
      
      geometry <- data.frame(array[1:df.index - 1,])
      names(geometry) <- c("lat", "lng")
      return(geometry)
    }
    
    # content <- paste(sep = "<br/>",
    #                  "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
    #                  "606 5th Ave. S",
    #                  "Seattle, WA 98138"
    # )
    # 
    # leaflet() %>% addTiles() %>%
    #   addPopups(-122.327298, 47.597131, content,
    #             options = popupOptions(closeButton = FALSE)
    #   )
    
    map <- function() {
      m <- leaflet() %>%
        addTiles(group = "OSM") %>%
        addProviderTiles("Stamen.TonerLite") %>%
        addLayersControl(
          baseGroups = c("OSM", "Stamen.TonerLite")
        )
      return(m)
    }
    
    # leaflet(df) %>% addTiles() %>%
    #   addMarkers(df$lng, df$lat, popup = ~htmlEscape(df$name))
    
    map_route <- function(df, my_list) {
      m <- map()
     # m <- leaflet(df)
      m <- addCircleMarkers(map = m,
                            lat = df$lat,
                            lng = df$lng,
                            color = "blue",
                            stroke = FALSE,
                            label = df$label,
                            radius = 6,
                            fillOpacity = 0.8) %>%
        addLayersControl(baseGroups = c("OSM", "Stamen.TonerLite")) %>% 
        addMarkers(lat = home$home_lat,
                   lng = home$home_long,
                   label = home$home_label) %>% 
        addCircles(lng = home$home_long, 
                   lat = home$home_lat, 
                   radius = home$radius)
    
          for (i in 1:length(my_list)) {
            m <- addPolylines(map = m, lat = my_list[[i]]$lat, lng = my_list[[i]]$lng, color = "red", weight = 4)
          }
          return(m)
    }

    # Main
    # ======
    # m <- map()
    # m <- m %>% addCircleMarkers(lat = df$lat,
    #                             lng = df$lng,
    #                             color = "red",
    #                             stroke = FALSE,
    #                             radius = 10,
    #                             fillOpacity = 0.8) %>%
    #   addMarkers(lat = home$home_lat,
    #              lng = home$home_long,
    #              label = "Home")
    
    my_list <- list()
    r <- 1
    temp <- data.frame(distance = NA, travel_time = NA)
    
    for (i in 1:(nn)) {
      #for (j in ((i+1):nn)) {
        # my_route <- viaroute(df$lat[i], df$lng[i],df$lat[j], df$lng[j])
        my_route <- viaroute(home$home_lat, home$home_long, df$lat[i], df$lng[i])
        geom <- decode_geom(my_route$routes[[1]]$geometry)
        distance <- my_route$routes[[1]]$legs[[1]]$distance
        travel_time <- my_route$routes[[1]]$legs[[1]]$duration
        temp[i, 1] <- distance
        temp[i, 2] <- travel_time
        my_list[[r]] <- geom

        r <- r + 1
        
     # }

    }
    # my_list <<- my_list 
    df_create_route_out <- df %>% bind_cols(temp)
    
    # temp <<- temp
    
    # myroute <<- my_route
    return(list(route_map = map_route(df, my_list),
                route_data = df_create_route_out))
    
    #
    
    # in some cases, a time out error may occur, need to simply run gh() in the
    # console
  }