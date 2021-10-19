#' @title Stamen map API.
#' @description Enter a city name and display its stamen map.
#' @details Input takes name of a location and zoom level of the map, output will return the related Stamen map.
#' @param name Location name.
#' @param zoom_l Zoom level of the map.
#' @param map_type Stamen map type.
#' @return A list, stamen map with predefined zoom level and map type.
#' @import methods
#' @importFrom ggmap get_stamenmap ggmap
#' @importFrom httr GET content
#' @importFrom rjson fromJSON
#' @export
findmap <- function(name,zoom_l,map_type) {
  map_types = c("terrain",
                "toner",
                "watercolor")
  if(map_type %in% map_types == FALSE) {
    cat("Enter a maptype (toner,terrain or watercolor):\n")
    stop()
  }
  if(zoom_l<9 | zoom_l>12){
    stop()
  }
  if(is.na(map_type)){
    cat("map type missing")
    stop()
  }
  zoom_l = as.numeric(zoom_l)
  n_url = paste0("https://nominatim.openstreetmap.org/search.php?q=",
                 name,
                 "&format=jsonv2")
  url1 = httr::GET(url = n_url)
  url_text =  httr::content(url1, "text")
  json = rjson::fromJSON(url_text)
  location_data =  data.frame(
    name = json[[1]]$display_name,
    bbox = json[[1]]$boundingbox,
    lat = json[[1]]$lat,
    lon = json[[1]]$lon
  )
  bbox0 = as.numeric(
    c(
      left = location_data$bbox[3],
      bottom = location_data$bbox[1],
      right = location_data$bbox[4],
      top = location_data$bbox[2]
    )
  )
  map = ggmap(get_stamenmap(
    bbox = bbox0,
    zoom = zoom_l,
    maptype = map_type
  ))
  mapinfo = list("map" = map, "name" = name,"zoom_l" =  zoom_l, "map_type" =  map_type)
  # return(map)
  return(mapinfo)
}
