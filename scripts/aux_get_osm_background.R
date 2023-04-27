

get_base_map <- function(bbox = spotlight_zoom_2){

library(osmdata)
streets <- bbox %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary",
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- bbox %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- bbox %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()


# ggplot() +
#   geom_sf(data = streets$osm_lines,
#           inherit.aes = FALSE,
#           color = "black",
#           size = .4,
#           alpha = .8) +
#   coord_sf(xlim = c(7.77, 7.92),
#            ylim = c(47.94, 48.06),
#            expand = FALSE)
#
# p <- ggplot() +
#   geom_sf(data = streets$osm_lines,
#           inherit.aes = FALSE,
#           color = "black",
#           size = .4,
#           alpha = .8) +
#   geom_sf(data = small_streets$osm_lines,
#           inherit.aes = FALSE,
#           color = "black",
#           size = .4,
#           alpha = .6) +
#   geom_sf(data = river$osm_lines,
#           inherit.aes = FALSE,
#           color = "black",
#           size = .2,
#           alpha = .5)
return(list(streets, small_streets, river))}

