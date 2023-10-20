setwd("~/Documents/code/computing-geographically/__scratch")

library(sf)
library(tmap)
library(dplyr)
library(units)

voronoi <- function(layer) {
  layer %>%
    st_union() %>%
    st_voronoi() %>%
    st_cast() %>%
    st_as_sf(crs = st_crs(layer)) %>%
    st_join(layer)
}

steps <- seq(1, 50, 1)

xy <- read.table("traces.csv", sep = " ", header = TRUE)

xy <- xy %>%
  mutate(x = x + 1.75e6, y = y + 5.5e6) %>%
  filter(t %in% steps)

xy_sf <- xy %>%
  st_as_sf(coords = c("x", "y"), crs = 2193)

xy_bb <- xy_sf %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_sf() %>%
  st_set_crs(2193) %>%
  st_buffer(-.5)

xy_vor <- xy_sf %>%
  voronoi() %>%
  st_intersection(xy_bb) %>%
  distinct(geometry)

xy_vor_b <- xy_vor %>%
  mutate(area = st_area(.), r = as_units(.25, "m") + sqrt(area / pi)) %>%
  st_buffer(.$r) %>%
  st_intersection(xy_bb)

xy_path <- xy_sf %>%
  group_by(who) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  st_intersection(xy_bb)

tm_shape(xy_vor_b)  +
  tm_polygons(col = "r", alpha = 0.3, style = "cont",
              palette = "-plasma", border.col = "white",
              lwd = 0., legend.show = FALSE) +
  tm_shape(xy_vor) +
  tm_borders(col = "white", alpha = 0.35, lwd = 0.35) +
  tm_shape(xy_path) +
  tm_lines(col = "white", palette = "Blues", lwd = 1, alpha = 0.5,
           legend.col.show = FALSE) +
  tm_layout(frame = FALSE, bg.color = "white")
