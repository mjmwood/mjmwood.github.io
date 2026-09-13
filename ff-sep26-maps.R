library(osmdata)
library(sf)
library(ggplot2)
# library(ggspatial)

# --- 1. City points ---
cities <- data.frame(
  city = c("London", "Paris"),
  lon  = c(-0.1276, 2.3522),
  lat  = c(51.5074, 48.8566)
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

# --- 2. Query Eurostar route geometry from OSM ---
# Relation 8214799 = "Eurostar: London → Amsterdam"
# We fetch its member ways, then clip to London–Paris extent.
# Using a raw Overpass query because osmdata's key/value interface
# can't filter by relation ID directly.

eurostar_query <- "[out:xml][timeout:120];
  relation(8214799);
  way(r);
  (._;>;);
  out body;"

eurostar_data <- opq(bbox = c(-0.5, 48.5, 2.8, 51.6)) |>
  add_osm_feature(key = "railway", value = "rail") |>
  osmdata_sf()

eurostar_lines <- eurostar_data$osm_lines

# The route geometry lives in osm_lines (individual way segments).
# osm_multilines may also exist but osm_lines is more reliable here.
eurostar_lines <- eurostar_data$osm_lines

# Clip to a bounding box that covers London–Paris only
# (the full relation continues to Amsterdam)
clip_box <- st_bbox(c(xmin = -0.5, ymin = 48.5, xmax = 2.8, ymax = 51.6),
                    crs = 4326) |>
  st_as_sfc()

eurostar_clipped <- st_intersection(eurostar_lines, clip_box)

# --- 3. Plot ---
library(tidyterra)

ggplot() +
  geom_spatraster_rgb(data = tiles) +
  geom_sf(data = eurostar_clipped, colour = "#d62828", linewidth = 1.2) +
  geom_sf(data = cities, size = 5, colour = "#1d3557") +
  geom_sf_label(data = cities, aes(label = city), nudge_y = 0.3) +
  coord_sf(xlim = c(-0.5, 2.8), ylim = c(48.5, 51.6)) +
  theme_void() +
  labs(title = "London – Paris (Eurostar)")

# ggsave("leg1_london_paris.png", width = 8, height = 8, dpi = 200)
