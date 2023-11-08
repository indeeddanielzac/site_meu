####################################################
#             Rivers in Brazil
#             Daniel Oliveira Zacarias
#             02/11/2023
#
####################################################


library(dplyr)
library(sf)
#library(giscoR)
library(ggplot2)


# Downloading the shapefile

library(httr)
library(XML)
library(stringr)


url <- paste0("https://download.geofabrik.de/south-america/brazil.html")

res <- httr::GET(url)

parse <- XML::htmlParse(res)

links <- XML::xpathSApply(parse, path = "//a", XML::xmlAttrs)

hrefs <- sapply(links, function(link) link[['href']])

brazil_links <- hrefs[grepl("latest-free.shp.zip", hrefs)]

brazil_links <- gsub(".htmlbrazil", "", brazil_links)

print(brazil_links)

brazil_links <- paste0("https://download.geofabrik.de/south-america/", brazil_links)


for (a in brazil_links) {
  download.file(a, destfile = basename(a), mode = "wb")
}



out_dir <- "30DaysMapChallenge/day2/unzipped_centro_oeste_osm"

water_filer <- list.files(path = out_dir, pattern = "\\.shp$", full.names = TRUE)

water_list <- lapply(water_filer, sf::st_read)

centro_oeste_water <- do.call(rbind, water_list)

co_water <- centro_oeste_water |>
  count(fclass == 'water') 


# waterway_files <- list.files(path = out_dir, pattern = "\\.shp$", full.names = TRUE)
# 
# waterway_list <- lapply(waterway_files, sf::st_read)
# 
# centro_oeste_waterway <- do.call(rbind, waterway_list)
# 
# centro_oesteIstreams <- centro_oeste_waterway |> 
#   filter(fclass == "stream")


states <- geobr::read_state(
  year = 2020, 
  showProgress = FALSE
)|> 
  filter(name_region == "Centro Oeste")


ggplot() +
  geom_sf(
  data = states,
  fill = "transparent", color = "#0FAAB8", size = .1) +
  geom_sf(
    data = co_water,
    color = "#B82178", size = .05, fill = "#B82178",
    alpha = .45
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.caption = element_text(size = 10, color = "grey90", hjust = .1, vjust = 10),
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA)) +
  labs(
    title = "",
    subtitle = "",
    caption = "River Map of Central-West Region of Brazil | \nSource: OpenStreetMap | Plot: @SuperEuEmFalta",
    x = "",
    y = ""
  )
    
ggsave("day2.png", width = 10, height = 8, dpi = 300)












