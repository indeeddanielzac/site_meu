rm(list = ls())


library(dplyr)
library(sf)
library(rworldmap)
library(ggplot2)
library(gganimate)


map_country <- function(country, x_limits = NULL, y_limits = NULL){
  ## Verifying the arguments passed to the function
  if(!is.character(country)) stop("Name of the country should be character")
  if(length(country) != 1) stop("Function supports only one country per map")
  ## Load libraries
  require(maps)
  require(ggplot2)
  if(!country %in% map_data('world')$region) stop('Country name not recognized\nTo see a list of recognized countries run <unique(maps::map_data("world")$region)>')
  ## If coords limits missing, print worldwide map with coordinates system to allow
  ## User observe coords for reference
  if(missing(x_limits) || missing(y_limits)) {
    warning("X and/or Y limits not provided.\nPrinting worldwide map.")
    map_country_theme <- theme(panel.background = element_rect(fill = '#4e91d2'))
  }
  else {
    if(length(x_limits) != 2 || length(y_limits) != 2 ||
       !all(grepl('^-?[0-9.]+$', c(x_limits, y_limits)))){
      stop("Limits for X and Y coords should be provided as vectors with two numeric values")
    }
    else {
      ## All the received inputs are correct.
      ## Let's define our custom theme for the final map 
      map_country_theme <- theme_bw() +
        theme(panel.background = element_rect(fill = '#4e91d2'),
              legend.position = 'none',
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }
  }
  ## make a df with only the country to overlap
  map_data_country <- map_data('world')[map_data('world')$region == country,]
  ## The map (maps + ggplot2 )
  ggplot() +
    ## First layer: worldwide map
    geom_polygon(data = map_data("world"),
                 aes(x=long, y=lat, group = group),
                 color = '#9c9c9c', fill = '#f3f3f3') +
    ## Second layer: Country map
    geom_polygon(data = map_data_country,
                 aes(x=long, y=lat, group = group),
                 color = '#4d696e', fill = '#8caeb4') +
    coord_map() +
    coord_fixed(1.3,
                xlim = x_limits,
                ylim = y_limits) +
    ggtitle(paste0("A map of ", country)) +
    scale_x_continuous(n.breaks = 20) +
    scale_y_continuous(n.breaks = 20) +
    map_country_theme
}

paises <- map_country("Brazil", x_limits = c(-74, -34), y_limits = c(-35, 6))



# -75.480473,-34.833893,-23.449223,2.336548

xmin <- -75.480473
ymin <- -34.833893
xmax <- -23.449223
ymax <- 2.336548

area_coords <- c(xmin, ymin, xmax, ymax)

area <- paste(
  area_coords,
  sep = ",",
  collapse = ","
)


main_url <- "https://firms.modaps.eosdis.nasa.gov/api/area/csv"
map_key <- "YOUR KEY"
source <- "VIIRS_SNPP_NRT"
day_range <- 10
date <- Sys.Date() - 11

url <- paste(
  main_url,
  map_key,
  source,
  area,
  day_range,
  date,
  sep = "/"
)


fire_data <- data.table::fread(url)

fire_data$datum <- as.Date(fire_data$acq_date)

range(fire_data$acq_date
      )

tema <- function(){
  theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(
        size = 12, color = "grey10"
      ),
      legend.text = element_text(
        size = 11, color = "grey10"
      ),
      plot.title = element_text(
        size = 16, color = "grey10",
        hjust = .5
      ),
      plot.subtitle = element_text(
        face = "bold",
        size = 24, color = "firebrick",
        hjust = .5
      ),
      plot.caption = element_text(
        size = 10, color = "grey30",
        hjust = .5, vjust = 0
      ),
      plot.margin = unit(
        c(t = -2, r = 0, b = -5, l = .1),
        "lines"
      )
    )
}

p <- paises +
  geom_point(data = fire_data,
             aes(x = longitude,
                 y = latitude,
                 color = bright_ti5 - 273.15,
                 group = datum),
             inherit.aes = FALSE)+
  scale_color_gradientn(
    name="Brightnesss\nTemperature\n(°C)",
    colors =rev(hcl.colors(6, "Inferno"))
  ) +
  tema() +
  labs(
    title = "Focos de incêndio no Brasil e Arredores",
    subtitle = "{as.Date(frame_time)}",
    caption = "Fonte: NASA FIRMS"
  ) 



timelapse_map <- p + gganimate::transition_time(as.Date(datum)) +
  gganimate::shadow_mark() +
  gganimate::enter_fade() +
  gganimate::exit_fade() +
  gganimate::ease_aes(
    "cubic-in-out",
    interval = .25
  )



animated_map <- gganimate::animate(
  timelapse_map,
  nframes = 60,  # Reduzindo o número de frames
  duration = 20,
  start_pause = 3,
  end_pause = 30,
  width = 5,  # Reduzindo a largura
  height = 5,  # Reduzindo a altura
  units = "in",
  res = 150,  # Diminuindo a resolução
  fps = 12,
  renderer = gifski_renderer(
    loop = T
  )
)


gganimate::anim_save(
  "day4.gif",
  animation = animated_map,
  path = getwd(),
  overwrite = TRUE
)




