
rm(list = ls())

library(dplyr)
library(sf)
library(geobr)
library(ggplot2)
library(censobr)
library(viridis)

hs <- read_households(year = 2010,showProgress = FALSE)

muni_sf <- geobr::read_municipality(year = 2020,
                                    showProgress = FALSE)

esg <- hs |> 
  compute() |>
  group_by(code_muni) |>                                             # (a)
  summarize(rede = sum(V0010[which(V0207=='1')]),                    # (b)
            total = sum(V0010)) |>                                   # (b)
   mutate(cobertura = rede / total) |>                                # (c)
  collect() 

muni_sf$code_muni <- as.character(muni_sf$code_muni)

esg_sf <- left_join(muni_sf, esg, by = 'code_muni')


points <- st_sample(esg_sf$geom, size = 1000, type = "hexagonal") 

coords <- st_centroid(esg_sf)  # Obtém as coordenadas do centroide de cada polígono

# Cria os hexbins usando a função hexbin
# hbin <- hexbin(coords$x, coords$y, xbins = 30)  # Ajuste o número de xbins conforme necessário
# 
# # Transforma os dados do hexbin em um data frame
# hdf <- data.frame(hexbin::hcell2xy(hbin), count = hbin@count)

# Renomeia as colunas do data frame do hexbin
colnames(hdf) <- c("x", "y", "count")


coords_df <- st_coordinates(coords$geom)
coords <- cbind(coords, coords_df)


ggplot(coords, aes(x = X, y = Y)) +
  stat_summary_hex(aes(z = cobertura), fun = mean) +
  labs(title = "Share of households connected to a sewage network") +
  scale_fill_distiller(palette = "RdPu", direction = 1,
                       name = 'Share of\nhouseholds',
                       labels = scales::percent) +
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#bfafb2"),  
    text = element_text(color = "black"),  
    plot.title = element_text(color = "black"),  
    plot.caption = element_text(size = 10, color = "black", hjust = .1, vjust = 10)  
  ) +
  labs(
    caption = "Source: censobr | Plot: @SuperEuEmFalta"
  )

ggsave("day3.png", width = 10, height = 8, dpi = 300)

