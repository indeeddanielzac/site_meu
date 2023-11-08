
rm(list = ls())

library(dplyr)
library(sf)
library(geobr)
library(ggplot2)

# Download dos dados

meso_sf <- geobr::read_meso_region(year = 2020,
                                    showProgress = FALSE)

ggplot() +
  geom_sf(data=meso_sf, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municipalities of Rio de Janeiro, 2000", size=8) +
  theme_minimal()



dados_caps <- readr::read_csv("caps.csv") |> 
  janitor::clean_names() |> 
  mutate(municipio = stringr::str_to_lower(municipio))

codigo_uf <- data.frame(
  codigo_uf = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
  uf = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")
)

# Juntando as tabelas para adicionar o código UF à base dados_caps
dados_caps <- merge(dados_caps, codigo_uf, by = "uf", all.x = TRUE) |> 
  as_tibble() |> 
  select(-x1, -endereco, -estabelecimento)




dados_lat_lon <- readr::read_csv("municipios.csv") |> 
  janitor::clean_names()   |> 
  mutate(nome = stringr::str_to_lower(nome)) |> 
  rename(municipio = nome)

dados_combinados <- left_join(dados_caps, dados_lat_lon, by = c("codigo_uf", "municipio"))


library(viridis)

ggplot(dados_combinados) +
  geom_sf(data=meso_sf, fill="#c5d1b4", color="#ddfcb9", size=.15, show.legend = FALSE) +
  geom_point(aes(x = longitude, y = latitude, color = tipo), show.legend = FALSE, size = 1) +
  labs(title="Centros de Atenção Psicossocial (CAPS) por Localização Geográfica", size=14,
       x = "", y ="")+
  scale_color_viridis(discrete = TRUE, option = "viridis") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#988E6D"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(face = "bold"))+
  facet_wrap(~tipo)


ggsave("day1.png", width = 10, height = 8, dpi = 300)


