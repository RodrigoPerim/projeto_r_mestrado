# carregando pacotes
library(tidyverse)
library(geobr)
source("scripts/my_functions.R")


# ver quais colunas existem nos arquivos
colnames(data_set)


# Carregamento, leitura e filtro inicial
data_set <- read_rds("data/data-set-xco2.rds") |>
     filter(xco2 > 0) |>

  glimpse(data_set)


# instalar e novos pacotes
install.packages("rnaturalearth")
install.packages("devtools")
library(sf)
library(ggplot2)
library(rnaturalearth)


# carregar o mapa do Brasil para ser o fundo
brasil <- ne_countries(country = "Brazil", scale = "large", returnclass = "sf")


# definir as coordenadas do seu quadrante estudado
quadrante_coords <- data.frame(
  long = c(-59.7700, -49.8361, -49.8361, -59.7700, -59.7700), # precisa repetir no final o primeiro ponto, na lat e na long
  lat = c(-1.6058, -1.6061, -12.3561, -12.3358, -1.6058)
)

# criação de um objeto sf -> sendo o quadrante
# st_polygon - cria a geometria (dado lat e long antes)
# st_sfc - objeto de geometria simples
# st_sf - data frame com uma coluna de geometria
quadrante <- st_sf(
  geom = st_sfc(st_polygon(list(as.matrix(quadrante_coords)))),
  crs = st_crs(4326)   # (EPSG: 4326)
)


# carrega e converte os dados
dados_brutos <- readRDS("C:/Users/RODRIGO/Desktop/Projeto R mestrado/data/data-set-xco2.rds")


# pré-filtro (um quadrante com buffer de 3 graus maior do que o quadrante)
dados_filtrados_preliminar <- dados_brutos %>%
  dplyr::filter(
    longitude >= min(quadrante_coords$long) - 3,  # 3 é o buffer para o quadrante
    longitude <= max(quadrante_coords$long) + 3,
    latitude >= min(quadrante_coords$lat) - 3,
    latitude <= max(quadrante_coords$lat) + 3
  )


# converte os dados pré-filtrados para um objeto sf (formato para dados geoespaciais)
dados_sf <- st_as_sf(
  dados_filtrados_preliminar,
  coords = c("longitude", "latitude"),
  crs = 4326
)


# filtra os dados que estão no quadrante
dados_no_quadrante <- dados_sf[st_within(dados_sf, quadrante, sparse = FALSE), ]


# plota o mapa com o quadrante
ggplot() +
  # adiciona o mapa do Brasil
  geom_sf(data = brasil, fill = "lightgray", color = "black") +

  # adiciona o quadrante
  geom_sf(data = quadrante, fill = "white", color = "red", alpha = 0.6, size = 1) +

  # adiciona os dados de xcoo2
  # aes() - ligação entre a coluna e a estética do gráfico
  geom_sf(data = dados_no_quadrante, aes(color = xco2), size = 0.5) +

  # adiciona cores
  scale_color_viridis_c(option = "magma") +

  # ajusta os limites do mapa
  coord_sf(xlim = c(-62, -48), ylim = c(-15, 0)) +

  # adiciona títulos
  labs(
    title = "Valores de Concentração de xco₂",
    subtitle = "",
    x = "Longitude",
    y = "Latitude",
    color = "xco2 (ppm)"
  ) +

  # Define um tema visual
  theme_minimal()

# salva o data frame filtrado em um arquivo .rds
saveRDS(dados_no_quadrante, file = "data/dados_xco2_quadrante_filtrados.rds")

# Para carregar o data frame no futuro
#dados_filtrados <- readRDS("data/dados_quadrante_filtrados.rds")
