# Projeto Mestrado Rodrigo Perim
## Carregando Pacotes
library(tidyverse)
library(ncdf4)
library(geobr)
source("R/my_functions.R")

## Ler um arquivo de dados `.NC`
my_year <- dir(path = "data-raw/")

# Buscar os nomes dos arquivo .nc4
list_nc4 <- list.files("data-raw/",
           recursive = TRUE,
           full.names = TRUE,
           pattern = "\\.nc4$")

list_nc4_xco2 <- grep("OCO2/|OCO3/", list_nc4, value = TRUE)
list_nc4_sif <- grep("OCO2 SIF/|OCO3 SIF/", list_nc4, value = TRUE)
length(list_nc4_xco2)
length(list_nc4_sif)

# Abrindo o1 arquivo
nc_obj <- nc_open(list_nc4[1])

# buscando os nomes dos atributos no arquivo
names(nc_obj[['var']])


## Função para ler um arquivo e extrair colunas de interesse
## Vamos ler todos os arquivos na pasta
ncdf_reader <- function(path){
  id_stl <- stringr::str_split(path,"/",simplify = TRUE)[,3]
  if(id_stl == "OCO2" | id_stl == "OCO3") {
    df_aux <- get_oco2_xco2(path)
    }else(
    df_aux <- get_oco2_sif(path)
    )
  return(df_aux)
}


## Usando multisession para deixar mais rápido
future::plan("multisession")

# Usando a função map do dplyr
data_set_xco2 <- furrr::future_map_dfr(list_nc4_xco2,ncdf_reader)
data_set_xco2 |> write_rds(paste0("data/","data-set-xco2-",my_year,".rds"))

data_set_sif <- furrr::future_map_dfr(list_nc4_sif,ncdf_reader)
data_set_sif |> write_rds(paste0("data/","data-set-sif-",my_year,".rds"))

## Resumo dos dados
data_set <- data_set |>
  mutate(
    time_1 = lubridate::as_datetime(time, tz = "America/Sao_Paulo"),
    time = lubridate::date(time),
  )
glimpse(data_set)


## Retirando 500 obs do banco de dados e plotando
data_set_xco2 |>
  sample_n(500) |>
  ggplot(aes(x=longitude, y=latitude)) +
  geom_point()

## Então vamos plotar dentro do mapa do Brasil
## Pegando o contorno do BR
br <- read_country(showProgress = FALSE)

# fazer o plot do BR
br |>
  ggplot() +
  geom_sf(fill="white", color="#FEBF57",
          size=.15, show.legend = FALSE) +
  geom_point(data= data_set_xco2 |>
               sample_n(500) ,
             aes(x=longitude, y=latitude),
             shape=3,
             col="red",
             alpha=0.2)
## Compilar os arquivos
list_of_batch <- list.files("data/",all.files = TRUE,full.names = TRUE)
list_of_sif_rds <- grep("sif",list_of_batch,value = TRUE)
list_of_xco2_rds <- grep("xco2",list_of_batch,value = TRUE)

data_set_xco2 <- map_df(list_of_xco2_rds,read_rds)
data_set_sif <- map_df(list_of_sif_rds,read_rds)

write_rds(data_set_xco2,"data/data-set-xco2.rds")
write_rds(data_set_sif,"data/data-set-sif.rds")



