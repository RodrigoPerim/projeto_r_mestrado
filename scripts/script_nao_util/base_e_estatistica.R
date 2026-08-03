# Carregando pacotes
library(tidyverse)
library(ggridges)
library(ggpubr)
library(geobr)
library(gstat)
library(vegan)

#Carregando polígono do Brasil
country_br <- geobr::read_country(showProgress = FALSE)


# Carregando os dados de xco2
data_set_xco2 <- readr::read_rds("data/data-set-xco2.rds") |>
  dplyr::filter(
    longitude >=-59.7700 & longitude <= -49.8361,
    latitude >=-12.3561 & latitude <= -1.6058
  ) |>
  dplyr::mutate(
    time = lubridate::as_datetime(time, tz = "America/Sao_Paulo"),
    year = lubridate::year(time),
    month = lubridate::month(time),
    day = lubridate::day(time),
  )
dplyr::glimpse(data_set_xco2)
readr::write_rds(data_set_xco2,"data/data-set-xco2-filter.rds")


# Carregando os dados de SIF
data_set_sif <- readr::read_rds("data/data-set-sif.rds") |>
  dplyr::filter(
    longitude >=-59.7700 & longitude <= -49.8361,
    latitude >=-12.3561 & latitude <= -1.6058
  ) |>
  dplyr::mutate(
    time = lubridate::as_datetime(time,
                                  origin = "1990-01-01 00:00:00",
                                  tz = "America/Sao_Paulo"),
    year = lubridate::year(time),
    month = lubridate::month(time),
    day = lubridate::day(time),
  )
dplyr::glimpse(data_set_sif)
readr::write_rds(data_set_sif,"data/data-set-sif-filter.rds")
###
###
###

# Criando coluna de semestre
data_set_xco2 <- data_set_xco2 %>%
  mutate(
    epoca = case_when(
      month %in% 1:6   ~ "Chuvosa. (Jan-Jun)",
      month %in% 7:12  ~ "Seca. (Jul-Dez)"
    )
  )
head(data_set_xco2)
###
###
###
# Visualização de Histograma xco2
data_set_xco2 |>
  dplyr::filter(year <2025) |>
  ggplot2::ggplot(ggplot2::aes(x=xco2)) +
  ggplot2::geom_histogram(color="black",fill="gray",
                          bins = 30) +
  ggplot2::coord_cartesian(xlim = c(395, 435) , ylim = c(0, 70000))+
  ggplot2::facet_wrap(~year, scales = "free") +
  ggplot2::theme_bw()


# Visualização de Histograma SIF
data_set_sif |>
  dplyr::filter(year <2025) |>
  ggplot2::ggplot(ggplot2::aes(x=daily_sif757)) +
  ggplot2::geom_histogram(color="black",fill="gray",
                          bins = 30) +
  ggplot2::coord_cartesian(xlim = c(-2, 2) , ylim = c(0, 350000))+
  ggplot2::facet_wrap(~year, scales = "free") +
  ggplot2::theme_bw()

###
###
###

# Tendencia regional, r2 e plotagem de gráfico
data_set_xco2 |>
 sample_n(10000) |>
 drop_na() |>
 mutate( year = year - min(year)) |>
 ggplot(aes(x=year, y=xco2)) +
 geom_point() +
 geom_point(shape=21,color="black",fill="gray") +
 geom_smooth(method = "lm") +
 stat_regline_equation(aes(
 label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
 theme_bw() +
 labs(x="Ano",y="xco2")


 # Análise da regressão linear simples para caracterização da tendencia XCO2
 mod_trend_xco2 <- lm(xco2 ~ year,
                      data = data_set_xco2 |>
                        filter(xco2_quality_flag == 0) |>
                        drop_na() |>
                        mutate( year = year - min(year))
 )
 mod_trend_xco2

 summary.lm(mod_trend_xco2)

 # retirando a tendencia e separando por quadrimestre, estou retirando a tendencia
 # e substituindo o arquivo que existia com tendencia, para o sem tendencia
 a_co2 <- mod_trend_xco2$coefficients[[1]]
 b_co2 <- mod_trend_xco2$coefficients[[2]]

 data_set_xco2_sem_tendencia <- data_set_xco2 |>
   filter(xco2_quality_flag == 0,
          year >= 2020 & year <= 2025) |>
   mutate(
     year_modif = year -min(year),
     xco2_est = a_co2+b_co2*year_modif,
     delta = xco2_est-xco2,
     xco2_detrend = (a_co2-delta) - (mean(xco2) - a_co2)
   ) |>
   select(-c( time, xco2_quality_flag,xco2_incerteza,
             path,year_modif:delta)) |>
   rename(xco2_trend = xco2,
          xco2 = xco2_detrend) |>
   mutate(
     xco2_anomaly = xco2 - median(xco2, na.rm = TRUE),
     .after = xco2
   ) |>
   ungroup()

 # Plotando gráfico da distribuição de xco2 com tendencia por ano
 # separado por quadrimestre
 data_set_xco2 %>%

   ggplot(aes(x = xco2, y = as.factor(year), fill = epoca)) +

   geom_density_ridges(
     rel_min_height = 0.03,
     alpha = .6,
     color = "black"
   ) +

   scale_fill_viridis_d(name = "Epoca") +
   coord_cartesian(xlim = c(400, 440)) +
   theme_ridges() +

   labs(
     title = "Distribuição Epoca de XCO₂ por Ano",
     x = "Concentração de XCO₂ (ppm)",
     y = "Ano"
   )


 # Plotando gráfico da distribuição de xco2 sem tendencia por ano
 # separado por semestre
 data_set_xco2_sem_tendencia %>%

   ggplot(aes(x = xco2, y = as.factor(year), fill = epoca)) +

   geom_density_ridges(
     rel_min_height = 0.03,
     alpha = .6,
     color = "black"
   ) +

   scale_fill_viridis_d(name = "Epoca") +
   coord_cartesian(xlim = c(400, 420)) +
   theme_ridges() +

   labs(
     title = "Distribuição Epoca de XCO₂ por Ano",
     x = "Concentração de XCO₂ (ppm)",
     y = "Ano"
   )
 ###
 ###
 ###

 # Anomalia de xco2, criando a coluna.

 data_set_xco2_com_tendencia <- data_set_xco2 %>%

   filter(xco2_quality_flag == 0,
          year >= 2020 & year <= 2025) %>%

   group_by(year, epoca) %>%

   mutate(
     xco2_anomaly = xco2 - median(xco2, na.rm = TRUE)
   ) %>%

   ungroup()

 # Anomalia de xco2 com tendencia

 data_set_xco2_com_tendencia %>%
   ggplot(aes(x = xco2_anomaly, y = as.factor(year), fill = epoca)) +

   geom_density_ridges(
     rel_min_height = 0.03,
     alpha = .6,
     color = "black"
   ) +
     geom_vline(xintercept = 0, linetype = "dashed", color = "black") +

     scale_fill_manual(
       name = "Epoca",
       values = c(
         "Chuvosa. (Jan-Jun)" = "blue", # Tom de roxo/lilás
         "Seca. (Jul-Dez)" = "yellow"  # Tom de amarelo claro
       )
     ) +
  coord_cartesian(xlim = c(-5, 5))
  theme_ridges()

##################################
  ################################
  ################################
  # --- Carregue as bibliotecas (se necessário) ---
  library(dplyr)
  library(ggplot2)

  # ===================================================================
  # PASSO 1: PREPARAR OS DADOS (AGORA POR ÉPOCA)
  # ===================================================================
  # Assumindo que seu data frame base se chama 'data_set_xco2_sem_tendencia'

  dados_temporais_epoca <- data_set_xco2_sem_tendencia %>%

    # MUDANÇA 1: Criando a coluna 'epoca' com base no mês
    mutate(
      epoca = case_when(
        month %in% 1:6  ~ "Chuvosa (Jan-Jun)",
        month %in% 7:12 ~ "Seca (Jul-Dez)"
      )
    ) %>%

    # Garante que as épocas estejam em ordem no gráfico
    mutate(
      epoca = factor(epoca, levels = c("Chuvosa (Jan-Jun)", "Seca (Jul-Dez)"))
    ) %>%

    # MUDANÇA 2: Agrupando por 'year' e a nova coluna 'epoca'
    group_by(year, epoca) %>%

    # Calcula a média e o desvio padrão da anomalia para cada grupo
    summarise(
      anomalia_media = mean(xco2_anomaly, na.rm = TRUE),
      desvio_padrao = sd(xco2_anomaly, na.rm = TRUE),
      .groups = "drop"
    ) %>%

    # MUDANÇA 3: Ajustando o mês representativo para as duas épocas
    # Usaremos Março (mês 3) para Chuvosa e Setembro (mês 9) para Seca
    mutate(
      mes_representativo = case_when(
        epoca == "Chuvosa (Jan-Jun)" ~ 3,
        epoca == "Seca (Jul-Dez)"    ~ 9
      ),
      data = as.Date(paste(year, mes_representativo, "15", sep = "-"))
    )

  # Veja o resultado da preparação (agora com 2 linhas por ano)
  print(dados_temporais_epoca)


  #####################################
  ######################################
  ###################################
  # Anomalia de xco2 sem tendencia

  data_set_xco2_sem_tendencia %>%
    ggplot(aes(x = xco2_anomaly, y = as.factor(year), fill = quadrimestre)) +

    geom_density_ridges(
      rel_min_height = 0.03,
      alpha = .6,
      color = "black"
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +

    scale_fill_manual(
      name = "Epoca",
      values = c(
        "Chuvosa. (Jan-Jun)" = "blue", # Tom de roxo/lilás
        "Seca. (Jul-Dez)" = "yellow"  # Tom de amarelo claro
      )
    ) +
    coord_cartesian(xlim = c(-5, 5))
  theme_ridges()
 ###
 ###
 ###

#Estatistica do xco2 - com tendencia

  # --- Carregue as bibliotecas necessárias ---
  library(dplyr)
  library(ggplot2)

  # ===================================================================
  # PASSO 1: PREPARAR OS DADOS (AGORA MENSALMENTE)
  # ===================================================================
  # Assumindo que seu data frame é 'data_set_xco2_sem_tendencia'

  dados_xco2_mensais <- data_set_xco2_com_tendencia %>%

    # MUDANÇA: Agrupando por 'year' e 'month'
    group_by(year, month) %>%

    # Calcula a média e o desvio padrão do XCO2 para cada mês
    summarise(
      xco2_media = mean(xco2, na.rm = TRUE),
      xco2_sd = sd(xco2, na.rm = TRUE),
      .groups = "drop" # Remove o agrupamento ao final
    ) %>%

    # MUDANÇA: A criação da coluna 'data' agora usa o próprio mês
    # Usamos o dia 15 como representante do meio do mês
    mutate(
      data = as.Date(paste(year, month, "15", sep = "-")),

      # Adicionando a coluna 'epoca' apenas para podermos COLORIR o gráfico
      epoca = case_when(
        month %in% 1:6  ~ "Chuvosa (Jan-Jun)",
        month %in% 7:12 ~ "Seca (Jul-Dez)"
      ),
      epoca = factor(epoca, levels = c("Chuvosa (Jan-Jun)", "Seca (Jul-Dez)"))
    ) %>%
    # Ordena por data para garantir que as linhas sejam desenhadas corretamente
    arrange(data)

  # Verifique os dados preparados (agora com até 12 linhas por ano)
  print(head(dados_xco2_mensais))


  # ===================================================================
  # PASSO 2: CRIAR O GRÁFICO DE LINHA MENSAL
  # ===================================================================

  ggplot(dados_xco2_mensais, aes(x = data, y = xco2_media, color = epoca, fill = epoca)) +

    # Área sombreada para o desvio padrão
    geom_ribbon(aes(ymin = xco2_media - xco2_sd, ymax = xco2_media + xco2_sd), alpha = 0.3, color = NA) +

    # Linha da média
    geom_line(linewidth = 1) +

    # Pontos para cada média mensal
    geom_point(size = 2.5) +

    # Define as cores para cada época
    scale_color_manual(values = c("Chuvosa (Jan-Jun)" = "blue", "Seca (Jul-Dez)" = "orange")) +
    scale_fill_manual(values = c("Chuvosa (Jan-Jun)" = "blue", "Seca (Jul-Dez)" = "orange")) +

    # MUDANÇA: Rótulos do eixo X a cada 6 meses para não poluir o gráfico
    scale_x_date(
      breaks = breaks_finais,      # Define os pontos exatos dos rótulos
      date_labels = "%b %Y"        # Formata como "Jan 2021", "Jul 2021", etc.
    ) +
    # Títulos e tema
    labs(
      title = "Série Temporal Mensal de XCO₂ com tendencia",
      subtitle = "Média e Desvio Padrão para cada mês, com cores por época",
      x = "Data",
      y = "Concentração Média de XCO₂ (ppm)",
      color = "Época",
      fill = "Época"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1) # Gira os rótulos do eixo X
    )
 ###############
 ###############
 ############## ###############
  ###############
  ############## ###############
  ###############
  ############## ###############
  ###############
  ############## ###############
  ###############
  ##############

  #Estatistica do xco2 - SEM tendencia

  # --- Carregue as bibliotecas necessárias ---
  library(dplyr)
  library(ggplot2)

  # ===================================================================
  # PASSO 1: PREPARAR OS DADOS (AGORA MENSALMENTE)
  # ===================================================================
  # Assumindo que seu data frame é 'data_set_xco2_sem_tendencia'

  dados_xco2_mensais <- data_set_xco2_sem_tendencia %>%

    # MUDANÇA: Agrupando por 'year' e 'month'
    group_by(year, month) %>%

    # Calcula a média e o desvio padrão do XCO2 para cada mês
    summarise(
      xco2_media = mean(xco2, na.rm = TRUE),
      xco2_sd = sd(xco2, na.rm = TRUE),
      .groups = "drop" # Remove o agrupamento ao final
    ) %>%

    # MUDANÇA: A criação da coluna 'data' agora usa o próprio mês
    # Usamos o dia 15 como representante do meio do mês
    mutate(
      data = as.Date(paste(year, month, "15", sep = "-")),

      # Adicionando a coluna 'epoca' apenas para podermos COLORIR o gráfico
      epoca = case_when(
        month %in% 1:6  ~ "Chuvosa (Jan-Jun)",
        month %in% 7:12 ~ "Seca (Jul-Dez)"
      ),
      epoca = factor(epoca, levels = c("Chuvosa (Jan-Jun)", "Seca (Jul-Dez)"))
    ) %>%
    # Ordena por data para garantir que as linhas sejam desenhadas corretamente
    arrange(data)

  # Verifique os dados preparados (agora com até 12 linhas por ano)
  print(head(dados_xco2_mensais))


  # ===================================================================
  # PASSO 2: CRIAR O GRÁFICO DE LINHA MENSAL
  # ===================================================================

  ggplot(dados_xco2_mensais, aes(x = data, y = xco2_media, color = epoca, fill = epoca)) +

    # Área sombreada para o desvio padrão
    geom_ribbon(aes(ymin = xco2_media - xco2_sd, ymax = xco2_media + xco2_sd), alpha = 0.3, color = NA) +

    # Linha da média
    geom_line(linewidth = 1) +

    # Pontos para cada média mensal
    geom_point(size = 2.5) +

    # Define as cores para cada época
    scale_color_manual(values = c("Chuvosa (Jan-Jun)" = "blue", "Seca (Jul-Dez)" = "orange")) +
    scale_fill_manual(values = c("Chuvosa (Jan-Jun)" = "blue", "Seca (Jul-Dez)" = "orange")) +

    # MUDANÇA: Rótulos do eixo X a cada 6 meses para não poluir o gráfico
    scale_x_date(
      breaks = breaks_finais,      # Define os pontos exatos dos rótulos
      date_labels = "%b %Y"        # Formata como "Jan 2021", "Jul 2021", etc.
    ) +
    # Títulos e tema
    labs(
      title = "Série Temporal Mensal de XCO₂ sem tendencia",
      subtitle = "Média e Desvio Padrão para cada mês, com cores por época",
      x = "Data",
      y = "Concentração Média de XCO₂ (ppm)",
      color = "Época",
      fill = "Época"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1) # Gira os rótulos do eixo X
    )



  ###############
  ###############
  ############## ###############
  ###############
  ############## ###############
  ###############
  ############## ###############
  ###############
  ############## ###############
  ###############
  ##############
  # --- Carregue as bibliotecas necessárias ---
  library(dplyr)
  library(ggplot2)

  # ===================================================================
  # PASSO 1: PREPARAR OS DADOS MENSAIS (Sem alterações aqui)
  # ===================================================================
  # Assumindo que o objeto 'dados_xco2_mensais' já existe
  # Se não, rode o script anterior para criá-lo.


  # ===================================================================
  # PASSO 2: CRIAR OS RÓTULOS (BREAKS) PERSONALIZADOS PARA O EIXO X
  # ===================================================================

  # Encontra o primeiro e o último ano nos seus dados
  primeiro_ano <- min(dados_xco2_mensais$year)
  ultimo_ano <- max(dados_xco2_mensais$year)

  # Cria uma sequência de datas para o dia 1º de Janeiro de cada ano no intervalo
  breaks_jan <- seq.Date(from = as.Date(paste0(primeiro_ano, "-01-01")),
                         to = as.Date(paste0(ultimo_ano, "-01-01")),
                         by = "1 year")

  # Cria uma sequência de datas para o dia 1º de Julho de cada ano no intervalo
  breaks_jul <- seq.Date(from = as.Date(paste0(primeiro_ano, "-07-01")),
                         to = as.Date(paste0(ultimo_ano, "-07-01")),
                         by = "1 year")

  # Combina as duas sequências e ordena para ter a lista final de rótulos
  breaks_finais <- sort(c(breaks_jan, breaks_jul))


  # ===================================================================
  # PASSO 3: CRIAR O GRÁFICO COM OS NOVOS RÓTULOS
  # ===================================================================

  ggplot(dados_xco2_mensais, aes(x = data, y = xco2_media, color = epoca, fill = epoca)) +

    geom_ribbon(aes(ymin = xco2_media - xco2_sd, ymax = xco2_media + xco2_sd), alpha = 0.3, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +

    scale_color_manual(values = c("Chuvosa (Jan-Jun)" = "blue", "Seca (Jul-Dez)" = "orange")) +
    scale_fill_manual(values = c("Chuvosa (Jan-Jun)" = "blue", "Seca (Jul-Dez)" = "orange")) +

    # MUDANÇA AQUI: Usando o vetor 'breaks_finais' que criamos
    scale_x_date(
      breaks = breaks_finais,      # Define os pontos exatos dos rótulos
      date_labels = "%b %Y"        # Formata como "Jan 2021", "Jul 2021", etc.
    ) +

    labs(
      title = "Série Temporal Mensal de XCO₂",
      subtitle = "Média e Desvio Padrão para cada mês, com cores por época",
      x = "Data",
      y = "Concentração Média de XCO₂ (ppm)",
      color = "Época",
      fill = "Época"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1) # Gira os rótulos do eixo X
    )
 ##############
  #############
  #############
# Estatistica da anomalia
  data_set_xco2_sem_tendencia |>
    group_by(year) |>
    summarise(
      N = length(xco2_anomaly),
      MEAN = mean(xco2_anomaly),
      MEDIAN = median(xco2_anomaly),
      STD_DV = sd(xco2_anomaly),
      SKW = agricolae::skewness(xco2_anomaly),
      KRT = agricolae::kurtosis(xco2_anomaly),
    ) |>
    writexl::write_xlsx("docs/estat-desc-anomaly.xlsx")


  data_set_xco2_sem_tendencia %>%


    ggplot(aes(x = as.factor(year), y = xco2_anomaly, fill = epoca)) +


    geom_boxplot(position = position_dodge(width = 0.8), width = 0.7) +

    coord_cartesian(ylim = c(-10, 10)) +

    theme_bw() +
    theme(legend.position = "bottom") +

    scale_fill_manual(
      name = "Epoca",
      values = c(
        "Chuvosa. (Jan-Jun)" = "blue", # Tom de roxo/lilás
        "Seca. (Jul-Dez)" = "yellow"  # Tom de amarelo claro
      )
    ) +

    labs(
      title = "Anomalia por época de XCO₂ por Ano",
      subtitle = "Comparativo entre as épocas dentro de cada ano (dados sem tendência)",
      x = "Ano",
      y = "Anomalia de XCO₂ (ppm)"
    )


###########################################
###########################################
###########################################
###########################################
###########################################

#Análise Geoestatística

  longitude_min <- -59.7700
  longitude_max <- -49.8361

  latitude_min  <- -12.3561
  latitude_max  <- -1.6058

   resolucao_em_graus <- 0.05 # Aprox. 5.5 km

    grid_area_estudo <- expand.grid(
    longitude = seq(longitude_min, longitude_max, by = resolucao_em_graus),
    latitude  = seq(latitude_min, latitude_max, by = resolucao_em_graus)
  )

  head(grid_area_estudo)

  # --- Carregue os pacotes necessários (instale com install.packages("sf"), etc.) ---
  library(sf)
  library(geobr)

  # ===================================================================
  # PASSO A: CONVERTER SUA GRADE EM UM OBJETO ESPACIAL
  # ===================================================================
  # O CRS 4326 é o sistema de coordenadas padrão (WGS84) para lat/lon
  grid_sf <- st_as_sf(grid_area_estudo,
                      coords = c("longitude", "latitude"),
                      crs = 4326)

  # ===================================================================
  # PASSO B: BAIXAR E AJUSTAR OS DADOS DOS MUNICÍPIOS
  # ===================================================================
  # O pacote 'geobr' facilita o acesso a dados oficiais do Brasil
  municipios_sf_original <- read_municipality(year = 2020) %>%
    filter(abbrev_state %in% c("AM", "PA", "MT", "RO")) # Ajuste os estados conforme sua área

  # ----- LINHA ADICIONADA AQUI PARA CORRIGIR O ERRO -----
  # Transforma o CRS dos municípios para ser IGUAL ao da sua grade (4326)
  municipios_sf <- st_transform(municipios_sf_original, crs = 4326)
  # ----------------------------------------------------

  # ===================================================================
  # PASSO C: FAZER A JUNÇÃO ESPACIAL (O "CRUZAMENTO")
  # ===================================================================
  # Agora que os CRS são iguais, esta linha vai funcionar
  grid_com_municipios <- st_join(grid_sf, municipios_sf)

  # Veja o resultado: agora sua grade tem as colunas com informações do município!
  head(grid_com_municipios)

  # --- Carregue os pacotes (se já não estiverem carregados) ---
  library(sf)
  library(dplyr)

  # Somente execute este bloco se o comando acima retornou um número maior que 0

  # A. Separe os pontos com e sem município
  pontos_com_municipio <- grid_com_municipios %>% filter(!is.na(name_muni))
  refugos_sf <- grid_com_municipios %>% filter(is.na(name_muni))

  # B. Encontre o ponto mais próximo para cada "refugo"
  # A função st_nearest_feature é extremamente rápida para isso
  indices_proximos <- st_nearest_feature(refugos_sf, pontos_com_municipio)

  # C. Atribua o nome do município do ponto mais próximo ao "refugo"
  nomes_municipios_proximos <- pontos_com_municipio$name_muni[indices_proximos]
  refugos_sf$name_muni <- nomes_municipios_proximos

  # D. Junte tudo de volta em um único arquivo completo
  grid_final_completo <- rbind(pontos_com_municipio, refugos_sf)

  # Verifique se ainda há algum NA (deve retornar 0)
  sum(is.na(grid_final_completo$name_muni))

  library(ggplot2)
  ggplot(grid_final_completo) +
    geom_sf(aes(color = name_muni), size = 0.5) + # geom_sf é próprio para objetos 'sf'
    theme_minimal() +
    theme(legend.position = "none") # Remove a legenda para um mapa mais limpo

  ggplot() +

    # Camada 1: Desenha o mapa dos municípios no fundo
    # Usamos 'municipios_sf' como a base do mapa
    geom_sf(
      data = municipios_sf,
      fill = "gray90",      # Cor de preenchimento dos municípios
      color = "white",      # Cor das bordas
      size = 0.1
    ) +

    # Camada 2: Desenha uma amostra dos pontos da sua grade por cima
    # Usamos 'grid_final_completo' e pegamos uma amostra para não poluir o gráfico
    geom_sf(
      data = grid_final_completo %>% sample_n(5000), # Amostra de 2000 pontos
      color = "red",
      size = 0.1
    ) +

    # Estilo e Títulos
    theme_minimal() +
    labs(
      title = "Verificação da Grade de Pontos sobre os Municípios",
      subtitle = "Amostra de pontos da grade (vermelho) sobre o mapa da área de estudo",
      x = "Longitude",
      y = "Latitude"
    )

  # --- Carregue os pacotes (se necessário) ---
  library(sf)
  library(terra)

  # --- Garanta que seus objetos existam ---
  # grid_final_completo <- readRDS("dados/grid_final_com_municipios.rds")
  # resolucao_em_graus <- 0.05 # A mesma resolução usada para criar a grade

  # ===================================================================
  # CONVERSÃO PARA RASTER USANDO SEU OBJETO FINAL
  # ===================================================================
  # A função rast() do pacote 'terra' pode usar o objeto 'sf' diretamente
  # para definir a extensão (limites) da grade.

  grid_raster <- rast(
    grid_final_completo,
    resolution = resolucao_em_graus,
    crs = "EPSG:4326" # Garante que o CRS esteja correto
  )
  # Você terá um objeto da classe 'SpatRaster' com a geometria correta
  print(grid_raster)

  ##############################################
  ##############################################
  ##############################################

  # --- Carregue os pacotes necessários ---
  library(sf)
  library(dplyr)

  # ===================================================================
  # PASSO 1: PREPARAR OS DADOS
  # ===================================================================

  # Assumindo que você já tem:
  # - 'data_set_xco2': Seus dados de satélite brutos com colunas 'longitude', 'latitude', 'xco2', 'year', 'month'.
  # - 'grid_area_estudo': Sua grade regular com colunas 'longitude' e 'latitude'.

  # --- Converta ambos para objetos espaciais 'sf' (com o mesmo CRS) ---

  # Dados de Satélite
  satelite_sf <- st_as_sf(data_set_xco2,
                          coords = c("longitude", "latitude"),
                          crs = 4326) %>%
    # Adiciona a coluna quadrimestre que vamos usar para agrupar
    mutate(
      quadrimestre = case_when(
        month %in% 1:4   ~ "1º Quad. (Jan-Abr)",
        month %in% 5:8   ~ "2º Quad. (Mai-Ago)",
        month %in% 9:12  ~ "3º Quad. (Set-Dez)"
      )
    )

  # Grade Regular
  grid_sf <- st_as_sf(grid_area_estudo,
                      coords = c("longitude", "latitude"),
                      crs = 4326) %>%
    # Adiciona um ID único para cada ponto da grade, o que facilitará o agrupamento
    mutate(id_ponto_grid = 1:n())


  # ===================================================================
  # PASSO 2: JUNTAR CADA PONTO DE SATÉLITE AO PONTO MAIS PRÓXIMO DA GRADE
  # ===================================================================
  # A função st_join com st_nearest_feature faz isso de forma otimizada
  # Esta única linha substitui todo o 'for' loop
  dados_juntos <- st_join(satelite_sf, grid_sf, join = st_nearest_feature)


  # ===================================================================
  # PASSO 3: CALCULAR A MÉDIA DE XCO2 PARA CADA PONTO DA GRADE E PERÍODO
  # ===================================================================
  # Agora, agrupamos pelo ID do ponto da grade e pelo período de tempo
  grid_com_valores <- dados_juntos %>%
    # Remove a geometria para fazer um cálculo de tabela mais rápido
    st_drop_geometry() %>%

    group_by(id_ponto_grid, year, quadrimestre) %>%

    summarise(
      xco2_medio = mean(xco2, na.rm = TRUE),
      .groups = "drop" # Remove o agrupamento ao final
    )

  # ===================================================================
  # PASSO 4: JUNTAR OS VALORES MÉDIOS DE VOLTA À GRADE ESPACIAL
  # ===================================================================
  # O resultado final é a sua grade, agora com os valores de XCO2 reamostrados
  grid_final_reamostrado <- grid_sf %>%
    left_join(grid_com_valores, by = "id_ponto_grid")


  # Veja o resultado final!
  head(grid_final_reamostrado)


  ##############################################
  ##############################################
  ##############################################

  # ===================================================================
  # PASSO 1: PREPARAR OS DADOS (COM OS VALORES CORRIGIDOS)
  # ===================================================================
  # AQUI VOCÊ EDITA: Use um ano e um nome de quadrimestre que existem nos seus dados
  ano_escolhido <- 2021 # <-- CONFIRME SE ESTE ANO EXISTE
  quadrimestre_escolhido <- "2º Quad. (Mai-Ago)" # <-- CONFIRME SE ESTE NOME ESTÁ EXATO

  # Filtra os dados para o período selecionado
  dados_para_variograma <- grid_final_reamostrado %>%
    filter(
      year == ano_escolhido,
      quadrimestre == quadrimestre_escolhido,
      !is.na(xco2_medio)
    )

  # ===================================================================
  # PASSO 2: VERIFICAÇÃO (IMPORTANTE!)
  # ===================================================================
  # Se este número for 0, o erro acontecerá. Se for maior que 0, deve funcionar.
  if (nrow(dados_para_variograma) == 0) {
    stop("ERRO: Nenhum dado encontrado para o ano e quadrimestre selecionados. Verifique os filtros.")
  }
  # PASSO 3: CALCULAR E PLOTAR O VARIOGRAMA (VERSÃO AJUSTADA)
  # ===================================================================
  formula_vgm <- xco2_medio ~ 1

  # MUDANÇA 1: Aumentando o cutoff para 10, para calcular o variograma até essa distância
  variograma_experimental <- gstat::variogram(
    formula_vgm,
    data = dados_para_variograma,
    cutoff = 8,  # <-- MUDADO AQUI PARA CALCULAR ATÉ 10
    width = 0.8   # Mantendo uma largura de lag razoável
  )

  # Inspecione o resultado para garantir que foi calculado corretamente
  print(variograma_experimental)

  # ===================================================================
  # PASSO 3: PLOTAR O GRÁFICO (COM NOVOS LIMITES)
  # ===================================================================
  ggplot(variograma_experimental, aes(x = dist, y = gamma)) +
    geom_point(size = 3, color = "darkblue") +

    # MUDANÇA 2: Adicionando coord_cartesian para definir os limites visuais dos eixos
    coord_cartesian(
      xlim = c(0, 10), # Limite do eixo X vai de 0 a 10
      ylim = c(0, 6)   # Limite do eixo Y vai de 0 a 6
    ) +

    labs(
      title = paste("Variograma para", quadrimestre_escolhido, "de", ano_escolhido),
      x = "Distância (graus decimais)",
      y = expression(paste("Semivariância (", gamma, ")"))
    ) +
    theme_bw()

  ##############################################
  ##############################################
  ##############################################

  # --- Carregue os pacotes necessários ---
  library(sf)
  library(gstat)
  library(ggplot2)
  library(dplyr)

  # ===================================================================
  # PASSO 1: AJUSTAR OS MODELOS TEÓRICOS
  # ===================================================================
  # Assumindo que 'variograma_experimental' foi criado no passo anterior

  # Vamos pegar estimativas iniciais a partir do nosso próprio variograma
  # Isso ajuda o algoritmo de ajuste a encontrar um bom resultado
  epepita_inicial <- min(variograma_experimental$gamma)
  patamar_inicial <- max(variograma_experimental$gamma)
  alcance_inicial <- median(variograma_experimental$dist) # Uma estimativa simples

  # Ajusta os três modelos mais comuns
  # A função fit.variogram encontrará os melhores parâmetros para cada modelo
  modelo_esferico <- fit.variogram(variograma_experimental,
                                   vgm(psill = patamar_inicial, "Sph", range = alcance_inicial, nugget = epepita_inicial))

  modelo_exponencial <- fit.variogram(variograma_experimental,
                                      vgm(psill = patamar_inicial, "Exp", range = alcance_inicial, nugget = epepita_inicial))

  modelo_gaussiano <- fit.variogram(variograma_experimental,
                                    vgm(psill = patamar_inicial, "Gau", range = alcance_inicial, nugget = epepita_inicial))

  # Imprime os parâmetros encontrados para cada modelo para sua referência
  print(modelo_esferico)
  print(modelo_exponencial)
  print(modelo_gaussiano)


  # ===================================================================
  # PASSO 2: VISUALIZAR E COMPARAR OS MODELOS (Substituindo 'plot_my_models')
  # ===================================================================

  # A função variogramLine cria os dados para a curva suave de cada modelo
  linha_esferico <- variogramLine(modelo_esferico, maxdist = max(variograma_experimental$dist))
  linha_exponencial <- variogramLine(modelo_exponencial, maxdist = max(variograma_experimental$dist))
  linha_gaussiano <- variogramLine(modelo_gaussiano, maxdist = max(variograma_experimental$dist))

  # Criando o gráfico com ggplot2
  ggplot(variograma_experimental, aes(x = dist, y = gamma)) +
    # 1. Plota os pontos do seu variograma experimental
    geom_point(size = 3, color = "darkblue") +

    # 2. Plota a linha do modelo esférico
    geom_line(data = linha_esferico, aes(color = "Esférico"), linewidth = 1) +

    # 3. Plota a linha do modelo exponencial
    geom_line(data = linha_exponencial, aes(color = "Exponencial"), linewidth = 1) +

    # 4. Plota a linha do modelo gaussiano
    geom_line(data = linha_gaussiano, aes(color = "Gaussiano"), linewidth = 1) +

    # 5. Estilo e legendas
    scale_color_manual(name = "Modelos", values = c("Esférico" = "red", "Exponencial" = "green3", "Gaussiano" = "purple")) +
    labs(
      title = "Ajuste de Modelos ao Variograma Experimental",
      x = "Distância (graus decimais)",
      y = expression(paste("Semivariância (", gamma, ")"))
    ) +
    theme_bw()



  ##################################
  ##################################
  ###################################



  # --- Carregue os pacotes necessários ---
  library(sf)
  library(gstat)
  library(dplyr)
  library(ggplot2)

  # ===================================================================
  # PASSO 1: CRIAR UMA AMOSTRA ALEATÓRIA DE 1000 PONTOS
  # ===================================================================
  # Assumindo que 'dados_para_variograma' é seu dataset completo para o período

  # Verifique se você tem pelo menos 1000 pontos para amostrar
  if (nrow(dados_para_variograma) < 500) {
    stop("ERRO: O conjunto de dados tem menos de 1000 pontos para amostrar.")
  }

  # Cria o novo data frame com a amostra
  dados_validacao_500 <- dados_para_variograma %>%
    sample_n(500)

  cat("Amostra de 500 pontos criada com sucesso!\n")


  # ===================================================================
  # PASSO 2: RODAR A VALIDAÇÃO CRUZADA NA AMOSTRA
  # ===================================================================
  # Usaremos 'dados_validacao_1000' como nossa fonte de dados

  cat("Iniciando validação cruzada na amostra (será bem mais rápido)...\n")

  # nfold = 1000 para fazer o "Leave-One-Out" na amostra
  cv_esferico_500 <- krige.cv(formula_vgm, dados_validacao_500, model = modelo_esferico, nfold = 500)
  cv_exponencial_500 <- krige.cv(formula_vgm, dados_validacao_500, model = modelo_exponencial, nfold = 500)
  cv_gaussiano_500 <- krige.cv(formula_vgm, dados_validacao_500, model = modelo_gaussiano, nfold = 500)

  cat("Validação cruzada na amostra concluída!\n")


  # ===================================================================
  # PASSO 3: CALCULAR AS ESTATÍSTICAS DE VALIDAÇÃO E COMPARAR
  # ===================================================================

  # A função para calcular as métricas continua a mesma
  calcular_stats_cv <- function(cv_output, nome_modelo) {
    obs <- cv_output$observed
    est <- cv_output$var1.pred
    RMSE <- sqrt(mean((obs - est)^2))
    modelo_reg <- lm(obs ~ est)
    R2 <- summary(modelo_reg)$r.squared
    Intercepto <- coef(modelo_reg)[1]
    Inclinacao <- coef(modelo_reg)[2]
    data.frame(Modelo = nome_modelo, RMSE = RMSE, R_quadrado = R2, Intercepto_a = Intercepto, Inclinacao_b = Inclinacao)
  }

  # Aplica a função para cada resultado da validação na amostra
  stats_esferico <- calcular_stats_cv(cv_esferico_500, "Esférico")
  stats_exponencial <- calcular_stats_cv(cv_exponencial_500, "Exponencial")
  stats_gaussiano <- calcular_stats_cv(cv_gaussiano_500, "Gaussiano")

  # Junta tudo em uma única tabela e ordena pelo melhor (menor RMSE)
  tabela_validacao_500 <- bind_rows(stats_esferico, stats_exponencial, stats_gaussiano) %>%
    arrange(RMSE)

  cat("\n### Tabela de Validação dos Modelos (Amostra de 1000 pontos) ###\n")
  print(tabela_validacao_500, digits = 3)


  # ===================================================================
  # PASSO 4: VISUALIZAR A COMPARAÇÃO OBSERVADO VS. PREVISTO
  # ===================================================================
  # Junta os resultados para plotar com ggplot2
  plot_data_500 <- bind_rows(
    as.data.frame(cv_esferico_500) %>% mutate(Modelo = "Esférico"),
    as.data.frame(cv_exponencial_500) %>% mutate(Modelo = "Exponencial"),
    as.data.frame(cv_gaussiano_500) %>% mutate(Modelo = "Gaussiano")
  )

  ggplot(plot_data_500, aes(x = var1.pred, y = observed)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    facet_wrap(~Modelo) +
    labs(
      title = "Validação Cruzada (Amostra de 1000 Pontos): Observado vs. Previsto",
      x = "Valor Previsto (Estimado)",
      y = "Valor Real (Observado)"
    ) +
    theme_bw() +
    coord_equal()
