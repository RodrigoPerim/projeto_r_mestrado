#Carregando o polígono do Brasil
country_br <- geobr::read_country(showProgress = FALSE)

#Carregando os dados
#XC02
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

#SIF
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
# Carrega as bibliotecas necessárias
library(dplyr)
library(tidyr) # <-- ADICIONE ESTA LINHA
# 1. Preparação dos dados
dados_limpos <- data_set_xco2 |>
  filter(xco2_quality_flag == 0) |>
  drop_na() |>
  mutate(ano_ajustado = year - min(year)) # Usando um nome diferente para clareza

# 2. Ajuste do modelo de regressão linear
mod_trend_xco2 <- lm(xco2 ~ ano_ajustado, data = dados_limpos)

# 3. Exibição dos resultados detalhados
summary(mod_trend_xco2)
###
###
###

# Certifique-se de que o tidyverse está carregado
library(tidyverse)

# Carregue seus dados, se ainda não o fez
# dados <- readRDS("data_set_xco2.rds") # Use o nome correto do seu objeto
# Para o exemplo, vou usar o nome "data_set_xco2"

# Cria o modelo de regressão para obter a tendência
mod_trend_xco2 <- lm(xco2 ~ year,
                     data = data_set_xco2 |>
                       # Se você tiver uma coluna de flag de qualidade, filtre aqui
                       # filter(xco2_quality_flag == 0) |>
                       drop_na(xco2, year) |> # Remove NAs apenas das colunas essenciais
                       mutate(year = year - min(year))
)

# Visualiza o resumo do modelo (opcional, mas recomendado)
summary(mod_trend_xco2)

# 1. Extrai os coeficientes do modelo que acabamos de criar
a_co2 <- mod_trend_xco2$coefficients[[1]] # Intercepto
b_co2 <- mod_trend_xco2$coefficients[[2]] # Inclinação (tendência anual)

# 2. Aplica o pipeline completo de processamento
dados_processados <- data_set_xco2 |>
  # Filtra os dados (adapte os anos para o seu intervalo de interesse)
  # filter(xco2_quality_flag == 0) |> # Adicione se tiver flag de qualidade
  filter(year >= 2020 & year <= 2024) |> # Exemplo de filtro de ano

  # Calcula a tendência, os resíduos e o valor "detrend"
  mutate(
    year_modif = year - min(year),
    xco2_est = a_co2 + b_co2 * year_modif, # Valor estimado pela tendência
    delta = xco2_est - xco2, # Resíduo (Estimado - Observado)
    xco2_detrend = (a_co2 - delta) - (mean(xco2, na.rm=TRUE) - a_co2) # Detrending
  ) |>

  # Remove colunas intermediárias e desnecessárias
  # Adapte esta lista para as colunas do SEU arquivo que não precisa mais
  select(-c(year_modif:delta)) |> # Remove as colunas que acabamos de criar

  # Renomeia as colunas para guardar o valor original e usar o detrend
  rename(
    xco2_trend = xco2,
    xco2 = xco2_detrend
  ) |>

  # Cria as variáveis de estação (rainy/dry) e ano sazonal
  mutate(
    season = ifelse(month <= 2 | month >= 9, "rainy", "dry"),
    season_year = ifelse(month <= 2,
                         paste0("rainy_", year - 1, ":", year),
                         paste0("rainy_", year, ":", year + 1)),
    season_year = ifelse(season == "dry",
                         paste0("dry_", year),
                         season_year),
    epoch = str_remove(season_year, "dry_|rainy_")
  ) |>

  # Remove anos sazonais incompletos nas pontas da série (ajuste se necessário)
  # filter(epoch != "2019:2020" & epoch != "2024:2025") |>

  # Calcula a anomalia em relação à mediana de cada ano sazonal
  group_by(season_year) |>
  mutate(
    xco2_anomaly = xco2 - median(xco2, na.rm = TRUE),
    .after = xco2
  ) |>
  ungroup()

# 3. Visualize o resultado final
print(head(dados_processados))

# --- Carregue as bibliotecas (se necessário) ---
library(tidyverse)
library(ggridges)

# --- Garanta que o objeto 'dados_brutos_xco2' e os coeficientes do modelo existam ---
# (Estes vêm dos Passos 2 e 3 da resposta completa)

# --- PASSO ÚNICO: Processamento e Visualização ---
dados-set-xco2 %>%

  # 1. Filtra para o intervalo de anos de interesse
  filter(year >= 2020 & year <= 2024) %>%

  # 2. Calcula a tendência, resíduos e o valor "detrend"
  mutate(
    ano_normalizado = year - min(year),
    xco2_estimado_tendencia = intercepto_tendencia + inclinacao_anual * ano_normalizado,
    residuo = xco2 - xco2_estimado_tendencia,
    xco2_sem_tendencia = mean(xco2, na.rm = TRUE) + residuo
  ) %>%

  # 3. Renomeia as colunas
  rename(
    xco2_original_com_tendencia = xco2,
    xco2 = xco2_sem_tendencia
  ) %>%

  # 4. Cria as variáveis de estação e o "ano sazonal"
  mutate(
    estacao = case_when(
      month %in% 6:11 ~ "Seca (Jun-Nov)",
      TRUE ~ "Chuvosa (Dez-Mai)"
    ),
    ano_sazonal = case_when(
      month >= 6 ~ paste0(year, "/", year + 1),
      month <= 5 ~ paste0(year - 1, "/", year)
    )
  ) %>%

  # 5. Calcula a anomalia (agora o data frame está completo)
  group_by(ano_sazonal) %>%
  mutate(
    xco2_anomalia = xco2 - median(xco2, na.rm = TRUE)
  ) %>%
  ungroup() %>%

  # -----------------------------------------------------------
# 6. AGORA, O CÓDIGO DO GRÁFICO COMEÇA, USANDO OS DADOS JÁ PROCESSADOS
# -----------------------------------------------------------

# Garante a ordem cronológica correta para o eixo Y
mutate(ano_sazonal = forcats::fct_inorder(ano_sazonal)) %>%

  # Define as estéticas do ggplot
  ggplot(aes(x = xco2_anomalia, y = ano_sazonal, fill = estacao)) +

  geom_density_ridges(
    rel_min_height = 0.01,
    alpha = 0.8,
    color = "white",
    panel_scaling = FALSE
  ) +

  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +

  scale_fill_manual(
    name = "Estação Climática",
    values = c("Chuvosa (Dez-Mai)" = "#3B9AB2", "Seca (Jun-Nov)" = "#E1AF00")
  ) +

  coord_cartesian(xlim = c(-7.5, 7.5)) +

  theme_ridges(font_size = 12, grid = TRUE) +

  labs(
    title = "Anomalia da Concentração de XCO₂ por Ano Sazonal",
    subtitle = "Comparativo entre estações seca e chuvosa",
    x = "Anomalia de XCO₂ (ppm)",
    y = "Ano Sazonal"
  )
###
###
###



# Plotagem dos dados no quadrante xco2
country_br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill = "lightgray", color = "black") +
  ggplot2::geom_point(data = data_set_xco2 |>
                        dplyr::filter(year==2021),
                      ggplot2::aes(longitude, latitude),
                      size=.3,color="red") +
  # ajusta os limites do mapa
  ggplot2::coord_sf(xlim = c(-72, -48), ylim = c(-15, 0)) +
  ggplot2::labs(title = "XCO2")

###
###
###

# --- PASSO 1: Carregar as bibliotecas necessárias ---
# Se não tiver, instale com: install.packages(c("dplyr", "ggplot2", "ggridges"))
library(dplyr)
library(ggplot2)
install.packages("ggridges")
library(ggridges) # Biblioteca para o geom_density_ridges

# --- PASSO 2: Ler o arquivo .rds para um objeto (data frame) ---
# Substitua "caminho/para/seu/arquivo/" se ele não estiver na mesma pasta
# Estou chamando o objeto de 'dados', mas você pode dar o nome que quiser.
dados <- readRDS("data/data-set-xco2-filter.rds")
colnames(dados)

# --- PASSO 3: Rodar o código do gráfico, começando com o objeto 'dados' ---
# --- Carregue as bibliotecas ---
library(dplyr)
library(ggplot2)
library(ggridges)

# --- Carregue seus dados (se ainda não o fez) ---
# dados <- readRDS(file.choose())

# --- Opção 1: Plot por Mês, Cores por Ano ---
# --- Script do Gráfico Corrigido ---
# --- Script do Gráfico Corrigido ---
dados %>%
  # --- Script do Gráfico com Agrupamento Trimestral ---
  # --- Script do Gráfico com Agrupamento por Estação ---
  # --- Script do Gráfico (Opção 2: Zoom) ---
  dados %>%

  # Cria a coluna 'estacao' a partir da coluna 'month'
  mutate(estacao = case_when(
    month %in% 6:11 ~ "Seca (Jun-Nov)",
    TRUE              ~ "Chuvosa (Dez-Mai)"
  )) %>%

  # REMOVIDO: A linha filter() foi retirada desta parte do código.
  # O ggplot agora receberá TODOS os dados.

  ggplot(aes(x = xco2, y = as.factor(year), fill = estacao)) +

  geom_density_ridges(
    rel_min_height = 0.01,
    alpha = 0.7,
    color = "white",
    panel_scaling = FALSE
  ) +

  scale_fill_manual(
    name = "Estação",
    values = c("Chuvosa (Dez-Mai)" = "#3B9AB2", "Seca (Jun-Nov)" = "#E1AF00")
  ) +

  # ADICIONADO: Camada para dar zoom no eixo X sem remover dados do cálculo
  # As curvas são calculadas primeiro, depois o gráfico é "cortado" para esta visão.
  coord_cartesian(xlim = c(400, 430)) +

  theme_ridges() +

  labs(
    title = "Distribuição Sazonal de XCO₂ por Ano",
    subtitle = "Visão focada na faixa 400-430 ppm (cálculo com todos os dados)",
    x = "Concentração de XCO₂ (ppm)",
    y = "Ano"
  )
####
####
###
# Plotagem dos dados no quadrante xco2
country_br |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill = "lightgray", color = "black") +

  # MUDANÇA AQUI: 'color' foi para dentro do aes() e associado à coluna xco2
  ggplot2::geom_point(data = data_set_xco2 |>
                        dplyr::filter(year == 2021),
                      ggplot2::aes(x = longitude, y = latitude, color = xco2),
                      size = .3) + # A cor fixa 'red' foi removida daqui

  # ADIÇÃO: A mesma escala de cores que usamos antes para um visual melhor
  ggplot2::scale_color_viridis_c(option = "inferno") +

  # ajusta os limites do mapa
  ggplot2::coord_sf(xlim = c(-72, -48), ylim = c(-15, 0)) +

  # ADIÇÃO: Melhorei os títulos para incluir a legenda da escala
  ggplot2::labs(
    title = "Concentração de XCO₂ em 2021",
    subtitle = "Dados de satélite sobre o Brasil",
    x = "Longitude",
    y = "Latitude",
    color = "XCO₂ (ppm)" # Título da legenda da escala de cores
  )
###
###
###

# Certifique-se de que o dplyr está carregado
library(dplyr)

# Filtra os dados para o ano de 2021 (o mesmo do seu mapa)
dados_2021 <- data_set_xco2 |>
  dplyr::filter(year == 2021)
# Gera as estatísticas básicas para a coluna xco2
summary(dados_2021$xco2)

# Calcula o desvio padrão (sd)
# na.rm = TRUE é importante para remover valores ausentes (NA) que podem dar erro
sd(dados_2021$xco2, na.rm = TRUE)

hist(dados_2021$xco2,
     col = "steelblue",
     main = "Distribuição da Concentração de XCO₂ em 2021",
     xlab = "XCO₂ (ppm)",
     ylab = "Frequência")
###
##
###

# Rode este comando para ver todos os anos presentes nos seus dados
table(data_set_xco2$year)

library(dplyr)

# 1. Escolha os 4 anos que você quer visualizar
#    (Estou usando 2019, 2020, 2021, 2022 como exemplo)
anos_selecionados <- c(2020, 2021, 2022, 2023, 2024) # <-- MODIFIQUE AQUI

# 2. Filtre o dataset principal para conter apenas esses anos
dados_para_plot <- data_set_xco2 |>
  dplyr::filter(year %in% anos_selecionados)

library(ggplot2)

ggplot(dados_para_plot, aes(x = xco2)) +

  # Define o tipo de gráfico (histograma) e sua aparência
  geom_histogram(bins = 40, fill = "steelblue", color = "black", alpha = 0.8) +

  # A MÁGICA ACONTECE AQUI: Cria um painel para cada ano
  # ncol = 2 organiza os 4 gráficos em uma grade de 2x2
  facet_wrap(~ year, ncol = 2) +

  # Define os títulos e rótulos
  labs(
    title = "Distribuição Anual da Concentração de XCO₂",
    subtitle = "Comparativo entre os anos selecionados",
    x = "XCO₂ (ppm)",
    y = "Contagem (Frequência)"
  ) +

  # Aplica um tema visual limpo
  theme_minimal()

###
###
###
# Vamos criar uma cópia do seu dataset para não alterar o original
dados_regressao <- data_set_xco2

# Criando a variável de "observações" (índice sequencial)
# Esta será nossa variável X na regressão
dados_regressao$indice_obs <- 1:nrow(dados_regressao)

# Ajusta o modelo de regressão linear simples
# A fórmula "xco2 ~ indice_obs" significa "xco2 em função de indice_obs"
modelo_lm <- lm(xco2 ~ indice_obs, data = dados_regressao)

# Para ver os detalhes do modelo (coeficientes, R², etc.), você pode usar:
summary(modelo_lm)

# Adiciona a coluna com os valores de XCO2 estimados pelo modelo
dados_regressao$xco2_estimado <- fitted(modelo_lm)

# Adiciona a coluna com a diferença (resíduos)
dados_regressao$diferenca <- residuals(modelo_lm)

# Seleciona e exibe as colunas relevantes para facilitar a visualização
print(head(dados_regressao[, c("xco2", "xco2_estimado", "diferenca")]))

library(ggplot2)

ggplot(dados_regressao, aes(x = indice_obs, y = xco2)) +
  # Plota os pontos de dados observados
  geom_point(alpha = 0.5, color = "darkgray") +

  # Adiciona a linha de regressão linear (calcula e plota automaticamente)
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1.5) +

  labs(
    title = "Regressão Linear Simples de XCO₂",
    subtitle = "Tendência de XCO₂ em função da sequência de observações",
    x = "Sequência de Observações",
    y = "XCO₂ Observado (ppm)"
  ) +
  theme_minimal()

###
###
###


library(dplyr)
library(ggplot2)

# Filtra o dataset para incluir apenas os anos de 2020 a 2024
dados_plot_anos <- data_set_xco2 |>
  dplyr::filter(year %in% 2020:2024)

ggplot(dados_plot_anos, aes(x = factor(year), y = xco2)) +

  # 1. Adiciona os Boxplots para cada ano
  # Mostra a mediana, os quartis e a dispersão dos dados anuais
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7, outlier.shape = NA) +

  # Opcional: Adiciona os pontos com transparência por trás dos boxplots
  geom_jitter(width = 0.1, alpha = 0.2) +

  # 2. Adiciona a linha de regressão geral
  # aes(group = 1) é um truque para garantir que ele desenhe UMA linha para todos os anos
  geom_smooth(method = "lm", aes(group = 1), color = "red", se = FALSE, linewidth = 1.5) +

  # 3. Customiza os títulos e rótulos
  labs(
    title = "Distribuição e Tendência Anual de XCO₂ (2020-2024)",
    subtitle = "Boxplots anuais com linha de regressão linear geral",
    x = "Ano",
    y = "Concentração de XCO₂ (ppm)"
  ) +

  theme_minimal()

###
###
###


library(dplyr)
library(ggplot2)
library(lubridate)

# --- Passo 1: Criar a Coluna 'trimestre' e Calcular a Média Trimestral ---

dados_trimestrais <- data_set_xco2 %>%

  # PASSO NOVO: Cria a coluna 'trimestre' a partir da coluna 'month'
  mutate(trimestre = case_when(
    month %in% 1:3   ~ 1,  # Se o mês está entre 1 e 3, atribui 1
    month %in% 4:6   ~ 2,  # Se o mês está entre 4 e 6, atribui 2
    month %in% 7:9   ~ 3,  # Se o mês está entre 7 e 9, atribui 3
    month %in% 10:12 ~ 4   # Se o mês está entre 10 e 12, atribui 4
  )) %>%

  # O resto do código continua como antes...
  # Agrupa primeiro por ano e depois pelo trimestre que acabamos de criar
  group_by(year, trimestre) %>%

  # Calcula a média de xco2 para cada grupo
  summarise(
    xco2_medio = mean(xco2, na.rm = TRUE),
    .groups = 'drop' # Remove o agrupamento após o summarise
  ) %>%

  # Garante que os dados estejam na ordem cronológica correta
  arrange(year, trimestre)

# --- Passo 2: Criar uma Coluna de Data para o Eixo X ---
# (Este passo permanece o mesmo)
dados_trimestrais <- dados_trimestrais %>%
  mutate(
    data_trimestre = ymd(paste(year, (trimestre * 3 - 1), "15", sep = "-"))
  )

# --- Passo 3: Gerar o Gráfico com os Dados Trimestrais ---
# (O código do gráfico também permanece exatamente o mesmo)

ggplot(dados_trimestrais, aes(x = data_trimestre, y = xco2_medio)) +

  geom_point(color = "darkred", size = 2.5) +
  geom_line(color = "darkred", linewidth = 0.9) +

  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +

  labs(
    title = "Média Trimestral da Concentração de XCO₂",
    x = "Ano",
    y = "Média de XCO₂ (ppm)"
  ) +

  theme_minimal() +

  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "gray80")
  )


###
###
###
# Plotagem dos dados no quadrante SIF
country_br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill = "lightgray", color = "black") +
  ggplot2::geom_point(data = data_set_sif |>
                        dplyr::filter(year==2020),
                      ggplot2::aes(longitude, latitude),
                      size=.3,color="blue") +
  # ajusta os limites do mapa
  ggplot2::coord_sf(xlim = c(-72, -48), ylim = c(-15, 0))+
  ggplot2::labs(title = "SIF")
