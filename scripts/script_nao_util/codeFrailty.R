library(rstan)
library(shinystan)
library(ggplot2)
library(frailtyHL)

data(kidney)
str(kidney)

# Numero de "areas"
n <- length(unique(dados_frailty$id))
J <- table(dados_frailty$id)
Jmax <- max(J)
N <- nrow(dados_frailty)

# Covariates
dados_frailty$chuva_acum_1000 <- dados_frailty$chuva_acumulada / 1000
dados_frailty$temp_sum_1000 <- dados_frailty$temp_sum / 1000
dados_frailty$porc_queimada_log <- log(dados_frailty$porc_queimada + 0.001)
# sex <- kidney$sex[seq(1, 2 * n, 2)] - 1 # Reference = male
X <- model.matrix(~ chuva_acum_1000 + temp_sum_1000 + porc_queimada_log + sif_mean, data = dados_frailty)
cor(X[,-1])
summary(X[,-1])
# X <- matrix(1, ncol = 1, nrow = n)
# Quando tivermos as covariaveis, iremos corrigir essa linha acima.

# Survival information
time_aux <- dados_frailty$time
delta_aux <- dados_frailty$cens
# Matrix format
time <- matrix(0, nrow = n, ncol = Jmax)
delta <- matrix(0, nrow = n, ncol = Jmax)
count <- 1
for(i in 1:n){
  time[i, 1:J[i]] <- time_aux[count:(count+J[i]-1)]
  delta[i, 1:J[i]] <- delta_aux[count:(count+J[i]-1)]
  count <- count + J[i]
}

model <- stan(file   = "../projeto r mestrado/scripts/Frailty.stan",
           data   = list(n = n, N = N, Jmax = Jmax, J = J, Nbetas = ncol(X),
                         time = time, delta = delta, X = X),
           warmup = 1000,
           iter   = 3000,
           chains = 3,
           seed   = 1,
           cores  = getOption("mc.cores",3))

#Salvar results
 # save.image("C:/Users/RODRIGO/Desktop/Projeto R mestrado/scripts/results.RData")
#Abrir results Rdata
load("C:/Users/RODRIGO/Desktop/Projeto R mestrado/scripts/results.RData")

print(model, digits = 3)



# Diagnostics
# launch_shinystan(model)
pars1 <- c("beta", "alpha", "lambda", "psi")
plot(model, plotfun = "trace", pars = pars1, inc_warmup = TRUE)
plot(model, plotfun = "hist", pars = pars1)

pars2 <- c("beta", "alpha", "lambda", "psi", "w")
post <- rstan::extract(model, pars2)

hist(post$beta)
hist(post$alpha)


# Individual survival curve
grid <- 1000
#time <- seq(0, max(dados_frailty$time), len = grid)
time <- seq(0, 310, len = grid)
surv <- matrix(NA, n, grid)

for(i in 1:n){
  for(k in 1:grid){
    surv[i, k] <- mean( exp(-post$w[,i] * post$lambda * time[k]^post$alpha * exp(as.vector(post$beta %*% X[i,]))) )
  }
}

surv2 <- rep(NA, n)

for(i in 1:n){
  surv2[i] <- median( exp(-post$w[,i]) )
}


color_aux <- rep("black",n)
color_aux [c(which.min(surv2), which.max(surv2))] <- c("red", "blue")
color_ext = rep(color_aux, each=grid)


df <- data.frame(time = rep(time + min(df_final$time)-1,n), survival = c(t(surv)),
                 area = rep(1:n, each = grid),color_ext = as.factor(color_ext))

ggplot(data = df, aes(x = time, y = survival, group = area)) +
  geom_line() + theme_bw() + theme(legend.position = "top")+
  ylim(0,1)


median_time <- data.frame(area = rep(NA,n),time_est = rep(NA,n),
                          long = rep(NA,n), lat = rep(NA,n))
for(i in 1:n){
  pos <- which(df$survival[(1000*(i-1)+1):(1000*i)] < 0.5)[1] + 1000*(i-1)
  median_time$area[i]<- df$area[pos]
  median_time$time_est[i]<- df$time[pos] #- min(df_final$time) + 1

  }
median_time$long <- substr(unique(df_final$long_lat), start = 1, stop = 3)
median_time$lat <- substr(unique(df_final$long_lat), start = 4, stop = 6)

#Retirar as réplicas da base frailty
#(pegar a primeira vez que aparece o valor no id(66))
print(dados_frailty)

# Cria uma nova base apenas com a primeira linha de cada ID
dados_unicos_frailty <- dados_frailty %>%
  distinct(id, .keep_all = TRUE)

# Conferindo se agora temos menos linhas
message("Linhas originais: ", nrow(dados_frailty))
message("Linhas após o filtro: ", nrow(dados_unicos_frailty))
####################

# Instale se ainda não tiver: install.packages("corrplot")
library(corrplot)
library(dplyr)
library(tidyr)

# 1. Selecionar e converter tudo para numérico
dados_cor <- dados_frailty %>%
  filter(cens == 0) |>
  select(
    time,
    #cens,
    sif_mean,
    porc_queimada_log,
    chuva_acum_1000,
    temp_sum_1000
  ) %>%
  # O 'as.character' antes do 'as.numeric' é um truque seguro para não perder dados de fatores
  mutate(across(everything(), ~as.numeric(as.character(.))))

# 2. Agora o cálculo da matriz deve funcionar
# 'use = "pairwise.complete.obs"' é mais flexível com NAs
M <- cor(dados_cor, use = "pairwise.complete.obs")

# 3. Gerar o gráfico
corrplot(M,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         title = "Correlação: Variáveis de Solo, Fogo e SIF",
         mar = c(0,0,1,0))

#######################
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Preparar e filtrar os dados usando a posição 3 como corte
dados_quadrante <- dados_frailty %>%
  # sep = 3 significa: "corte após o 3º caractere"
  # convert = TRUE transforma o resultado de texto para número automaticamente
  separate(long_lat, into = c("lon", "lat"), sep = 3, convert = TRUE) %>%

  # 2. Aplicar o seu filtro de delimitação
  filter(lon >= -59.7700 & lon <= -49.8361,
         lat >= -12.3561 & lat <= -1.6058)

# 3. Verificação de segurança (O "check" do foguete)
message("Pontos encontrados no quadrante: ", nrow(dados_quadrante))

# 4. Se houver dados, gerar o gráfico limpo
if (nrow(dados_quadrante) > 0) {

  ggplot(dados_quadrante, aes(x = lon, y = lat)) +
    # Cor pelo SIF e tamanho pela Porcentagem de Queima
    geom_point(aes(color = sif_mean, size = porc_queimada), alpha = 0.7) +

    scale_color_viridis_c(option = "mako", name = "SIF") +
    scale_size_continuous(name = "% Queimada") +

    # Janelas por ano
    facet_wrap(~year) +

    # Limites fixos do quadrante
    coord_cartesian(xlim = c(-59.7700, -49.8361),
                    ylim = c(-12.3561, -1.6058)) +

    theme_bw() +
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white", face = "bold")) +

    labs(title = "Quadrante de Estudo: SIF vs. Fogo",
         x = "Longitude", y = "Latitude")

} else {
  message("⚠️ O filtro ainda resultou em 0 linhas. Verifique os valores de 'lon' e 'lat' gerados.")
}



##############################

library(dplyr)
library(readr)

# 1. Preparar a base median_time (pegando apenas o necessário)
df_median_selecionado <- median_time %>%
  select(area, time_est) %>%
  mutate(time_est = as.numeric(time_est)) # Garante que a estimativa seja numérica

# 2. Unir com a base real e calcular o erro
df_final_validacao <- dados_unicos_frailty %>%

  inner_join(df_median_selecionado, by = c("id" = "area")) %>%
  mutate(
    # Remove o 'days' e transforma em número
    time_real = parse_number(as.character(time)) + min(df_final$time)-1,

    # Agora a conta é direta e segura
    erro = time_real - time_est
  )

# 3. Conferir o resultado
message("Base consolidada com sucesso!")
print(head(df_final_validacao %>% select(id, time_real, time_est, erro)))


library(ggplot2)

# 1. Gerar o gráfico sem interrupções na "corrente" de +
ggplot(df_final_validacao, aes(x = time_real, y = time_est)) +
  # Pontos: Cor representa o tamanho do erro absoluto
  geom_point(aes(color = abs(erro)), size = 3, alpha = 0.7) +

  # A Linha de Identidade (1:1) - Onde o modelo seria perfeito
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +

  # Linha de tendência dos seus dados
  geom_smooth(method = "lm", color = "blue", fill = "lightgrey", se = TRUE) +

  # Escala de cor 'plasma' para destacar erros maiores
  scale_color_viridis_c(option = "plasma", name = "Erro (Dias)") +

  # Garante que os eixos X e Y tenham a mesma escala (gráfico quadrado)
  coord_fixed(ratio = 1) +

  theme_minimal() +
  labs(
    title = "Validação do Modelo de Frailty: Real vs. Estimado",
    subtitle = "Linha tracejada vermelha: Ideal (1:1) | Linha azul: Tendência Real",
    x = "Tempo Real Observado (Dias)",
    y = "Tempo Mediano Estimado (Dias)"
  ) +
  theme(legend.position = "right")

# 2. Calcular o R² (Coeficiente de Determinação)
modelo_linear <- lm(time_est ~ time_real, data = df_final_validacao)
r2_valor <- summary(modelo_linear)$r.squared
message("O R² do seu modelo é: ", round(r2_valor, 3))


##########################


library(dplyr)
library(readr)

# 1. Preparar a base de estimativas (median_time)
df_median_limpo <- median_time %>%
  select(area, time_est) %>%
  mutate(time_est = as.numeric(time_est))

# 2. Unir as bases, limpar o texto "days" e calcular o erro
df_final_validacao <- dados_unicos_frailty %>%
  filter(cens == 1) |>
  inner_join(df_median_limpo, by = c("id" = "area")) %>%
  mutate(
    # Extrai apenas o número da coluna que tem "xx days"
    time_real = parse_number(as.character(time)) + min(df_final$time)-1,
    # Diferença entre o observado e o previsto
    erro = time_real - time_est
  )

# 3. Gerar os indicadores de performance
resumo_estatistico <- df_final_validacao %>%
  summarise(
    n = n(),
    Erromedioabs = mean(abs(erro), na.rm = TRUE),
    RMSE = sqrt(mean(erro^2, na.rm = TRUE)),
    Erro_Medio = mean(erro, na.rm = TRUE),
    Correlacao = cor(time_real, time_est, use = "complete.obs")
  )

# 4. Exibir os resultados
print(resumo_estatistico)


########################

library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Preparar os dados para o formato 'longo'
# Isso permite criar todos os gráficos de uma vez com o facet_wrap
df_diagnostico <- df_final_validacao %>%
  select(erro, sif_mean, porc_queimada_log, chuva_acum_1000, temp_sum_1000, time_real) %>%
  pivot_longer(cols = -erro, names_to = "variavel", values_to = "valor")

# 2. Gerar o conjunto de gráficos
ggplot(df_diagnostico, aes(x = valor, y = erro)) +
  # Pontos com transparência para ver a densidade
  geom_point(alpha = 0.4, color = "darkblue") +

  # Linha do Erro Zero (Onde o modelo acertou)
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.8) +

  # Linha de tendência para identificar vieses
  geom_smooth(method = "lm", color = "black", se = TRUE) +

  # Cada variável em seu próprio quadro com sua escala de X
  facet_wrap(~variavel, scales = "free_x", ncol = 2) +

  theme_bw() +
  labs(title = "Diagnóstico de Erros: O que o modelo está ignorando?",
       subtitle = "",
       x = "Valor da Variável",
       y = "Erro (Tempo Real - Estimado)",
       caption = "Linha vermelha = Acerto perfeito | Linha preta = Tendência do erro") +
  theme(strip.background = element_rect(fill = "gray20"),
        strip.text = element_text(color = "white", face = "bold"))










########################
ggplot() +
  # Fundo
  geom_sf(data = mapa_fundo_local, fill = "gray95", color = "gray60") +

  # Dados de Tempo
  geom_tile(data = dados_,
            aes(x = longitude_t4, y = latitude_t4, fill = dias_ate_pico),
            width = 1, height = 1) +

  # Escala de cores para TEMPO
  # Roxo/Azul = Rápido (Cedo, logo em Janeiro)
  # Amarelo/Verde = Lento (Tarde, só em Março)
  scale_fill_viridis_c(option = "viridis", direction = -1,
                       name = "Dias até\no Pico") +

  facet_wrap(~year) +

  coord_sf(xlim = c(limites_recorte["xmin"], limites_recorte["xmax"]),
           ylim = c(limites_recorte["ymin"], limites_recorte["ymax"])) +

  theme_minimal() +
  labs(title = "Velocidade de Resposta do Sumidouro",
       subtitle = "Cores claras: A vegetação atinge o pico de absorção rápido.\nCores escuras: Demora mais para atingir o pico.",
       x = "Longitude", y = "Latitude")
