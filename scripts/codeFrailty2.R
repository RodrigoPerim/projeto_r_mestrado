library(rstan)
library(shinystan)
library(ggplot2)
library(frailtyHL)
library(survminer)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(tibble)
library(writexl)
library(sf)

###############
# Criação e organização de modelo
###############
view(dados_frailty)
# data(kidney)
# str(kidney)
#
# # Numero de "areas"
# n <- length(unique(dados_frailty$id))
# J <- table(dados_frailty$id)
# Jmax <- max(J)
# N <- nrow(dados_frailty)
#
# # Covariates
# dados_frailty$chuva_acum_1000 <- dados_frailty$chuva_acumulada / 1000
# dados_frailty$temp_sum_1000 <- dados_frailty$temp_sum / 1000
# dados_frailty$porc_queimada_log <- log(dados_frailty$porc_queimada + 0.001)
# # sex <- kidney$sex[seq(1, 2 * n, 2)] - 1 # Reference = male
dados_frailty$temp <- dados_frailty$temp_sum/as.numeric(df_final$time)
dados_frailty$chuva <- dados_frailty$chuva_acumulada/as.numeric(df_final$time)
# X <- model.matrix(~ chuva_acum_1000 + temp_sum_1000 + porc_queimada_log + sif_mean, data = dados_frailty)
X <- model.matrix(~ chuva + temp + porc_queimada_log + sif_mean, data = dados_frailty)
# cor(X[,-1])
# summary(X[,-1])
# # X <- matrix(1, ncol = 1, nrow = n)
# # Quando tivermos as covariaveis, iremos corrigir essa linha acima.
#
# # Survival information
# time_aux <- dados_frailty$time
# delta_aux <- dados_frailty$cens
# # Matrix format
# time <- matrix(0, nrow = n, ncol = Jmax)
# delta <- matrix(0, nrow = n, ncol = Jmax)
# count <- 1
# for(i in 1:n){
#   time[i, 1:J[i]] <- time_aux[count:(count+J[i]-1)]
#   delta[i, 1:J[i]] <- delta_aux[count:(count+J[i]-1)]
#   count <- count + J[i]
# }
#
# model <- stan(file   = "../projeto r mestrado/scripts/Frailty2.stan",
#            data   = list(n = n, N = N, Jmax = Jmax, J = J, Nbetas = ncol(X),
#                          time = time, delta = delta, X = X),
#            warmup = 1000,
#            iter   = 3000,
#            chains = 3,
#            seed   = 1,
#            cores  = getOption("mc.cores",3))
#
# #Salvar results
  # save.image("C:/Users/RODRIGO/Desktop/Projeto R mestrado/scripts/results2.RData")
# #Abrir results Rdata

library(survival)
library(ggsurvfit)

fig.4 <- survfit2(Surv(time, cens) ~ 1, data = dados_frailty) |>
  ggsurvfit() +
  labs(x = "Time (in days)", y = "Survival probability") +
  add_confidence_interval()+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = seq(8, 98, 20),
                     labels = seq(100, 190, 20))+
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("Fig4.png", fig.4, width=10, height=5, dpi=300, units="in", device='png')



load("C:/Users/RODRIGO/Desktop/Projeto R mestrado/scripts/results2.RData")

print(model, digits = 3) # TABELA 1 pós gráfico 1

library(HDInterval)
beta <- rstan::extract(model, "beta")$beta
round(apply(beta,2,mean),3)[-1]
round(hdi(beta[,2], credMass = 0.95),3)
round(hdi(beta[,3], credMass = 0.95),3)
round(hdi(beta[,4], credMass = 0.95),3)
round(hdi(beta[,5], credMass = 0.95),3)



# ====================================================================
# NOVO PASSO: exp(beta) e os respectivos ICs (HDI 95%)
# ====================================================================

# 1. Média do exp(beta) para cada covariável (ignorando a coluna 1 que é o intercepto)
exp_beta_medias <- round(apply(exp(beta), 2, mean), 3)[-1]
cat("\nMédias de exp(beta) [Var 2 a 5]:\n")
print(exp_beta_medias)

# 2. Intervalos de Credibilidade (HDI 95%) do exp(beta) para cada covariável
hdi_exp_beta2 <- round(hdi(exp(beta[,2]), credMass = 0.95), 3)
hdi_exp_beta3 <- round(hdi(exp(beta[,3]), credMass = 0.95), 3)
hdi_exp_beta4 <- round(hdi(exp(beta[,4]), credMass = 0.95), 3)
hdi_exp_beta5 <- round(hdi(exp(beta[,5]), credMass = 0.95), 3)

cat("\nHDI 95% para exp(beta2) - Chuva:\n")
print(hdi_exp_beta2)

cat("\nHDI 95% para exp(beta3) - Temp:\n")
print(hdi_exp_beta3)

cat("\nHDI 95% para exp(beta4) - Queimada:\n")
print(hdi_exp_beta4)

cat("\nHDI 95% para exp(beta5) - SIF:\n")
print(hdi_exp_beta5)




# ######################################
#  ########## TABELA DE VARIAVEIS BETA, ALPHA E LAMBDA
# library(dplyr)
# library(gt)
# library(tibble)
#
# # 1. Definindo os dados com notação Markdown para letras gregas
# dados_artigo <- tibble(
#   Interpretation = c("Precipitation", "Temperature", "Burned area", "SIF", "Weibull shape", "Weibull scale"),
#   # Substituindo o texto por notação de símbolos
#   Parameters = c("$$\\beta_{prec}$$", "$$\\beta_{temp}$$", "$$\\beta_{burn}$$", "$$\\beta_{sif}$$", "$$\\alpha$$", "$$\\lambda$$"),
#   Mpike = c("-1.323 (-2.083, -0.572)", "-3.181 (-3.872, -2.531)", "0.016 (-0.050, 0.081)",
#             "-0.189 (-3.161, 2.857)", "3.574 (2.956, 4.246)", "1.150 (0.156, 4.476)"),
#   Group = c(rep("Climate & Environmental Parameters", 4), rep("Survival Distribution Parameters", 2))
# )
#
# # 2. Gerando a tabela final
# tabela_final <- dados_artigo %>%
#   group_by(Group) %>%
#   gt() %>%
#   tab_header(
#     title = md("**TABLE 1** Posterior summary (posterior mean and 95% credible interval) for atmospheric CO2 analysis.")
#   ) %>%
#   cols_label(
#     Interpretation = "Interpretation",
#     Parameters = "Parameters",
#     Mpike = "Mean (95% CI)"
#   ) %>%
#   # ESSENCIAL: Habilitar a renderização dos símbolos Markdown/LaTeX
#   fmt_markdown(columns = Parameters) %>%
#   # Configurações de estilo
#   tab_options(
#     table.border.top.style = "solid",
#     table.border.top.color = "black",
#     table.border.top.width = px(2),
#     table.border.bottom.style = "solid",
#     table.border.bottom.color = "black",
#     table.border.bottom.width = px(2),
#     column_labels.border.bottom.style = "solid",
#     column_labels.border.bottom.color = "black",
#     column_labels.border.bottom.width = px(1.5),
#     row_group.background.color = "#E9E9E9",
#     table.font.size = px(18),
#     column_labels.border.lr.style = "none",
#     table_body.vlines.style = "none",
#     table_body.hlines.style = "none"
#   ) %>%
#   cols_align(
#     align = "center",
#     columns = c(Parameters, Mpike)
#   )
#
# # Exibir a tabela
# tabela_final


###################################


###############
# Análise de residuos (Cox Snell) - GRÁFICO 1
###############
library(survival)
library(survminer)


# Cox-Snell residuals com eixos amplificados
cox_snell_fc <- function(model, time, cens, X){

  # ESPECIFICADO rstan::extract para evitar conflito com tidyr
  beta <- apply(rstan::extract(model, "beta")$beta, 2, mean)
  alpha <- mean(rstan::extract(model, "alpha")$alpha)

  r_CS <- time^alpha * exp( X %*% beta )

  dta <- data.frame(time = r_CS, status = cens)
  km <- survfit(Surv(time, status) ~ 1, data = dta)
  tt <- seq(0, 1.972, len = 100)
  dta_exp <- data.frame(time = tt, exp1 = exp(-tt))

  survplot <- ggsurvplot(km,
                         censor = FALSE,
                         legend = "none",
                         palette = "black",
                         linetype = "dashed",
                         xlab = "Cox-Snell residuals",
                         xlim = c(0, 1.972),
                         break.x.by = 0.5,
                         size = 0.6,
                         conf.int = TRUE,
                         # AQUI ESTÁ O AJUSTE: Note que o parêntese do theme() fecha só no final
                         ggtheme = theme_bw() +
                           theme(axis.text = element_text(size = 14),
                                 axis.title = element_text(size = 16)),
                         data = dta)$plot

  survplot + geom_line(data = dta_exp, aes(x = time, y = exp1), col = "red") +
    scale_x_continuous(limits=c(0, 2))
}


# Chame a função normalmente
fig.6 <- cox_snell_fc(model, as.numeric(dados_frailty$time), dados_frailty$cens, X)

ggsave("Fig6.png", fig.6, width=10, height=5, dpi=300, units="in", device='png')
###############
#Valores pós modelo
###############


# Diagnostics
# launch_shinystan(model)
pars1 <- c("beta", "alpha", "lambda")
plot(model, plotfun = "trace", pars = pars1, inc_warmup = TRUE)
plot(model, plotfun = "hist", pars = pars1)

pars2 <- c("beta", "alpha", "lambda")
post <- rstan::extract(model, pars2)

hist(post$beta)
hist(post$alpha)


# Individual survival curve
grid <- 1000
#time <- seq(0, max(dados_frailty$time), len = grid)
time <- seq(0, 310, len = grid)
surv <- surv_lower <- surv_upper <- matrix(NA, N, grid)

for(i in 1:N){
  for(k in 1:grid){
    surv[i, k] <- mean( exp(-time[k]^post$alpha * exp(as.vector(post$beta %*% X[i,]))) )
    surv_lower[i, k] <- quantile( exp(-time[k]^post$alpha * exp(as.vector(post$beta %*% X[i,]))), probs = 0.025)
    surv_upper[i, k] <- quantile( exp(-time[k]^post$alpha * exp(as.vector(post$beta %*% X[i,]))), probs = 0.975)
  }
}


df <- data.frame(time = rep(time + min(df_final$time)-1,N), survival = c(t(surv)),
                 lower = c(t(surv_lower)), upper = c(t(surv_upper)), area = rep(1:N, each = grid))
# Adicionando as covariáveis de dados_frailty (108 obs) ao df (108.000 obs)
# O parâmetro 'each = grid' garante que cada valor se repita para os 1.000 pontos de cada área

df$chuva   <- rep(dados_frailty$chuva, each = grid)
df$temp  <- rep(dados_frailty$temp, each = grid)
df$porc_queimada <- rep(dados_frailty$porc_queimada, each = grid)
df$sif_mean          <- rep(dados_frailty$sif_mean, each = grid)

# Verificação rápida: as primeiras 1.000 linhas devem ter o mesmo valor de chuva (da área 1)
head(df)


ggplot(data = df, aes(x = time, y = survival, group = area)) +
  geom_line() + theme_bw() + theme(legend.position = "top")+
  ylim(0,1)

########### RODAR ESSE E PRESTAR ATENÇÃO NAS ORDEM QUE IREMOS RODAR AS PRÓXIMAS COISAS
#Plotagem de gráfico
###########
# Individual survival curve
grid <- 1000
#time <- seq(0, max(dados_frailty$time), len = grid)
time <- seq(0, 310, len = grid)
surv_mean <- surv_mean_lower <- surv_mean_upper <- rep(NA, grid)
# X_mean <- apply(X,2,mean)
X_mean <- round(apply(X,2,mean),3)

for(k in 1:grid){
    surv_mean[k] <- mean( exp(-time[k]^post$alpha * exp(as.vector(post$beta %*% X_mean))) )
    surv_mean_lower[k] <- quantile( exp(-time[k]^post$alpha * exp(as.vector(post$beta %*% X_mean))), probs = 0.025)
    surv_mean_upper[k] <- quantile( exp(-time[k]^post$alpha * exp(as.vector(post$beta %*% X_mean))), probs = 0.975)
}

########### FEITO PELO DANILO ###############

# plot(df$time[40001:41000], surv_mean, type = "l",
#      ylim = c(0,1), xlim = c(91,180),
#      xlab = "Time (in days)",          # Nome do eixo X
#      ylab = "Survival probability",    # Nome do eixo Y
#      lwd = 2)
# lines(df$time[40001:41000], surv_mean_lower, col = "blue", lty = 2)
# lines(df$time[40001:41000], surv_mean_upper, col = "red", lty = 2)

############ Mesmo gráfico que o de cima, mas com ggplot #############

library(ggplot2)

library(ggplot2)

# 1. Preparando os dados
df_plot <- data.frame(
  time  = time + 92,
  mean  = surv_mean,
  lower = surv_mean_lower,
  upper = surv_mean_upper
)

# 2. ENCONTRAR O PONTO DE INTERSEÇÃO (Onde a média cruza 0.5)
# Isso garante que a linha pontilhada caia no lugar exato
median_time <- df_plot$time[which.min(abs(df_plot$mean - 0.5))]

# 3. Gerando o gráfico
fig.5 <- ggplot(df_plot, aes(x = time)) +
  # A "Fumaça" (sempre primeiro para ficar no fundo)
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.5) +

  # A Linha Principal
  geom_line(aes(y = mean), color = "black", linewidth = 1) +

  # --- AS LINHAS DE REFERÊNCIA (A MIRA) ---
  # Linha Horizontal (do eixo Y até a curva)
  annotate("segment", x = 91, xend = median_time, y = 0.5, yend = 0.5,
           linetype = "dotted", color = "black", linewidth = 0.7) +

  # Linha Vertical (da curva até o eixo X)
  annotate("segment", x = median_time, xend = median_time, y = 0, yend = 0.5,
           linetype = "dotted", color = "black", linewidth = 0.7) +
  # ----------------------------------------

# Ajustes de Eixos e Labels
scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(limits = c(91, 280), breaks = c(seq(100, 280, 30),round(median_time, 0)),
                     labels = c(seq(100, 280, 30),round(median_time, 0)),
                     minor_breaks = c(seq(100, 280, 30),round(median_time, 0))) +
  labs(
    x = "Time (in days)",
    y = "Survival probability"
  ) +

  # Tema Acadêmico Clean
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("Fig5.png", fig.5, width=10, height=5, dpi=300, units="in", device='png')




















# # 1. Gráfico principal com eixos em inglês
# plot(df$time[6001:7000], df$survival[6001:7000], type = "l",
#      ylim = c(0,1), xlim = c(91,200),
#      xlab = "Time (in days)",          # Nome do eixo X
#      ylab = "Survival probability",    # Nome do eixo Y
#      lwd = 2)
#
# # 2. Linhas de intervalo de confiança (Semântica Azul/Vermelho)
# lines(df$time[6001:7000], df$lower[6001:7000], col = "blue", lty = 2)
# lines(df$time[6001:7000], df$upper[6001:7000], col = "red", lty = 2)
#
# # 3. Indicadores da Mediana (0.5)
# segments(0, 0.5, 123.7, 0.5, lty = 3)
# segments(123.7, 0, 123.7, 0.5, lty = 3)
#
# # 4. Inclusão da nota técnica em inglês
# mtext("Note: The estimated time for each region was calculated based on the median time obtained by the model.",
#       side = 1, line = 4, adj = 0, cex = 0.8, font = 3)
#
# library(dplyr)
#
# # Calculando a média dos drivers para esse recorte do gráfico
# resumo_recorte <- df %>%
#   slice(6001:7000) %>%
#   summarise(
#     mean_SIF = mean(sif_mean, na.rm = TRUE),
#     mean_Prec = mean(chuva_acumulada, na.rm = TRUE),
#     mean_Queim = mean(porc_queimada, na.rm = TRUE),
#     mean_Temp = mean(temp_sum, na.rm = TRUE)
#   )
#
# print(resumo_recorte)













# Cálculo da mediana de TODOS os valores da coluna time
mediana_geral <- median(df$time, na.rm = TRUE)

# 1. Gráfico principal (subset mantido para visualização limpa da curva)
plot(median(df$time, na.rm = TRUE), median(df$survival, na.rm = TRUE),
     ylim = c(0,1), xlim = c(91,280),
     xlab = "Time (in days)",
     ylab = "Survival probability",
     lwd = 2)

# 2. Linhas de intervalo de confiança
lines(median(df$time, na.rm = TRUE), median(df$lower, na.rm = TRUE), col = "blue", lty = 2)
lines(median(df$time, na.rm = TRUE), median(df$upper, na.rm = TRUE), col = "red", lty = 2)

# 3. Indicadores da Mediana (Usando a mediana global calculada acima)
segments(0, 0.5, mediana_geral, 0.5, lty = 3)
segments(mediana_geral, 0, mediana_geral, 0.5, lty = 3)

# Opcional: Mostra o valor da mediana no console para conferência
print(paste("A mediana geral é:", mediana_geral))

# 4. Inclusão da nota técnica
mtext("Note: The dashed segments represent the median time calculated from the complete dataset.",
      side = 1, line = 4, adj = 0, cex = 0.8, font = 3)
























library(gt)
library(dplyr)

# Gerar a tabela simples a partir do resumo_recorte
tabela_simples <- resumo_recorte %>%
  gt() %>%
  # Renomear os títulos das colunas
  cols_label(
    mean_SIF = md("**SIF**<br>(mean)"),
    mean_Prec = md("**Precipitation**<br>(mm)"),
    mean_Queim = md("**Burned Area**<br>(%)"),
    mean_Temp = md("**Temperature**<br>(°C)")
  ) %>%
  # Formatar os números com 3 casas decimais
  fmt_number(
    columns = everything(),
    decimals = 3
  ) %>%
  # Ajustes de layout para ficar "clean"
  tab_options(
    table.width = pct(100),
    column_labels.font.size = px(16),
    table.font.size = px(18),
    data_row.padding = px(10),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
    table.border.top.style = "none",
    table.border.bottom.style = "none"
  ) %>%
  cols_align(align = "center")

# Exibir
tabela_simples








# 1. Preparar o recorte dos dados (Slicing 6001:7000)
df_plot <- df[6001:7000, ]
names(df_plot)
# 2. Criar o gráfico
ggplot(df_plot, aes(x = time)) +
  # A SOMBRA CINZA: geom_ribbon cria a área entre o lower e o upper
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) +

  # As linhas de limite (em azul e tracejadas)
  geom_line(aes(y = lower), color = "blue", linetype = "dashed") +
  geom_line(aes(y = upper), color = "blue", linetype = "dashed") +

  # A CURVA PRINCIPAL: Sobrevivência
  geom_line(aes(y = survival), color = "black", size = 0.5) +

  # SEGMENTOS DE REFERÊNCIA (Mediana em 0.5)
  geom_segment(aes(x = 91, xend = 124.8238, y = 0.5, yend = 0.5), linetype = "dotted") +
  geom_segment(aes(x = 124.8238, xend = 124.8238, y = 0, yend = 0.5), linetype = "dotted") +

  # PONTOS DE VALIDAÇÃO (ID #7)
  # Real (Vermelho)
  geom_point(aes(x = df_median_limpo$time_real[7], y = 0.5), color = "red", size = 2) +
  # Estimado (Preto)
  geom_point(aes(x = df_median_limpo$time_est[7], y = 0.5), color = "black", size = 2) +

  # LIMITES E ZOOM
  coord_cartesian(xlim = c(91, 200), ylim = c(0, 1)) +

  # ESTÉTICA E LABELS
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title = "Exemplo de Curva de Sobrevivência estimada e seu intervalo de credibildiade de 95% p/ area especifica", # Retirar e colocar na legenda da tabela,
       subtitle = "Sombra cinza indica o Intervalo de Confiança do Modelo de Fragilidade(ponto preto indica tempo mediano estimado e o vermelho o observado)",# Retirar e colocar na legenda da tabela,
       x = "Time (in days)",
       y = "Survival")

cor(df_median_limpo$time_real, df_median_limpo$time_est)


###############
# Criação do median time
###############

median_time <- data.frame(area = rep(NA,N),time_est = rep(NA,N),
                          long = rep(NA,N), lat = rep(NA,N))
for(i in 1:N){
  pos <- which(df$survival[(1000*(i-1)+1):(1000*i)] < 0.5)[1] + 1000*(i-1)
  median_time$area[i]<- df$area[pos]
  median_time$time_est[i]<- df$time[pos] #- min(df_final$time) + 1

  }
median_time$long <- substr((df_final$long_lat), start = 1, stop = 3)
median_time$lat <- substr((df_final$long_lat), start = 4, stop = 6)

#Retirar as réplicas da base frailty
#(pegar a primeira vez que aparece o valor no id(66))
print(dados_frailty)



###############
#Matriz de correlação
###############


# Instale se ainda não tiver: install.packages("corrplot")
library(corrplot)
library(dplyr)
library(tidyr)

# 1. Selecionar, RENOMEAR e converter para numérico
dados_cor <- dados_frailty %>%
  select(
    time,
    chuva,
    temp,
    porc_queimada,
    sif_mean
  ) %>%
  # Renomeando para nomes profissionais para o gráfico
  rename(
    "Time-to-anomaly" = time,
    "Precipitation" = chuva,
    "Temperature"   = temp,
    "Burned area"   = porc_queimada,
    "SIF"           = sif_mean

  ) %>%
  mutate(across(everything(), ~as.numeric(as.character(.))))

# 2. Cálculo da matriz de correlação
M <- cor(dados_cor, use = "pairwise.complete.obs")

# 3. Gerar o gráfico profissional
corrplot(M,
         method = "color",
         type = "upper",
         order = "original",
         addCoef.col = "black", # Mostra os números da correlação
         tl.col = "black",      # Cor do texto das variáveis
         tl.srt = 45,           # Rotaciona o texto para não encavalar
         diag = FALSE,          # Remove a diagonal (1.0) que é redundante
         mar = c(0,0,2,0))      # Ajuste da margem para o título não cortar











library(corrplot)

# 1. Preparar os dados filtrados
dados_cor <- dados_frailty %>%
  select(time, chuva, temp, porc_queimada, sif_mean) %>%
  rename(
    "Time-to-anomaly" = time,
    "Precipitation" = chuva,
    "Temperature"   = temp,
    "Burned area"   = porc_queimada,
    "SIF"           = sif_mean
  ) %>%
  mutate(across(everything(), ~as.numeric(as.character(.))))

# 2. Cálculo da matriz e p-valores
M <- cor(dados_cor, use = "pairwise.complete.obs")
res1 <- cor.mtest(dados_cor, conf.level = 0.95)

# --- DICA: Rode a linha abaixo para ver os p-valores reais no seu console ---
# print(res1$p)

# 3. Gerar o gráfico
# Aumentamos um pouco as margens para os nomes não cortarem
corrplot(M,
         method = "color",
         type = "upper",
         order = "original",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         mar = c(0,0,2,0))

# 4. Adicionar asteriscos no canto superior direito (Ajustado)
p_mat <- res1$p
n <- ncol(M)

for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    # Verifica se é significativo (p < 0.05)
    if (!is.na(p_mat[i,j]) && p_mat[i,j] < 0.05) {
      # Coordenadas: j é a coluna, (n - i + 1) é a linha
      # O ajuste +0.3 e +0.25 joga o asterisco para a quina da célula
      text(x = j + 0.32, y = n - i + 1.28, labels = "*", cex = 2.5, col = "black")
    }
  }
}




# ==============================================================================
library(ggplot2)
# ==============================================================================
# GRÁFICO 1: Temperature vs Time-to-anomaly (+90 dias)
# ==============================================================================

# 1. Ajuste do modelo linear (usando a mesma variável y do gráfico: Time-to-anomaly + 90)
modelo_temp <- lm(I(`Time-to-anomaly` + 90) ~ Temperature, data = dados_cor)
intercepto <- coef(modelo_temp)[1]
inclinacao <- coef(modelo_temp)[2]

# Texto da equação para exibir no gráfico
eq_label <- sprintf("y = %.3fx + %.3f", inclinacao, intercepto)
cat(eq_label, "\n")

# 2. Gráfico
fig.9 <- ggplot(dados_cor, aes(x = Temperature, y = `Time-to-anomaly` + 90)) +
  # Os pontos azuis
  geom_point(color = "royalblue", size = 2.5, alpha = 0.8) +
  # A linha de tendência linear (vermelha e pontilhada)
  geom_smooth(method = "lm", color = "red", linetype = "dotted", linewidth = 1.2, se = FALSE) +
  # Equação no canto superior direito
  annotate(
    "text",
    x = Inf, y = Inf,
    label = eq_label,
    hjust = 1.05, vjust = 1.5,
    colour = "black", size = 5, fontface = "bold"
  ) +
  # Textos dos eixos (o eixo Y agora refletirá o valor real + 90)
  labs(
    x = "Temperature (°C)",
    y = "Time-to-anomaly (days)"
  ) +
  # Estética idêntica à sua imagem de referência (fundo branco, caixa preta, sem grid)
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  )

print(fig.9)
ggsave("Fig9.png", fig.9, width=10, height=5, dpi=300, units="in", device='png')




# 1. Ajuste do modelo linear
modelo_precip <- lm(Precipitation ~ Temperature, data = dados_cor)
intercepto <- coef(modelo_precip)[1]
inclinacao <- coef(modelo_precip)[2]

# Texto da equação para exibir no gráfico
eq_label <- sprintf("y = %.3fx + %.3f", inclinacao, intercepto)
cat(eq_label, "\n")

# 2. Gráfico
fig.10 <- ggplot(dados_cor, aes(x = Temperature, y = Precipitation)) +
  geom_point(color = "royalblue", size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", color = "red", linetype = "dotted", linewidth = 1.2, se = FALSE) +
  # Equação no canto superior direito
  annotate(
    "text",
    x = Inf, y = Inf,
    label = eq_label,
    hjust = 1.05, vjust = 1.5,
    colour = "black", size = 5, fontface = "bold"
  )+
  labs(
    x = "Temperature (°C)",
    y = "Precipitation (mm)"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  )

print(fig.10)

ggsave("Fig10.png", fig.10, width=10, height=5, dpi=300, units="in", device='png')














###############
# Preparando a base geral e resumo estatistico
###############



library(dplyr)
library(readr)

# 1. Preparar a base de estimativas (median_time)
cc <- dados_frailty$cens == 1
df_median_limpo <- median_time[cc,] %>%
  select(area, time_est) %>%
  mutate(time_est = as.numeric(time_est),
         time_real = as.numeric(df_final$time[cc]),
         erro = time_real - time_est,
         sif_mean = dados_frailty$sif_mean[cc],
         porc_queimada = dados_frailty$porc_queimada[cc],
         chuva = dados_frailty$chuva[cc],
         temp = dados_frailty$temp[cc],
         long = df_final$longitude_t4[cc],
         lat = df_final$latitude_t4[cc],
         cens = dados_frailty$cens[cc],
         year = dados_frailty$year[cc])


library(dplyr)

# Cria o banco novo puxando as colunas inteiras, sem o filtro [cc]
df_median_limpo <- median_time %>%
  select(area, time_est) %>%
  mutate(
    time_est      = as.numeric(time_est),
    time_real     = as.numeric(df_final$time),
    erro          = time_real - time_est,
    sif_mean      = dados_frailty$sif_mean,
    porc_queimada = dados_frailty$porc_queimada,
    chuva         = dados_frailty$chuva,
    temp          = dados_frailty$temp,
    long          = df_final$longitude_t4,
    lat           = df_final$latitude_t4,
    cens          = dados_frailty$cens,
    year          = dados_frailty$year
  )

##################
#Tabela 2 - conta as histórias dos dados (erros) - USAR ESTE
##################

# 2. Gerar os indicadores de performance
resumo_estatistico <- df_median_limpo %>%
  summarise(
    n = n(),
    Erromedioabs = mean(abs(erro), na.rm = TRUE),
    RMSE = sqrt(mean(erro^2, na.rm = TRUE)),
    Erro_Medio = mean(erro, na.rm = TRUE),
    Correlacao = cor(time_real, time_est, use = "complete.obs")
  )

# 3. Exibir os resultados
print(resumo_estatistico)







###########################
library(dplyr)
library(tidyr)
library(gt)
library(purrr)

# 1. Mapeamento das variáveis
labels_map <- c(
  "time_real"        = "Time-to-anomaly",
  "chuva_acumulada"  = "Precipitation",
  "temp_sum"         = "Temperature",
  "porc_queimada"    = "Burned area",
  "sif_mean"         = "SIF"
)

# 2. Cálculo dos P-valores (Teste-T)
p_values <- map_df(names(labels_map), function(col_name) {
  formula_teste <- as.formula(paste(col_name, "~ cens"))
  test <- t.test(formula_teste, data = df_median_limpo)

  data.frame(
    Variable = labels_map[col_name],
    p_value = test$p.value
  )
})

# 3. Processar Média e DP e REORDENAR COLUNAS
df_table <- df_median_limpo %>%
  select(cens, all_of(names(labels_map))) %>%
  group_by(cens) %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  ), .names = "{.col}??{.fn}")) %>%
  pivot_longer(cols = -cens, names_to = c("Variable", "Stat"), names_sep = "\\?\\?") %>%
  pivot_wider(names_from = c(cens, Stat), values_from = value) %>%
  mutate(Variable = recode(Variable, !!!labels_map)) %>%
  left_join(p_values, by = "Variable") %>%
  # PASSO NOVO: Selecionando a ordem das colunas explicitamente (1 antes de 0)
  select(Variable, `1_mean`, `1_sd`, `0_mean`, `0_sd`, p_value)

# 4. Gerar a Tabela GT
tabela_final <- df_table %>%
  gt() %>%
  # Cabeçalhos superiores (A ordem aqui segue a ordem do select acima)
  tab_spanner(label = "Positive anomaly", columns = c(`1_mean`, `1_sd`)) %>%
  tab_spanner(label = "Negative anomaly", columns = c(`0_mean`, `0_sd`)) %>%
  # Rótulos das colunas
  cols_label(
    Variable = "Variable",
    `1_mean` = "Mean",
    `1_sd` = "SD",
    `0_mean` = "Mean",
    `0_sd` = "SD",
    p_value = "P-value"
  ) %>%
  # Formatação acadêmica
  fmt_number(columns = -Variable, decimals = 3) %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    column_labels.font.weight = "bold",
    table.width = pct(100),
    table.font.size = 14
  ) %>%
  cols_align(align = "center", columns = -Variable)

tabela_final
###########################












library(dplyr)
library(tidyr)

# 1. Defina as colunas exatas que você quer consultar no seu df_median_limpo
colunas_interesse <- c("time_real", "chuva", "temp", "porc_queimada", "sif_mean")

# 2. Extrair apenas Min e Max separados por CENS
valores_brutos <- df_median_limpo %>%
  select(cens, all_of(colunas_interesse)) %>%
  group_by(cens) %>%
  summarise(across(everything(), list(
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  ), .names = "{.col}??{.fn}")) %>%

  # Gira os dados para ficar fácil de ler no console (uma variável por linha)
  pivot_longer(cols = -cens, names_to = c("Variavel", "Stat"), names_sep = "\\?\\?") %>%
  pivot_wider(names_from = c(cens, Stat), values_from = value,
              names_prefix = "CENS_") # Adiciona o prefixo para ficar claro

# 3. Imprime o resultado cru no console
print(valores_brutos)













library(dplyr)
library(gt)

# 2. Gerar os indicadores de performance separados por cens (0 e 1)
resumo_estatistico_cens <- df_median_limpo %>%
  filter(cens == 1) %>%
  summarise(
    n = n(),
    Bias = mean(erro, na.rm = TRUE),
    SD = sd(erro, na.rm = TRUE) # Adicionando o Desvio Padrão do erro
  )

# 3. Exibir os resultados em formato de tabela profissional
resumo_estatistico_cens %>%
  gt() %>%
  tab_header(
    title = md("**Performance Indicators for times of positive anomalies**")
  ) %>%
  fmt_number(
    columns = c(Bias, SD), # Ajustado para focar apenas nas variáveis restantes
    decimals = 3
  ) %>%
  tab_options(
    column_labels.font.weight = "bold",
    table.width = pct(90)
  )











library(dplyr)
library(gt)

# 2. Gerar os indicadores de performance separados por cens (0 e 1)
resumo_estatistico_cens <- df_median_limpo %>%
  filter(cens == 1) %>%
  summarise(
    n = n(),
    MAE = mean(abs(erro), na.rm = TRUE),
    RMSE = sqrt(mean(erro^2, na.rm = TRUE)),
    Bias = mean(erro, na.rm = TRUE),
    Correlation = cor(time_real, time_est, use = "complete.obs")
  )


# 3. Exibir os resultados em formato de tabela profissional
resumo_estatistico_cens %>%
  gt() %>%
  tab_header(
    title = md("**Performance Indicators for times of positive anomalies**")
  ) %>%
  fmt_number(
    columns = c(MAE, RMSE, Bias, Correlation),
    decimals = 3
  ) %>%
  tab_options(
    column_labels.font.weight = "bold",
    table.width = pct(90)
  )






library(gt)
library(dplyr)

# 1. Formatting the summary table with 3 decimal places
performance_table <- resumo_estatistico %>%
  gt() %>%
  # Header and Subtitle for the defense
  tab_header(
    title = md("**Survival Model Performance Metrics**")
  ) %>%
  # Renaming columns to standard international terminology
  cols_label(
    n = "Observations (n)",
    Erromedioabs = "Mean Absolute Error (MAE)",
    RMSE = "Root Mean Square Error (RMSE)",
    Erro_Medio = "Mean Error (Bias)",
    Correlacao = "Pearson Correlation (r)"
  ) %>%
  # AJUSTE: Formatação para 3 casas decimais
  fmt_number(
    columns = c(Erromedioabs, RMSE, Erro_Medio, Correlacao),
    decimals = 3
  ) %>%
  # Visual styling to highlight correlation
  tab_options(
    table.width = pct(85),
    heading.title.font.size = 22,
    column_labels.font.weight = "bold",
    table.font.size = 16
  )

# Display the table
performance_table








###############
# Gráfico entre real x estimado
###############



library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Preparar os dados para o formato 'longo'
# Isso permite criar todos os gráficos de uma vez com o facet_wrap
df_diagnostico <- df_median_limpo %>%
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


###############
# Validação do modelo real x estimado - USAR ESTE
###############


# 1. Preparar a base median_time (pegando apenas o necessário)
# df_median_limpo %>%
#   select(area, time_est) %>%
#   mutate(time_est = as.numeric(time_est)) # Garante que a estimativa seja numérica


# 1. Gerar o gráfico sem interrupções na "corrente" de +
ggplot(df_median_limpo %>%
         filter(cens == 1),
       aes(x = time_real, y = time_est)) +
  xlim(80,220) +
  ylim(80,220) +
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
modelo_linear <- lm(time_est ~ time_real, data = df_median_limpo %>%
                      filter(cens == 1))
r2_valor <- summary(modelo_linear)$r.squared
message("O R² do seu modelo é: ", round(r2_valor, 3))











#########################
# Se quiser trazer a estatistica geral das variaveis

library(dplyr)
library(tidyr)
library(gt)
library(purrr)

# 1. Mapeamento
labels_map <- c(
  "time_real" = "Time-to-anomaly (days)",
  "sif_mean" = "Solar-Induced Fluorescence (SIF)",
  "porc_queimada" = "Burned Area Intensity (Log)",
  "chuva" = "Accumulated Precipitation (mm)",
  "temp" = "Accumulated Temperature (°C)"
)

# 2. Calcular P-valores (Wilcoxon) - Versão Corrigida
p_values <- map_df(names(labels_map), function(var) {

  # Criamos a fórmula dinamicamente usando o nome da variável (texto)
  formula_wilcox <- as.formula(paste(var, "~ cens"))

  # Rodamos o teste passando o dataframe no argumento 'data'
  test <- wilcox.test(formula_wilcox, data = df_median_limpo)

  # Retornamos o data frame com o nome limpo e o p-value
  data.frame(
    Variable = labels_map[var],
    p_value = test$p.value
  )
})

# 3. Processar estatísticas em formato compacto
df_stats_compact <- df_median_limpo %>%
  select(cens, all_of(names(labels_map))) %>%
  pivot_longer(-cens, names_to = "Variable") %>%
  mutate(Variable = recode(Variable, !!!labels_map)) %>%
  group_by(Variable, cens) %>%
  summarise(
    mean_sd = paste0(format(round(mean(value, na.rm=TRUE), 3), nsmall=3),
                     " (±", format(round(sd(value, na.rm=TRUE), 3), nsmall=3), ")"),
    med_iqr = paste0(format(round(median(value, na.rm=TRUE), 3), nsmall=3),
                     " [", format(round(quantile(value, 0.25, na.rm=TRUE), 3), nsmall=3),
                     "-", format(round(quantile(value, 0.75, na.rm=TRUE), 3), nsmall=3), "]"),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = cens, values_from = c(mean_sd, med_iqr)) %>%
  left_join(p_values, by = "Variable")

# 4. Gerar Tabela GT "Legal"
tabela_legal <- df_stats_compact %>%
  gt() %>%
  tab_header(
    title = md("**Environmental Covariates Summary**"),
    subtitle = "Comparison between Coldspots and Hotspots"
  ) %>%
  # Organizar Colunas
  cols_move_to_start(columns = Variable) %>%
  cols_label(
    mean_sd_0 = md("Mean (±SD)"),
    med_iqr_0 = md("Median [Q1-Q3]"),
    mean_sd_1 = md("Mean (±SD)"),
    med_iqr_1 = md("Median [Q1-Q3]"),
    p_value = "P-value"
  ) %>%
  # Criar Spanners (Cabeçalhos superiores)
  tab_spanner(label = "Coldspot (0)", columns = c(mean_sd_0, med_iqr_0)) %>%
  tab_spanner(label = "Hotspot (1)", columns = c(mean_sd_1, med_iqr_1)) %>%
  # Estética
  fmt_number(columns = p_value, decimals = 4) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = p_value, rows = p_value < 0.10)
  ) %>%
  tab_options(
    table.width = pct(100),
    column_labels.font.weight = "bold",
    table.font.size = 13,
    heading.title.font.size = 18
  )

tabela_legal











# #####################
#Mapa de bolas


# library(ggplot2)
library(dplyr)

# 1. Filtrar o quadrante e os anos de interesse (2020-2023)
df_mapa_anos <- df_median_limpo %>%
  filter(long >= -59.7700 & long <= -49.8361,
         lat >= -12.3561 & lat <= -1.6058,
         year %in% c(2020, 2021, 2022, 2023, 2024))

# 2. Gerar o gráfico focado em preenchimento e legenda lateral
mapa_final_lateral <- ggplot(df_mapa_anos, aes(x = long, y = lat)) +
  # As bolas: tamanho pelo tempo e cor pelo tipo (cens)
  geom_point(aes(size = time_real, color = as.factor(cens)), alpha = 0.6) +

  # Cores personalizadas (Azul para Negativa, Vermelho para Positiva)
  scale_color_manual(values = c("0" = "steelblue2", "1" = "red2"),
                     labels = c("Negative (coldspot)", "Positive (hotspot)"),
                     name = "Anomaly type") +

  # Escala de tamanho forçando o 90 e limites claros
  scale_size_continuous(name = "Time-to-anomaly (in days)",
                        range = c(2, 10),
                        breaks = c(90, 110, 130, 150, 170, 190),
                        limits = c(90, 190)) +

  # Separar por ano (2 colunas x 2 linhas)
  facet_wrap(~year, ncol = 2) +

  # AJUSTE DE ESPAÇO: quickmap otimiza a área útil do gráfico
  coord_quickmap(xlim = c(-59.7700, -49.8361),
                 ylim = c(-12.3561, -1.6058)) +

  # Estética limpa e legenda na lateral
  theme_bw() +
  theme(legend.position = "right", # Legenda na lateral direita
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray20"),
        strip.text = element_text(color = "white", face = "bold", size = 14),
        panel.spacing = unit(1, "lines"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")) +

  labs(x = "Longitude", y = "Latitude")

# Adicionando ajustes de escala de legenda ao seu gráfico
mapa_final_lateral <- mapa_final_lateral +
  theme(
    # 1. Aumenta o título da legenda (Anomaly type, Time-to-anomaly)
    legend.title = element_text(size = 15, face = "bold"),

    # 2. Aumenta o texto das categorias (Negative, Positive, Dias)
    legend.text = element_text(size = 13),

    # 3. Aumenta o espaço entre os itens da legenda para não embolar
    legend.key.height = unit(1.2, "cm"),
    legend.spacing.y = unit(0.4, "cm")
  ) +
  # 4. Força os pontos na legenda a ficarem maiores (override.aes)
  # Isso evita que o ponto da legenda fique minúsculo enquanto o texto é grande
  guides(
    color = guide_legend(override.aes = list(size = 6)),
    size = guide_legend(override.aes = list(color = "gray30"))
  )

# Visualizar e salvar novamente com os novos tamanhos
print(mapa_final_lateral)






# ==============================================================================
# 1. FILTRAR E CATEGORIZAR OS DADOS (Agora com apenas 3 tamanhos)
# ==============================================================================
df_mapa_anos <- df_median_limpo %>%
  filter(long >= -59.7700 & long <= -49.8361,
         lat >= -12.3561 & lat <= -1.6058,
         year %in% c(2020, 2021, 2022, 2023, 2024)) %>%
  mutate(
    time_cat = case_when(
      time_real >= 90  & time_real <= 120 ~ "90 - 120",
      time_real > 120  & time_real <= 150 ~ "121 - 150",
      time_real > 150  & time_real <= 190 ~ "151 - 190",
      TRUE ~ NA_character_
    ),
    time_cat = factor(time_cat, levels = c("90 - 120", "121 - 150", "151 - 190"))
  ) %>%
  filter(!is.na(time_cat))

# ==============================================================================
# 2. GERAR O GRÁFICO (Com 3 Tamanhos Manuais)
# ==============================================================================
mapa_final_lateral <- ggplot(df_mapa_anos, aes(x = long, y = lat)) +
  geom_point(aes(size = time_cat, color = as.factor(cens)), alpha = 0.6) +
  scale_color_manual(values = c("0" = "steelblue2", "1" = "red2"),
                     labels = c("Negative (coldspot)", "Positive (hotspot)"),
                     name = "Anomaly type") +
  scale_size_manual(
    name = "Time-to-anomaly (in days)",
    values = c("90 - 120"  = 3.0,
               "121 - 150" = 6.0,
               "151 - 190" = 9.5)
  ) +
  facet_wrap(~year, ncol = 2, labeller = as_labeller(c(
    "2021" = "2020/Q4 - 2021/Q1",
    "2022" = "2021/Q4 - 2022/Q1",
    "2023" = "2022/Q4 - 2023/Q1",
    "2024" = "2023/Q4 - 2024/Q1"
  ))) +
  coord_quickmap(xlim = c(-59.7700, -49.8361),
                 ylim = c(-12.3561, -1.6058)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray20"),
        strip.text = element_text(color = "white", face = "bold", size = 11),  # reduzido de 14 para 9
        panel.spacing = unit(0.6, "lines"),   # reduzido de 1 para 0.6 (mais espaço útil para os mapas)
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +  # margens menores
  labs(x = "Longitude", y = "Latitude")

# ==============================================================================
# 3. AJUSTES FINAIS DA LEGENDA
# ==============================================================================
fig.11 <- mapa_final_lateral +
  theme(
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.box = "vertical",
    legend.spacing.y = unit(0.1, "cm")
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1),
    size = guide_legend(override.aes = list(color = "gray30"), order = 2)
  )

# Exibe o mapa final
print(fig.11)

# ==============================================================================
# 4. EXPORTAR EM TAMANHO MAIOR (mapas ficam visualmente maiores no arquivo final)
# ==============================================================================
ggsave(
  filename = "Figura_11.png",
  plot = fig.11,
  width = 21,
  height = 19,
  units = "cm",
  dpi = 300,
  bg = "white"
)








library(ggplot2)
library(dplyr)

# ==============================================================================
# 1. FILTRAR E CATEGORIZAR OS DADOS (Agora com apenas 3 tamanhos)
# ==============================================================================
df_mapa_anos <- df_median_limpo %>%
  filter(long >= -59.7700 & long <= -49.8361,
         lat >= -12.3561 & lat <= -1.6058,
         year %in% c(2020, 2021, 2022, 2023, 2024)) %>%

  # Cria a nova coluna com as 3 categorias de tempo
  mutate(
    time_cat = case_when(
      time_real >= 90  & time_real <= 120 ~ "90 - 120",
      time_real > 120  & time_real <= 150 ~ "121 - 150",
      time_real > 150  & time_real <= 190 ~ "151 - 190",
      TRUE ~ NA_character_
    ),
    # Transforma em fator para garantir a ordem correta na legenda
    time_cat = factor(time_cat, levels = c("90 - 120", "121 - 150", "151 - 190"))
  ) %>%
  filter(!is.na(time_cat))


# ==============================================================================
# 2. GERAR O GRÁFICO (Com 3 Tamanhos Manuais)
# ==============================================================================
mapa_final_lateral <- ggplot(df_mapa_anos, aes(x = long, y = lat)) +

  geom_point(aes(size = time_cat, color = as.factor(cens)), alpha = 0.6) +

  scale_color_manual(values = c("0" = "steelblue2", "1" = "red2"),
                     labels = c("Negative (coldspot)", "Positive (hotspot)"),
                     name = "Anomaly type") +

  # AQUI: Definindo os 3 tamanhos de bolinhas
  scale_size_manual(
    name = "Time-to-anomaly (in days)",
    values = c("90 - 120"  = 3.0,  # Bolinha pequena
               "121 - 150" = 6.0,  # Bolinha média
               "151 - 190" = 9.5)  # Bolinha grande
  ) +

  # Substitua a linha: facet_wrap(~year, ncol = 2) +
  # Por esta:

  facet_wrap(~year, ncol = 2, labeller = as_labeller(c(
    "2021" = "2020 - 2021",
    "2022" = "2021 - 2022",
    "2023" = "2022 - 2023",
    "2024" = "2023 - 2024"
  ))) +

  coord_quickmap(xlim = c(-59.7700, -49.8361),
                 ylim = c(-12.3561, -1.6058)) +

  theme_bw() +
  theme(legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray20"),
        strip.text = element_text(color = "white", face = "bold", size = 14),
        panel.spacing = unit(1, "lines"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")) +

  labs(x = "Longitude", y = "Latitude")


# ==============================================================================
# 3. AJUSTES FINAIS DA LEGENDA
# ==============================================================================
fig.11 <- mapa_final_lateral +
  theme(
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 13),
    legend.key.height = unit(1.2, "cm"),
    legend.spacing.y = unit(0.4, "cm")
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 6), order = 1),
    size = guide_legend(override.aes = list(color = "gray30"), order = 2)
  )

# Exibe o mapa final
print(fig.11)

ggsave("Fig11.png", fig.11, width=10, height=5, dpi=300, units="in", device='png')




















##### VARIFICAÇÃO DA DISTRIBUIÇÃO DAS COVARIAVEIS - NORMAL OU NÃO #####

library(tidyverse)
library(nortest) # Para testes de normalidade em grandes amostras

# 1. Selecionar apenas as variáveis numéricas de interesse
vars_analise <- df_median_limpo %>%
  select(time_real, sif_mean, porc_queimada, chuva_acumulada, temp_sum)

# 2. Teste Estatístico (Shapiro-Wilk com amostragem de 5000)
# Como você tem 108k linhas, o Shapiro padrão não roda no vetor inteiro.
normality_stats <- vars_analise %>%
  map_df(~{
    res <- shapiro.test(sample(na.omit(.x), 108))
    data.frame(p_value = res$p.value)
  }, .id = "Variable") %>%
  mutate(Distribution = ifelse(p_value < 0.05, "Non-Normal", "Normal"))

print(normality_stats)

# 3. Inspeção Visual (O "padrão ouro" para grandes dados)
# Criando Histogramas para todas as variáveis
vars_analise %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  ggplot(aes(x = val)) +
  geom_histogram(aes(y = ..density..), fill = "steelblue", bins = 30, alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~var, scales = "free") +
  theme_minimal() +
  labs(title = "Distribuição das Variáveis (Histograma + Densidade)")

# 4. Q-Q Plot (Se os pontos seguirem a linha, é normal)
vars_analise %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  ggplot(aes(sample = val)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~var, scales = "free") +
  theme_light() +
  labs(title = "Q-Q Plots: Comparação com a Distribuição Normal")
