###############################################################
# DISCIPLINA: ME623 
# AUTORA: Nicole de Barros Silva
# PROJETO 2 – Análise Estatística e Modelagem do Rendimento de um Processo Químico 
# sob Diferentes Condições de Temperatura e Pressão
#
# DESCRIÇÃO:
# Este script realiza a análise completa do conjunto de dados obtidos no Problema 5.8 da obra
# Design and Analysis of Experiments (Montgomery), incluindo:
# - Validação e consistência dos dados;
# - Análise descritiva;
# - Análise exploratória gráfica;
# - Ajuste de modelo de regressão quadrática;
# - Análise inferencial;
# - Diagnóstico dos resíduos;
# - Avaliação da qualidade do ajuste;
# - Geração de tabelas e gráficos utilizados no relatório.
#
# INSTRUÇÕES DE USO:
# 1. Selecione e execute o código sequencialmente (Ctrl + Enter).
# 2. Os arquivos .png serão salvos no diretório de trabalho.
# 3. Nenhuma modificação é necessária para reprodução dos resultados.
###############################################################

# Pacotes necessários

if (!requireNamespace("knitr", quietly = TRUE))
  install.packages("knitr")

if (!requireNamespace("kableExtra", quietly = TRUE))
  install.packages("kableExtra")

if (!requireNamespace("ggplot2", quietly = TRUE))
  install.packages("ggplot2")

if (!requireNamespace("lmtest", quietly = TRUE))
  install.packages("lmtest")

if (!requireNamespace("viridis", quietly = TRUE))
  install.packages("viridis")

if (!requireNamespace("webshot2", quietly = TRUE))
  install.packages("webshot2")

library(knitr)
library(kableExtra)
library(ggplot2)
library(lmtest)
library(viridis)
library(webshot2)

# Garantir reprodutibilidade do ambiente
rm(list = ls())
set.seed(123)

# ==============================================================================
# 1. BANCO DE DADOS
# ==============================================================================

dados_processo <- data.frame(
  
  Temperatura = c(
    rep(150, 6),
    rep(160, 6),
    rep(170, 6)
  ),
  
  Pressao = c(
    200, 215, 230, 200, 215, 230,
    200, 215, 230, 200, 215, 230,
    200, 215, 230, 200, 215, 230
  ),
  
  Rendimento = c(
    90.4, 90.7, 90.2, 90.2, 90.6, 90.4,
    90.1, 90.5, 89.9, 90.3, 90.6, 90.1,
    90.5, 90.8, 90.4, 90.7, 90.9, 90.1
  )
)

# ==============================================================================
# 2. VERIFICAÇÃO
# ==============================================================================

# Dimensão do banco
dim(dados_processo)

# Estrutura das variáveis
str(dados_processo)

# Nomes das variáveis
names(dados_processo)

# Verificação de valores ausentes
sum(is.na(dados_processo))

# Valores ausentes por variável
colSums(is.na(dados_processo))

# Visualização do banco
dados_processo

# ==============================================================================
# 3. ESTATÍSTICAS DESCRITIVAS
# ==============================================================================

# Resumo estatístico geral
summary(dados_processo)

# Médias
colMeans(dados_processo)

# Desvios padrão
apply(dados_processo, 2, sd)

# Variâncias
apply(dados_processo, 2, var)

# Tabela de estatísticas descritivas
tabela_resumo <- data.frame(
  Variável = c("Temperatura", "Pressão", "Rendimento"),
  Média = c(
    mean(dados_processo$Temperatura),
    mean(dados_processo$Pressao),
    mean(dados_processo$Rendimento)
  ),
  `Desvio Padrão` = c(
    sd(dados_processo$Temperatura),
    sd(dados_processo$Pressao),
    sd(dados_processo$Rendimento)
  ),
  Variância = c(
    var(dados_processo$Temperatura),
    var(dados_processo$Pressao),
    var(dados_processo$Rendimento)
  ),
  Mínimo = c(
    min(dados_processo$Temperatura),
    min(dados_processo$Pressao),
    min(dados_processo$Rendimento)
  ),
  Mediana = c(
    median(dados_processo$Temperatura),
    median(dados_processo$Pressao),
    median(dados_processo$Rendimento)
  ),
  Máximo = c(
    max(dados_processo$Temperatura),
    max(dados_processo$Pressao),
    max(dados_processo$Rendimento)
  )
)
print(tabela_resumo)

# Tabela 1 (formatada)
kable(
  tabela_resumo,
  digits = 2,
  align = "c"
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#D9D9D9") %>%
  column_spec(1, bold = TRUE, background = "#EDEDED")

tab1 <- kable(
  tabela_resumo,
  digits = 2,
  align = "c"
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#D9D9D9") %>%
  column_spec(1, bold = TRUE, background = "#EDEDED")

save_kable(
  tab1,
  file = "Tabela_Estatisticas_Descritivas.png"
)


# ==============================================================================
# 4. ANÁLISE DAS COMBINAÇÕES EXPERIMENTAIS
# ==============================================================================

# Número de observações por combinação
table(
  dados_processo$Temperatura,
  dados_processo$Pressao
)

# Média do rendimento por temperatura
media_temp <- aggregate(
  Rendimento ~ Temperatura,
  data = dados_processo,
  FUN = mean
)
print(media_temp)

# Arredondar para duas casas
media_temp$Rendimento <- round(media_temp$Rendimento, 2)

# Tabela 2 (formatada)
tab2 <- kable(
  media_temp,
  digits = 2,
  align = "c",
  col.names = c("Temperatura", "Rendimento Médio")
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    background = "#D9D9D9"
  ) %>%
  column_spec(
    1,
    bold = TRUE,
    background = "#EDEDED"
  )

save_kable(
  tab2,
  file = "Media_Rendimento_Temperatura.png"
)

# Média do rendimento por pressão
media_pressao <- aggregate(
  Rendimento ~ Pressao,
  data = dados_processo,
  mean
)
print(media_pressao)

# Arredondar para duas casas decimais
media_pressao$Rendimento <- round(media_pressao$Rendimento, 2)

# Tabela 3 (formatada)
tab3 <- kable(
  media_pressao,
  digits = 2,
  align = "c",
  col.names = c("Pressão", "Rendimento Médio")
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    background = "#D9D9D9"
  ) %>%
  column_spec(
    1,
    bold = TRUE,
    background = "#EDEDED"
  )

save_kable(
  tab3,
  file = "Media_Rendimento_Pressao.png"
)

# Média do rendimento por combinação experimental
aggregate(
  Rendimento ~ Temperatura + Pressao,
  data = dados_processo,
  mean
)

# ==============================================================================
# 5. MATRIZ DE CORRELAÇÃO
# ==============================================================================

# Matriz de correlação
matriz_cor <- cor(
  dados_processo[, c("Temperatura", "Pressao", "Rendimento")]
)

# Converter para data.frame
tabela_cor <- data.frame(
  Variavel = rownames(matriz_cor),
  round(matriz_cor, 2),
  row.names = NULL
)
print(tabela_cor)

# Tabela 4 (formatada)
tab4 <- kable(
  tabela_cor,
  digits = 2,
  align = "c",
  col.names = c(
    "Variável",
    "Temperatura",
    "Pressão",
    "Rendimento"
  )
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    background = "#D9D9D9"
  ) %>%
  column_spec(
    1,
    bold = TRUE,
    background = "#EDEDED"
  )

save_kable(
  tab4,
  file = "Matriz_Correlacao.png"
)

# ==============================================================================
# 6. ANÁLISE EXPLORATÓRIA GRÁFICA
# ==============================================================================

# Boxplot do Rendimento
g1 <- ggplot(dados_processo, aes(x = "", y = Rendimento)) +
  geom_boxplot(fill = "#A8DADC", color = "#457B9D", width = 0.5) +
  geom_jitter(color = "#1D3557", width = 0.1, alpha = 0.5) +
  labs(title = "Distribuição Geral do Rendimento", x = "", y = "Rendimento (%)") +
  theme_minimal()

print(g1)

# Boxplot de Rendimento por Temperatura
g2 <- ggplot(dados_processo, aes(x = as.factor(Temperatura), y = Rendimento, fill = as.factor(Temperatura))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#E9C46A", "#F4A261", "#E76F51")) +
  labs(title = "Rendimento por Temperatura", x = "Temperatura (°C)", y = "Rendimento (%)") +
  theme_classic() +
  theme(legend.position = "none")

print(g2)

# Boxplot de Rendimento por Pressão
g3 <- ggplot(dados_processo, aes(x = as.factor(Pressao), y = Rendimento, fill = as.factor(Pressao))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#8ECAE6", "#219EBC", "#023047")) +
  labs(title = "Rendimento por Pressão", x = "Pressão (psig)", y = "Rendimento (%)") +
  theme_classic() +
  theme(legend.position = "none")

print(g3)


# ==============================================================================
# 7. GRÁFICO DE INTERAÇÃO
# ==============================================================================

df_media <- aggregate(Rendimento ~ Temperatura + Pressao, data = dados_processo, mean)

g4 <- ggplot(df_media, aes(x = as.factor(Temperatura), y = Rendimento, 
                           color = as.factor(Pressao), group = Pressao)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "D", name = "Pressão (psig)") +
  labs(title = "Gráfico de Interação", 
       subtitle = "Efeito da Temperatura e Pressão no Rendimento Médio",
       x = "Temperatura (°C)", y = "Rendimento Médio (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g4)

# ==============================================================================
# 8. AJUSTE DO MODELO DE REGRESSÃO QUADRÁTICO
# ==============================================================================

# Modelo quadrático com:
# - efeitos lineares
# - termos quadráticos
# - interação entre temperatura e pressão

modelo_quad <- lm(
  Rendimento ~ Temperatura + Pressao +
    I(Temperatura^2) + I(Pressao^2) +
    Temperatura:Pressao,
  data = dados_processo
)

# ==============================================================================
# 9. RESUMO DO MODELO
# ==============================================================================

summary(modelo_quad)

# Tabela dos coeficientes do modelo quadrático
tabela_coef <- as.data.frame(summary(modelo_quad)$coefficients)
tabela_coef$Coeficiente <- rownames(tabela_coef)
rownames(tabela_coef) <- NULL
colnames(tabela_coef) <- c("Estimativa", "Erro Padrão", "Estatística t", "p-valor", "Coeficiente")
tabela_coef <- tabela_coef[, c(5, 1, 2, 3, 4)]
print(tabela_coef)

# Tabela 5 (formatada)
tab5 <- kable(
  tabela_coef,
  digits = 5,
  align = "c"
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    background = "#D9D9D9"
  ) %>%
  column_spec(
    1,
    bold = TRUE,
    background = "#EDEDED"
  )

save_kable(
  tab5,
  file = "Coeficientes_Modelo_Quadratico.png"
)

# ==============================================================================
# 10. ANÁLISE DE VARIÂNCIA (ANOVA)
# ==============================================================================

anova(modelo_quad)

# Tabela ANOVA
tabela_anova <- as.data.frame(anova(modelo_quad))
tabela_anova$Fonte <- rownames(tabela_anova)
rownames(tabela_anova) <- NULL

tabela_anova <- tabela_anova[, c(6, 1, 2, 3, 4, 5)]
print(tabela_anova)

# Tabela 6
tab6 <- kable(
  tabela_anova,
  digits = c(0, 0, 5, 5, 4, 5),
  align = "c"
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    background = "#D9D9D9"
  ) %>%
  column_spec(
    1,
    bold = TRUE,
    background = "#EDEDED"
  )

save_kable(
  tab6,
  file = "Tabela_ANOVA.png"
)

# ==============================================================================
# 11. COEFICIENTES DO MODELO
# ==============================================================================

# Coeficientes estimados
coef(modelo_quad)

# Intervalos de confiança dos coeficientes
ic <- confint(modelo_quad)

# Tabela dos intervalos de confiança dos coeficientes

tabela_ic <- as.data.frame(ic)
tabela_ic$Coeficiente <- rownames(tabela_ic)
rownames(tabela_ic) <- NULL
colnames(tabela_ic) <- c("Limite Inferior (2.5%)", "Limite Superior (97.5%)", "Coeficiente")
tabela_ic <- tabela_ic[, c(3, 1, 2)]
print(tabela_ic)

# Tabela 7
tab7 <- kable(
  tabela_ic,
  digits = 5,
  align = "c",
  col.names = c(
    "Coeficiente",
    "Limite Inferior (2,5%)",
    "Limite Superior (97,5%)"
  )
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    background = "#D9D9D9"
  ) %>%
  column_spec(
    1,
    bold = TRUE,
    background = "#EDEDED"
  )

save_kable(
  tab7,
  file = "Intervalos_Confianca_Coeficientes.png"
)

# ==============================================================================
# 12. VALORES AJUSTADOS E RESÍDUOS
# ==============================================================================

dados_processo$Valores_Ajustados <- fitted(modelo_quad)

dados_processo$Residuos <- residuals(modelo_quad)

dados_processo

# ==============================================================================
# 13. GRÁFICOS DIAGNÓSTICOS
# ==============================================================================

# Gráficos diagnósticos ajustados para o relatório
g5 <- ggplot(
  data.frame(
    Ajustados = fitted(modelo_quad),
    Residuos = residuals(modelo_quad)
  ),
  aes(x = Ajustados, y = Residuos)
) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    title = "Resíduos versus Valores Ajustados",
    x = "Valores Ajustados",
    y = "Resíduos"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

print(g5)

g6 <- ggplot(
  data.frame(
    Residuos = residuals(modelo_quad)
  ),
  aes(sample = Residuos)
) +
  stat_qq(size = 3) +
  stat_qq_line(linewidth = 0.8) +
  labs(
    title = "Gráfico Q-Q dos Resíduos",
    x = "Quantis Teóricos",
    y = "Quantis Amostrais"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

print(g6)

# ==============================================================================
# 14. TESTE DE NORMALIDADE DOS RESÍDUOS
# ==============================================================================

shapiro.test(residuals(modelo_quad))

# ==============================================================================
# 15. TESTE DE HOMOCEDASTICIDADE
# ==============================================================================

bptest(modelo_quad)

# ==============================================================================
# 16. RESÍDUOS PADRONIZADOS
# ==============================================================================

rstandard(modelo_quad)

# ==============================================================================
# 17. DISTÂNCIA DE COOK
# ==============================================================================

cooks.distance(modelo_quad)

# ==============================================================================
# 18. VALORES DE ALAVANCAGEM
# ==============================================================================

hatvalues(modelo_quad)

# ==============================================================================
# 19. ANÁLISE DOS VALORES PREDITOS
# ==============================================================================

# Comparação entre valores observados e ajustados
g7 <- ggplot(dados_processo, aes(x = Rendimento, y = Valores_Ajustados)) +
  geom_point(size = 3, color = "#264653") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Observados vs. Ajustados",
       x = "Valores Reais (Rendimento)", y = "Valores Preditos pelo Modelo") +
  theme_light()

print(g7)

# ==============================================================================
# 20. GRÁFICO DE CONTORNO
# ==============================================================================

# Grade de predição

grid <- expand.grid(
  Temperatura = seq(150, 170, length.out = 100),
  Pressao = seq(200, 230, length.out = 100)
)

# Valores preditos pelo modelo quadrático
grid$Rendimento <- predict(
  modelo_quad,
  newdata = grid
)

g8 <- ggplot(
  grid,
  aes(
    x = Temperatura,
    y = Pressao,
    z = Rendimento
  )
) +
  
  geom_contour_filled(bins = 10) +
  
  geom_contour(
    color = "grey30",
    linewidth = 0.3
  ) +
  
  geom_point(
    data = dados_processo,
    aes(
      x = Temperatura,
      y = Pressao
    ),
    inherit.aes = FALSE,
    size = 2,
    color = "black"
  ) +
  
  scale_fill_viridis_d(
    option = "cividis",
    direction = 1
  ) +
  
  labs(
    x = "Temperatura (°C)",
    y = "Pressão (psig)",
    fill = "Rendimento (%)"
  ) +
  
  theme_classic(base_size = 13)

print(g8)
