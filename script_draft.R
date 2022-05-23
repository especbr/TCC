DataExplorer::create_report(
  df, 
  output_file = "report6.html")
# Usar quando quiser gerar um report do DF

df %>%
  filter(preco_unitario > 1,
         preco_unitario <= 1800000) %>% 
  ggplot(aes(y = preco_unitario)) +
  geom_boxplot(fill = "seagreen3") +
  coord_cartesian(ylim = c(0, 2000000)) +
  theme_classic() +
  ylab("Preço unitario") +
  scale_y_continuous(breaks = seq(0, 10000000, 200000))

df %>%
  filter(preco_unitario > 1,
         preco_unitario <= 1800000) %>% 
  ggplot(aes(x = preco_unitario)) +
  geom_histogram(aes(y = ..density..), fill = "seagreen3",
                 color = "chartreuse4", binwidth = 10) +
  geom_density(size = 1) +
  theme_classic() +
  ylab("Preço unitario") +
  scale_x_continuous(breaks = seq(0, 10000000, 200000))
  
df %>% 
  filter(preco_unitario <= 1) %>% 
  View()

69882 + 1.5*IQR(df$preco_unitario)

df %>% 
  filter(qtd <= 30) %>% 
  ggplot(aes(y = qtd)) +
  geom_boxplot(fill = "seagreen3") +
  coord_cartesian(ylim = c(0, 30)) +
  theme_classic() +
  ylab("Quantidade solicitada") +
  scale_y_continuous(breaks = seq(0, 30, 5))

df %>% 
  filter(qtd > 30) %>% 
  View()
  
7 + 1.5*IQR(df$qtd)


ggplot(a, aes(y = preco_unitario)) +
  geom_boxplot(fill = "seagreen3") +
  coord_cartesian(ylim = c(0, 1000000)) +
  theme_classic() +
  ylab("Preço Estimado") +
  scale_y_continuous(breaks = seq(0, 10000000, 200000))


ab <- glm(zero ~ painel_selecao, a, family = "binomial")
summary(ab)

standard_error <- function(x) sd(x) / sqrt(length(x))

abc <- itens_de_oportunidade %>% 
  group_by(categoria) %>% 
  summarise(valor = standard_error(preco_estimado)) %>% 
  View("SE")

ggplot(itens_de_oportunidade, aes(preco_estimado)) + 
  geom_histogram() +
  facet_wrap(~categoria, scales = "free")
  


oportunidades_resumo %>% 
  group_by(oportunidade) %>% 
  summarise(n()) %>% 
  View()


modelo_nulo_hlm2 <- lme(fixed = lances ~ 1,
                        random = ~ 1 | oportunidade, 
                        data = df_ok,
                        method = "REML")

summary(modelo_nulo_hlm2)

stderr_nlme(modelo_nulo_hlm2)

modelo_ols_nulo <- lm(formula = lances ~ 1,
                      data = df_ok)

summary(modelo_ols_nulo)

lrtest(modelo_ols_nulo, modelo_nulo_hlm2)


#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


df_ok %>% 
  group_by(categoria) %>% 
  summarise(lances = mean(lances)) %>%
  mutate(categoria = fct_reorder(categoria, lances)) %>% 
  ggplot() +
  aes(lances, categoria) +
  geom_point()



df_ok %>% 
  group_by(unidade_pb) %>% 
  summarise(lances = mean(lances)) %>%
  mutate(unidade_pb = fct_reorder(unidade_pb, lances)) %>% 
  ggplot() +
  aes(lances, unidade_pb) +
  geom_point()


modelo_intercept_hlm2 <- lme(fixed = lances ~ qtd + preco_unitario,
                             random = ~ 1 | oportunidade,
                             data = df_ok,
                             method = "REML")
summary(modelo_intercept_hlm2)

stderr_nlme(modelo_intercept_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

lrtest(modelo_nulo_hlm2, modelo_intercept_hlm2)


#Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm2 <- lme(fixed = lances ~ qtd + preco_unitario,
                                    random = ~ qtd | oportunidade,
                                    data = df_ok,
                                    method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_inclin_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

lrtest(modelo_intercept_hlm2, modelo_intercept_inclin_hlm2)


#Estimação do modelo final
modelo_final_hlm2 <- lme(fixed = lances ~ qtd + preco_unitario + qtd_forn_notif + qtd:qtd_forn_notif,
                         random = ~ qtd + preco_unitario | oportunidade,
                         data = df_ok,
                         method = "REML")

#Parâmetros do modelo
summary(modelo_final_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_final_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4,
         `HLM2 Modelo Final` = 5) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3",
                               "deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

lrtest(modelo_intercept_inclin_hlm2, modelo_final_hlm2)


# Modelagem de Teste #########  

# Tirando notação científica
options(scipen = 999)

# Rodando modelo linear
# Aqui um linear usando apenas as variáveis de nível 1 (item)

# modelo <- lm(lances ~ qtd + preco_unitario + unidades_pb (dummy), df_ok)
modelo_linear_item <- lm(lances ~ ., df_ok[c(3:5, 11, 13:31)])

# Obtendo summary do modelo
summary(modelo_linear_item)

# Criando stepwise do modelo linear por itens
modelo_linear_item_step <- step(modelo_linear_item, k = 3.841459)

# Summary do modelo stepwise
summary(modelo_linear_item_step)

# Nestes resultados preliminares, o R^2 reduziu com o modelo stepwise
# Entretanto, usaremos este modelo

# Precisamos fazer o teste de Shapiro-Francia no resíduos do modelo
sf.test(modelo_linear_item_step$residuals)

#Plotando os novos resíduos do modelo step_bc_planosaude com curva normal teórica
df_ok %>%
  mutate(residuos = modelo_linear_item_step$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_linear_item_step$residuals),
                            sd = sd(modelo_linear_item_step$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

# Kernel density estimation (KDE)
df_ok %>%
  ggplot() +
  geom_density(aes(x = modelo_linear_item_step$residuals), fill = "#440154FF") +
  labs(x = "Resíduos do Modelo Stepwise com Transformação de Box-Cox",
       y = "Densidade") +
  theme_bw()

# Transformação de Box-Cox
lambda_BC_modelo_step_item <- powerTransform(df_ok$lances + 0.01)
lambda_BC_modelo_step_item

# Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
df_ok$bc_lances <- (((df_ok$lances + 0.01 ^ lambda_BC_modelo_step_item$lambda) - 1) / 
                      lambda_BC_modelo_step_item$lambda)

# Estimando novo modelo com transformação de Box-Cox
modelo_linear_item_bc <- lm(bc_lances ~ ., df_ok[c(4:5, 44, 13:31)])

# Summary do novo modelo
summary(modelo_linear_item_bc)

# Step do novo modelo
modelo_linear_item_bc_step <- step(modelo_linear_item_bc)

# Summary do stepwise do modelo com transformação de Box-Cox
summary(modelo_linear_item_bc_step)

# Não melhorou nada

ols_vif_tol(modelo_linear_item_bc_step) # Diagnóstico de Multicolinearidade


ols_test_breusch_pagan(modelo_linar_item_step) # Teste de Breusch-Pagan





# Outras formas de exibir o summary (a primeira gera um doc)
jtools::export_summs(modelo_linear_item, to.file = "docx", file.name = "summs.docx" )
jtools::summ(modelo_linar_item_step,
             digits = 3)

# Calculando intervalos de confiança (verificar que cruza o zero)
confint(modelo_linar_item_step, level = 0.95)

# Testando normalidade dos resíduos do modelo reglin
# p-value < 0.05 não é normal
sf.test(modelo_linar_item_step$residuals)

# Visualizar correlações
df_ok %>% correlation(method = "pearson") %>% plot()

# Visualizar correlações através de outro método
PerformanceAnalytics::chart.Correlation((df_ok[c(3:11)]), histogram = TRUE)

# Log Likelihood do modelo linear
logLik(modelo_linar_item_step)

linear_dm <- lm(lances ~ . -oportunidade -item -familia -preco_estimado -centro -unidade_pb -zero, df_dummies)
summary(linear_dm)
linear_dm2 <- lm(lances ~ . -oportunidade -item -familia -preco_estimado -centro -unidade_pb -zero, linear_dm$model)
hist(linear_dm$residuals)
step_linear_dm <- step(linear_dm2, k = 3.841459)
summary(step_linear_dm)
hist(step_linear_dm$residuals)

ols_vif_tol(linear_dm2) # Diagnóstico de Multicolinearidade
ols_test_breusch_pagan(modelo_linear_item) # Teste de Breusch-Pagan


# RESULTADO PRELIMINAR REGLIN: UMA PORCARIA DE MODELO


# BAIXO NÍVEL DE CORRELAÇÃO NO DF COM TRATAMENTO DE OUTLIERS

# EM TESTES INDIVIDUAIS NENHUMA VARIÁVEL MOSTROU UM BOM R2
# EM REGLIN SIMPLES
# RESÍDUOS DA REGLIN NÃO É NORMAL
# TESTAR COM VARIÁVEL TRANSFORMADA POR BOX-COX


regpoisson <- glm(lances ~ qtd + preco_unitario +qtd_convidados + tempo_cot,
                  df, family = "poisson")
summary(regpoisson)

logit <- glm(zero ~ qtd + preco_unitario +qtd_convidados + tempo_cot, df, 
             family = "binomial")

summary(logit)
step(logit)

reg_negbin <- glm.nb(lances ~ qtd + preco_unitario + qtd_convidados + tempo_cot, df)
summary(reg_negbin)

mod_zbin <- pscl::zeroinfl(lances ~ qtd + preco_unitario + qtd_convidados
                           | tempo_cot ,
                           df,
                           dist = "negbin")

summary(mod_zbin)
step(reg_negbin)

logLik(modelo)
logLik(regpoisson)
logLik(reg_negbin)
logLik(mod_zbin)
logLik(logit)

df$previsto <- reg_negbin$fitted.values
df$previsto_se_zero <- logit$fitted.values

modelo_multi <- glmmTMB(
  formula = lances ~ qtd + preco_unitario + tempo_cot + (1 | oportunidade),
  zi = ~ qtd_convidados ,
  family = nbinom2,
  data = df)

summary(modelo_multi)

logLik(modelo_multi)


outlierTest(reg_negbin)
cooksd <- cooks.distance(reg_negbin)
plot(cooksd, pch = "*", cex = 2, main = "Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
df[influential, ]
mean(df$lances)
var(df$lances)
pscl::vuong(reg_negbin, mod_zbin)

overdisp::overdisp(df, dependent.position = 10,
                   predictor.position = c(4, 6, 8:9))

freq(df$lances)
freq(df$qtd_convidados)

# Anotações, premissas, lembretes

# Variável Dependente: Quantidade de Lances Válidos
# 
# Variáveis Preditoras:
#   Nível 1 - Item da Oportunidade
# - Quantidade
# - Preço Unitário Estimado
# - Unidade logística - variável dummy com agregação para Outros
# 
# Nível 2 - Oportunidade
# - Tempo para lance
# - Quantidade de Notificados
# - Painel de Seleção (Dicotômica, se teve seleção de fornecedores em painel)
# - Categoria (Agregação do Grupo de Mercadorias)

# Sugestão de Título: Prevendo Quantidade de Lances em Licitações
# utilizando Zero-Inflated Negative Binomial Mixed Models


# EDA provisório ####

summary(oportunidades_resumo)
View(cor(oportunidades_resumo[, 2:7]))
hist(oportunidades_resumo$lances_por_item)
DataExplorer::create_report(oportunidades_resumo)

DataExplorer::create_report(df)

boxplot_prop <- ggplot(
  df, aes(as.integer(lances))) + 
  geom_histogram(aes(y = ..density..), fill = "seagreen3",
                 color = "chartreuse4", bins = 28) +
  geom_density(outline.type = "upper", size = 1) +
  theme_classic() +
  ggtitle("Propostas Válidas por Item") +
  ylab("") +
  xlab("Total de Propostas") +
  scale_x_continuous(breaks = seq(0, 28, 1)) 

boxplot_prop
ggplotly(boxplot_prop)

ggplot(itens_total, aes(y = total)) +
  geom_boxplot(fill = "seagreen3") +
  coord_cartesian(ylim = c(0, 30)) +
  theme_classic() +
  ylab("Total de Lances")

ggplot(df, aes(y = preco_estimado)) +
  geom_boxplot(fill = "seagreen3") +
  coord_cartesian(ylim = c(0, 1800000)) +
  theme_classic() +
  ylab("Preço Estimado") +
  scale_y_continuous(breaks = seq(0, 10000000, 200000))

ggplot(df, aes(y = qtd)) +
  geom_boxplot(fill = "seagreen3") +
  coord_cartesian(ylim = c(0, 9000)) +
  theme_classic() +
  ylab("Quantidade solicitada") +
  scale_y_continuous(breaks = seq(0, 30, 5))

df %>% filter(tempo_cot <= 60) %>% 
  ggplot(aes(y = tempo_cot)) +
  geom_boxplot(fill = "seagreen3") +
  coord_cartesian(ylim = c(0, 60)) +
  theme_classic() +
  ylab("Tempo para Cotação") +
  scale_y_continuous(breaks = seq(0, 150, 20))

options(scipen = 999)

ggplot(df, aes(y = qtd_convidados)) +
  geom_boxplot(fill = "seagreen3") +
  coord_cartesian(ylim = c(0, 65)) +
  theme_classic() +
  ylab("Fornecedores Convidados") +
  scale_y_continuous(breaks = seq(0, 100, 10))


plot(density(itens_total$total))


#########################################


df_hlm2_regiao <- df %>% 
  group_by(regiao) %>% 
  summarise(media_notif_regiao = round(mean(qtd_forn_notif), 2),
            media_tempo_regiao = round(mean(tempo_cot), 2))

df_hlm2_categoria <- df %>% 
  group_by(categoria) %>% 
  summarise(media_notif_cat = round(mean(qtd_forn_notif), 2),
            media_tempo_cat = round(mean(tempo_cot), 2))

df_hlm2 <- df %>% 
  inner_join(df_hlm2_categoria, by = "categoria") %>% 
  inner_join(df_hlm2_regiao, by = "regiao")


modelo_zinbm_AA <- glmmTMB(formula = lances ~ qtd + media_notif_cat
                           + qtd:media_notif_cat
                           + (qtd | categoria),
                           zi = ~ tempo_cot,
                           family = nbinom2,
                           data = df_hlm2)

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_zinbm_AA)
logLik(modelo_zinbm_AA)

# Dummyzando o df
df_dummies_hlm2 <- dummy_cols(.data = df, 
                              select_columns = c("regiao"),
                              remove_most_frequent_dummy = FALSE, 
                              remove_selected_columns = TRUE)




df_teste <- df_hlm2
df_teste$predict <- predict(modelo_zinbm_AA, df_teste)




ggplotly(df_teste %>%
           mutate(fitted_categoria = predict(modelo_zinbm_AA, level = 1)) %>% 
           ggplot() +
           geom_point(aes(x = qtd_forn_notif, y = fitted_categoria)) +
           geom_smooth(aes(x = qtd_forn_notif, y = fitted_categoria, color = factor(categoria)), 
                       method = "lm", se = F) +
           scale_colour_viridis_d() +
           labs(x = "Quantidade de Fornecedores Notificados",
                y = "Quantidade de Lances (Fitted Values)") +
           theme_bw()
)



df_hlm2 %>% 
  ggplot() +
  aes(media_notif_cat, lances, color = categoria) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_jitter()