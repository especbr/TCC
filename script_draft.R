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
