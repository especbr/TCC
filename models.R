################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacman::p_load("plotly","tidyverse","knitr","kableExtra","reshape2","ggrepel",
             "lmtest","jtools","questionr","MASS","pscl","overdisp","glmmTMB")

##############################################################################
#                      REGRESSÃO PARA DADOS DE CONTAGEM                      #
#                        CARREGAMENTO DA BASE DE DADOS                       #
##############################################################################

# Já carregada


##############################################################################
#                   OBSERVAÇÃO DA BASE DE DADOS corruption                   #
##############################################################################
#Visualizando a base de dados
df %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

glimpse(df) #Visualização das observações e das  especificações 
#referentes às variáveis da base de dados

#Estatísticas descritivas univariadas e tabela de frequências
summary(df)

#Tabela de frequências da variável dependente (função freq para gerar tabelas de
#frequência do pacote questionr)
freq(df$lances) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 19)

#Histograma da variável dependente
df %>%
  ggplot(aes(x = lances, fill = ..count..)) +
  geom_histogram(bins = round(2 * nrow(df) ^ (1 / 3)), color = "black") +
  labs(x = "Quantidade de lances para o item",
       y = "Frequência",
       fill = "Contagem") +
  theme_few() +
  scale_fill_gradientn(colours = wes_palette(
    "Zissou1", 50, type = "continuous"))

#Diagnóstico preliminar para observação de eventual igualdade entre a média e
#a variância da variável dependente 'lances'
df %>%
  summarise(Média = mean(lances),
            Variância = var(lances)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 30)

#Comportamento das variáveis 'painel_selecao' e 'lances'
df %>%
  mutate(lnlances = log(lances),
         lnlances = ifelse(lnlances == -Inf,
                               yes = 0, 
                               no = lnlances)) %>%
  ggplot(aes(x = preco_unitario, y = lnlances)) +
  geom_point(color = "black") +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm",
              formula = y ~ splines::bs(x),
              se = FALSE, size = 2) +
  geom_jitter(width = 0.8, height = 0.1) +
  labs(y = "Lances em Itens de Oportunidades",
       x = "Quantidade de Fornecedores Notificados") +
  scale_color_manual("Label:",
                     values = "gold") +
  facet_wrap(~ zero) +
  theme_bw()


################################################################################
#                        ESTIMAÇÃO DO MODELO POISSON                           #
################################################################################
#Estimação do modelo Poisson
modelo_poisson <- glm(formula = lances ~ qtd,
                      data = df,
                      family = "poisson")

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_poisson)
logLik(modelo_poisson)

tbl_regression(modelo_poisson, estimate_fun = ~ style_sigfig(., digits = 7),
               intercept = TRUE, conf.int = FALSE) %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "tables\\table_poisson.docx")



################################################################################
#            TESTE DE SUPERDISPERSÃO DE CAMERON E TRIVEDI (1990)               #
################################################################################
#CAMERON, A. C.; TRIVEDI, P. K. Regression-based tests for overdispersion in
#the Poisson model. Journal of Econometrics, v. 46, n. 3, p. 347-364, 1990.

overdisp(x = df_dummies,
         dependent.position = 8,
         predictor.position = c(3:7, 10:20))


################################################################################
#                   ESTIMAÇÃO DO MODELO BINOMIAL NEGATIVO                      #
################################################################################
#Estimação do modelo binomial negativo pela função glm.nb do pacote MASS
#Modelo Binomial Negativo do Tipo 2 (NB2)
modelo_bneg <- glm.nb(formula = lances ~ qtd,
                      data = df_dummies)

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_bneg)
logLik(modelo_bneg)

tbl_regression(modelo_bneg, estimate_fun = ~ style_sigfig(., digits = 7),
               intercept = TRUE, conf.int = FALSE,
               label = qtd ~ "Quantidade", ) %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "tables\\table_bneg.docx")

################################################################################
#              ESTIMAÇÃO DO MODELO ZERO-INFLATED POISSON (ZIP)                 #
################################################################################
#Estimação do modelo ZIP pela função zeroinfl do pacote pscl
modelo_zip <- zeroinfl(formula = lances ~ qtd
                       | tempo_cot,
                       data = df,
                       dist = "poisson")

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_zip)
logLik(modelo_zip)

#Teste de Vuong:
#VUONG, Q. H. Likelihood ratio tests for model selection and non-nested
#hypotheses. Econometrica, v. 57, n. 2, p. 307-333, 1989.

vuong(m1 = modelo_poisson,
      m2 = modelo_zip)


################################################################################
#        ESTIMAÇÃO DO MODELO ZERO-INFLATED BINOMIAL NEGATIVO (ZINB)            #
################################################################################
#Estimação do modelo ZINB pela função zeroinfl do pacote pscl
modelo_zinb <- zeroinfl(formula = lances ~ qtd
                        | tempo_cot,
                        data = df,
                        dist = "negbin")

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_zinb)
logLik(modelo_zinb)

#Teste de Vuong (1989)
vuong(m1 = modelo_bneg,
      m2 = modelo_zinb)


################################################################################
#     ESTIMAÇÃO DO MODELO MULTINÍVEL ZERO-INFLATED BINOMIAL NEGATIVO (ZINB)    #
################################################################################
#Estimação do modelo ZINBm pela função glmmTMB do pacote glmmTMB

modelo_zinbm <- glmmTMB(formula = lances ~ qtd 
                        + (1 | categoria),
                        zi = ~ tempo_cot,
                        family = nbinom2,
                        data = df)

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_zinbm)
logLik(modelo_zinbm)

export_summs(modelo_poisson, modelo_bneg, modelo_zinbm,
             model.names = c("Poisson", "BNEG", "ZINB Multilevel"))


#Comparação entre os LLs de todos os modelos estimados neste exemplo
data.frame(Poisson = logLik(modelo_poisson),
           ZIP = logLik(modelo_zip),
           BNEG = logLik(modelo_bneg),
           ZINB = logLik(modelo_zinb),
           ZINBM = logLik(modelo_zinbm)) %>%
  rename(`Poisson` = 1,
         `ZIP` = 2,
         `BNEG` = 3,
         `ZINB` = 4,
         `ZINB Multilevel` = 5) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 5) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","maroon1","darkblue",
                               "deepskyblue3","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Teste de razão de verossimilhança
lrtest(modelo_zinb,modelo_zinbm)

####################################### FIM ####################################
