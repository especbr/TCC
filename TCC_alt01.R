# Pacotes ####

pacman::p_load(
readxl, # Ler Excel
lubridate, # Tratar datas
janitor, # Data Cleaning
forcats, # Dados Categóricos (muito útil o fct_lump)
glue, # Colar variáveis em chamadas SQL
plotly, # Plotar gráficos dinâmicos
stringr, # Tratar strings deveria ter vindo com o TDV,
dplyr, # Data wrangling
ggplot2, # Gráficos
corrplot, # Correlações
tibble, # Tibbles
MASS, # Modelos para Dados de Contagem Binomial Negativa 
questionr, # Para uso da função freq tabela de frequência,
car, # Regressões (Confint, cooksd, PowerTransform)
overdisp, # Teste de Superdisperção
wesanderson, # Paleta de cores
glmmTMB, # Pacote para usar Multinível
correlation, # Plotar correlação
nortest, # Testes de Normalidade
car, # "Companion to Applied Regression" - usaremos PowerTransform
olsrr, # Diagnóstico de Multicolinearidade e Heterocedasticidade
fastDummies, # Tratamento de variáveis dummies
DescTools, # Tratamento de Outliers
univOutl, # Tratamento de Outliers
ggraph # Plotar correlações
)

# Conexões às bases de dados ####
sgd <- dbConnect(odbc(), "BD SGD")

jdbcDriver = JDBC("oracle.jdbc.OracleDriver",classPath = "C:\\Users\\jhsc\\Desktop\\Analise_Contratos\\ojdbc6.jar")
datalake = dbConnect(jdbcDriver, "jdbc:oracle:thin:@//sand-scan1.petrobras.com.br:1521/odscp.petrobras.com.br", "JHSC", "Jose#Humb0")

# Consultas SQL (Dados brutos) ####

# Dados de oportunidades (cabeçalho)
oportunidades_bruto <- glue::glue_sql("SELECT *
                                  FROM BISBS.VW_BISBS_OPORTUNIDADE o
                                WHERE
                                PROCESS_TYPE = 'LICI'
                                -- AND CC_MODALIDADE = '101'
                                AND CC_DATETIME_OPEN_TIME BETWEEN DATE '2021-01-01' AND DATE '2021-12-31'
                                AND CC_DATETIME_CANC IS NULL 
                                AND JUDGEMENT_TYPE = 'Julgamento por Item'
                                AND PROC_GROUP LIKE 'MAT-EIVAT%'", .con = datalake) %>% 
  dbSendQuery(conn = datalake) %>% 
  dbFetch() %>% 
  clean_names() %>% 
  filter(!str_detect(proc_group, "^MAT-EIVAT/GLOBAIS"))

# Vetor lista de oportunidades para uso em consulta de itens
oportunidades_lista <- oportunidades_bruto$object_id

# Dados dos itens das oportunidades (uma linha por item)
itens_de_oportunidade_bruto <- glue_sql("SELECT *
                                  FROM BISBS.VW_BISBS_ITEM_OPORTUNIDADE i
                                  WHERE OBJECT_ID IN ({oportunidades_lista*})",
                                        .con = datalake) %>%
  dbSendQuery(conn = datalake) %>% 
  dbFetch() %>% 
  clean_names()

# Dados brutos das propostas (lances)
propostas_bruto <- glue_sql("SELECT *
                      FROM BISBS.VW_BISBS_COMPARATIVO_PROPOSTAS p
                      WHERE ID_OPORTUNIDADE IN ({oportunidades_lista*})",
                            .con = datalake) %>%
  dbSendQuery(conn = datalake) %>% 
  dbFetch() %>% 
  clean_names()

# RCs
rcs <- read.csv("rcs.csv") %>% 
  mutate(req = as.character(req),
         item_req = as.character(item_req))

# Tratamento dos DFs Principais ####

# O pacote MASS sobrescreve a função select do pacote dplyr. Como usamos mais este, vamos corrigir
select <- dplyr::select

# Dataframe de Centros e Metacategorias de GMs
centros <- read_xlsx("centros.xlsx", sheet = "Hierarquia") %>% 
  clean_names() %>% 
  rename("unidade_pb" = "unidade")

metacategorias <- read_xls("Grupos de Mercadoria x Macrocategorias.xls") %>% 
  clean_names() %>% 
  rename(grupo_mercadoria = grupo_de_mercadorias) %>% 
  distinct(grupo_mercadoria, .keep_all = TRUE)

# Tratamento do df de itens
itens_de_oportunidade <- itens_de_oportunidade_bruto %>%
  select(oportunidade = object_id, description_oportunidade, item = exlin, description_item,
         familia = ypfamitem, description_fam, grupo_merc = category_text, qtd = quantity, 
         unidade = msehl, preco_estimado = ypcon_price, centro = ypcon_werks, currency,
         req = reqnumber, item_req = reqnumber_item) %>% 
  mutate(item_req = str_remove(item_req, "^0+"),
         req = str_trim(req)) %>% 
  filter(unidade == "Unidade") %>% 
  left_join(select(centros, c("centro", "unidade_pb")), by = "centro") %>%
  left_join(select(rcs, "req", "item_req", "grupo_mercadoria"), by = c("req", "item_req")) %>% 
  left_join(select(metacategorias, "grupo_mercadoria", "categoria"), by = "grupo_mercadoria") %>% 
  mutate(#preco_estimado = as.vector(scale(preco_estimado)),
         familia = str_trunc(familia, 8, side = "left", ellipsis = "0"),
         categoria = case_when(
           is.na(categoria) ~ "Outras categorias",
           TRUE ~ categoria),
         categoria = fct_lump(as.factor(categoria), prop = 0.01 , other_level = "Outras categorias")
         )

# Tratamento de oportunidades. Seleção inicial de colunas relevantes e 
# criação de colunas
oportunidades <- oportunidades_bruto %>%
  select(oportunidade = object_id, description, status, cc_status_area_publ, cc_substat_area_publ, 
         chave_comprador = sobid, nome_comprador = name_text, data_criacao = cc_datetime_created_at,
         data_pub = cc_datetime_pub, data_inicio = cc_datetime_start_time, data_fechament = cc_datetime_fecha,
         data_abertura = cc_datetime_open_time,qtd_itens = cc_qtd_itens, currency, valor_transacionado, 
         qtd_convidados, qtde_lances, total_declinios, grupo_comprador = proc_group,
         qtd_forn_notif = cc_qtd_forn_notif, cc_qtd_forn_inscr, cc_qtd_propostas, cc_datetime_adj, cc_datetime_homolog) %>% 
  mutate(tempo_cot = as.integer(difftime(date(ymd_hms(data_fechament)),
                                         date(ymd_hms(data_inicio)),
                                         units = "days")),
         qtd_forn_notif = case_when(
           is.na(qtd_forn_notif) ~ 0,
           TRUE ~ qtd_forn_notif
         ),
         painel_selecao = case_when(
           qtd_forn_notif == 0 ~ "N",
           TRUE ~ "S"
         )) %>% 
  inner_join(select(itens_de_oportunidade, "oportunidade"), by = "oportunidade") %>% 
  unique() %>% 
  arrange(oportunidade)

# Tratamento do df de propostas
propostas <- propostas_bruto %>% 
  select(oportunidade = id_oportunidade, item = cc_item, description, fornecedor = vendor_no,
         razao_social = name_org1, status_proposta, status_classif, cc_melhor_cotacao,
         cc_menor_proposta, preco = netprice, preco_total = totalpriceeq, prazo_entrega = deliv_days,
         preco_estimado = itmestprice, qtd_desclassificados, just_desclassif = justf_class_com,
         faturamento_minimo) %>% 
  inner_join(select(itens_de_oportunidade,
                    c("oportunidade", "item")),
                    by = c("oportunidade", "item"))


# EDA ####

# DF apenas com propostas válidas
propostas_validas <- propostas %>% 
  filter(status_classif != "Desclassificado") %>% 
  inner_join(itens_de_oportunidade, by = c("oportunidade", "item"))

# Totais de propostas válidas por Item
propostas_por_item <- propostas_validas %>%
  group_by(oportunidade, item) %>% 
  summarise(total = n()) %>%
  right_join(select(itens_de_oportunidade, c("oportunidade", "item")),
             by = c("oportunidade", "item")) %>% 
  ungroup() %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  arrange(oportunidade, item)

# Totais de propostas válidas por Oportunidade
propostas_por_oportunidade <- propostas_validas %>%
  group_by(oportunidade) %>% 
  summarise(qtd_lances_validos = n()) %>% 
  right_join(select(oportunidades, "oportunidade"), by = "oportunidade") %>% 
  mutate_all(~replace(., is.na(.), 0)) 

# Total de itens cotados
itens_cotados_por_oportunidade <- propostas_por_item %>% 
  mutate(cotado = case_when(
    total == 0 ~ 0,
    TRUE ~ 1)) %>% 
  group_by(oportunidade) %>% 
  summarise(itens_cotados = case_when(
    sum(cotado) == 0 ~ 0,
    TRUE ~ sum(cotado)
  ))
    
# DF de oportunidades resumido com métricas quantitativas
oportunidades_resumo <- oportunidades %>%
  select(oportunidade, qtd_forn_notif, painel_selecao, tempo_cot) %>% 
  inner_join(propostas_por_oportunidade, by = "oportunidade") %>%
  inner_join(itens_cotados_por_oportunidade, by = "oportunidade") %>%
  inner_join(select(itens_de_oportunidade, oportunidade, categoria),
             by = "oportunidade") %>%
  distinct(oportunidade, .keep_all = TRUE) %>% 
  mutate(lances_por_item = case_when(
    itens_cotados == 0 ~ 0,
    TRUE ~ qtd_lances_validos / itens_cotados))
  

# DF para o modelo
# ATENÇÃO! ESTÁ ERRADO. ESTÁ TRAZENDO ZERO PARA QTD CONVIDADOS E TEMPO_COT
# PARA CASOS QUE ANTES ERAM NA. 

df <- itens_de_oportunidade %>% 
  select(-categoria) %>% 
  inner_join(propostas_por_item, by = c("oportunidade", "item")) %>%
  left_join(select(oportunidades_resumo, 
                   c("oportunidade", "qtd_forn_notif", "categoria",
                     "tempo_cot", "painel_selecao")),
            by = "oportunidade") %>% 
  mutate(zero = case_when(
           total == 0 ~ 0,
           TRUE ~ 1),
         preco_unitario = preco_estimado / qtd,
         unidade_pb = case_when(
           is.na(unidade_pb) ~ "Indefinido",
           TRUE ~ unidade_pb
         ),
         unidade_pb = as_factor(unidade_pb)) %>% 
  select(oportunidade, item, qtd, unidade_pb, preco_unitario, familia,
         qtd_forn_notif, painel_selecao, tempo_cot, categoria, lances = total, zero)

# Limitando a 20 a quantidade de categorias de Centros
df$unidade_pb <- fct_lump(df$unidade_pb, n = 19, other_level = "Outros")

# Dummyzando o df
df_dummies <- dummy_cols(.data = df, 
           select_columns = c("unidade_pb", "categoria"),
           remove_first_dummy = TRUE)


#Tratamento de Outliers ####

# O DF pode ser ajustado para desconsiderar outliers existentes em:
# 1. preco_unitario
# 2. qtd
# 3. tempo_cot
# A variável preco_estimado não será usada, então não há problema

# LIMPANDO PREÇO UNITÁRIO
# Iniciamos limpando preços abaixo de dez e acima de 2M(erros)
df_ok <- df_dummies %>% 
  filter(preco_unitario > 10,
         preco_unitario < 2000000)

# Seguimos identificando outliers usando o teste de Tukey com cercas de 1.5
out_pu <- boxB(df_ok$preco_unitario, k = 1.5, method = "asymmetric", logt = TRUE)

# Criando linhas a serem excluídas
remove_pu <- out_pu$outliers

# Finalizando DF
df_ok <- df_ok[-remove_pu, ]

# LIMPANDO QTD

# Seguimos identificando outliers usando o teste de Tukey com cercas de 2
out_qtd <- boxB(df_ok$qtd, k = 2, method = "asymmetric", logt = TRUE)

# Criando linhas a serem excluídas
remove_qtd <- out_qtd$outliers

# Finalizando DF
df_ok <- df_ok[-remove_qtd, ]

# LIMPANDO TEMPO COT
# Somente retirando erros
df_ok <- df_ok %>% filter(tempo_cot <= 100)


# DataExplorer::create_report(df_ok, output_file = "report3.html")
# Usar quando quiser gerar um report do DF

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



