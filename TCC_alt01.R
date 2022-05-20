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
car, # Regressões (Confint, cooksd, PowerTransform, scatter3D)
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
ggraph, # Plotar correlações
kableExtra, # Tabelas
nlme, # HLM2
lmtest, # Teste de LL
reshape2, # Usar a função melt (acho que só)
openxlsx, # Importar/Exportar Excel
hablar, # Conversão de tipos de dados
tidyr # Tidyezar BDS
)

# O pacote MASS sobrescreve a função select do pacote dplyr.
# Para que o R volte a entender o dplyr como standard, vamos definir:
select <- dplyr::select

# Datasets ####

itens_de_oportunidade_bruto <- openxlsx::read.xlsx(
  "datasets\\itens_de_oportunidade_bruto.xlsx")

oportunidades_bruto <- openxlsx::read.xlsx(
  "datasets\\oportunidades_bruto.xlsx")

propostas_bruto <- openxlsx::read.xlsx(
  "datasets\\propostas_bruto.xlsx")

# Vetor lista de oportunidades para uso em consulta de itens
oportunidades_lista <- oportunidades_bruto$object_id

# RCs
rcs <- read.csv("datasets\\rcs.csv") %>% 
  mutate(req = as.character(req),
         item_req = as.character(item_req))

# Dataframe de Centros e Categorias de Grupos de Mercadorias
# centros <- readxl::read_xlsx(
#   "datasets\\centros.xlsx", sheet = "Hierarquia") %>% 
#   clean_names() %>% 
#   rename("unidade_pb" = "unidade")

centros <- readRDS("datasets\\centros.rds") %>%
  clean_names() %>% 
  rename("unidade_pb" = "unidade") %>% 
  select(centro, unidade_pb, estado, regiao, lat, long) %>%
  filter(!is.na(estado),
         !is.na(unidade_pb)) %>% 
  convert(fct(estado, regiao))

categorias <- readxl::read_xls(
  "datasets\\Grupos de Mercadoria x Macrocategorias.xls") %>% 
  clean_names() %>% 
  rename(grupo_mercadoria = grupo_de_mercadorias) %>% 
  distinct(grupo_mercadoria, .keep_all = TRUE)

# Tratamento dos DFs Principais ####

# Tratamento do df de itens
itens_de_oportunidade <- itens_de_oportunidade_bruto %>%
  select(oportunidade = object_id, description_oportunidade, item = exlin,
         description_item, familia = ypfamitem, description_fam,
         grupo_merc = category_text, qtd = quantity, 
         unidade = msehl, preco_estimado = ypcon_price,
         centro = ypcon_werks, currency,
         req = reqnumber, item_req = reqnumber_item) %>% 
  mutate(item_req = str_remove(item_req, "^0+"),
         req = str_trim(req)) %>% 
  filter(unidade == "Unidade") %>% 
  left_join(select(centros, c("centro", "unidade_pb", "estado", "regiao")),
            by = "centro") %>%
  left_join(select(rcs, "req", "item_req", "grupo_mercadoria"),
            by = c("req", "item_req")) %>% 
  left_join(select(categorias, "grupo_mercadoria", "categoria"),
            by = "grupo_mercadoria") %>% 
  mutate(#preco_estimado = as.vector(scale(preco_estimado)),
         familia = str_trunc(familia, 8, side = "left", ellipsis = "0"),
         categoria = case_when(
           is.na(categoria) ~ "Outras categorias",
           TRUE ~ categoria),
         categoria = fct_lump(as.factor(categoria), prop = 0.05 ,
                              other_level = "Outras categorias")
         )

# Tratamento de oportunidades. Seleção inicial de colunas relevantes e 
# criação de colunas
oportunidades <- oportunidades_bruto %>%
  select(oportunidade = object_id, description, status, cc_status_area_publ, 
         cc_substat_area_publ, chave_comprador = sobid, 
         nome_comprador = name_text, data_criacao = cc_datetime_created_at,
         data_pub = cc_datetime_pub, data_inicio = cc_datetime_start_time,
         data_fechament = cc_datetime_fecha,
         data_abertura = cc_datetime_open_time,qtd_itens = cc_qtd_itens,
         currency, valor_transacionado, qtd_convidados, qtde_lances, 
         total_declinios, grupo_comprador = proc_group,
         qtd_forn_notif = cc_qtd_forn_notif, cc_qtd_forn_inscr,
         cc_qtd_propostas, cc_datetime_adj, cc_datetime_homolog) %>% 
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
  inner_join(select(itens_de_oportunidade, "oportunidade"), 
             by = "oportunidade") %>% 
  unique() %>% 
  arrange(oportunidade)

# Tratamento do df de propostas
propostas <- propostas_bruto %>% 
  select(oportunidade = id_oportunidade, item = cc_item, description,
         fornecedor = vendor_no, razao_social = name_org1, status_proposta,
         status_classif, cc_melhor_cotacao, cc_menor_proposta, preco = netprice,
         preco_total = totalpriceeq, prazo_entrega = deliv_days,
         preco_estimado = itmestprice, qtd_desclassificados, 
         just_desclassif = justf_class_com, faturamento_minimo) %>% 
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
           is.na(unidade_pb) ~ "Outros",
           TRUE ~ unidade_pb
         )) %>% 
  hablar::convert(fct(unidade_pb, regiao, estado, zero)) %>% 
  select(oportunidade, item, qtd, preco_unitario, unidade_pb, regiao,  
         qtd_forn_notif, painel_selecao, tempo_cot, categoria,
         lances = total, zero) %>% 
  filter(!is.na(regiao))
  
  
# Limitando a 20 a quantidade de categorias de Unidades Operacionais
df$unidade_pb <- gsub("^UTG", "UTG", df$unidade_pb)
df$unidade_pb <- fct_lump(df$unidade_pb, n = 20, other_level = "Outros")


# Dummyzando o df
df_dummies <- dummy_cols(.data = df, 
           select_columns = c("unidade_pb", "categoria"),
           remove_first_dummy = TRUE)


#Tratamento de Outliers ####

# O DF pode ser ajustado para desconsiderar outliers existentes em:
# 1. preco_unitario
# 2. qtd
# 3. tempo_cot

# LIMPANDO PREÇO UNITÁRIO
# Iniciamos limpando preços abaixo de dez e acima de 2M(erros)
df_ok <- df %>% 
  filter(preco_unitario > 10,
         preco_unitario < 2000000)

# Seguimos identificando outliers usando o teste de Tukey com cercas de 1.5
out_pu <- boxB(df_ok$preco_unitario, k = 1.5, 
               method = "asymmetric", logt = TRUE)

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

df <- df_ok
rm(df_ok)

# Função std dev das variâncias ####

#Algoritmo para determinação dos erros-padrão das variâncias no componente de
#efeitos aleatórios
#ATENÇÃO: A função abaixo é plenamente funcional para modelos do tipo HLM2
#e HLM3, desde que estimados pelo pacote nlme

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(
        `RE Components`=base::c("Var(v0j)","Var(e)"),
        `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                        base::exp(logs_sd_re[[2]])^2),
        `Std Err.`=base::c(stderr_tau00, stderr_sigma),
        z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                  base::exp(logs_sd_re[[2]])^2/stderr_sigma),
        `p-value`=base::round(stats::pnorm(
          q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                    base::exp(logs_sd_re[[2]])^2/stderr_sigma),
          lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(
        Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
        Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                             base::exp(logs_sd_re[[2]])^2,
                             base::exp(logs_sd_re[[4]])^2),
        Std_Err=base::c(stderr_tau00, stderr_tau01, stderr_sigma),
        z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                  base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                  base::exp(logs_sd_re[[4]])^2/stderr_sigma),
        `p-value`=base::round(stats::pnorm(
          q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                    base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                    base::exp(logs_sd_re[[4]])^2/stderr_sigma),
          lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(
        Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
        Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                            base::exp(logs_sd_re)[[1]]^2,
                            base::exp(logs_sd_re)[[3]]^2),
        Std_Err=base::c(stderr_tau_u000, stderr_tau_r000, stderr_sigma),
        z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                  base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                  base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
        `p-value`=base::round(stats::pnorm(
          q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                    base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                    base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
          lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(
        `RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                "Var(v0jk)","Var(v1jk)",
                                "Var(e)"),
        `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                       base::exp(logs_sd_re)[[5]]^2,
                                       base::exp(logs_sd_re)[[1]]^2,
                                       base::exp(logs_sd_re)[[2]]^2,
                                       base::exp(logs_sd_re)[[7]]^2),
        `Std Err.`=base::c(stderr_tau_u000, stderr_tau_u100,
                           stderr_tau_r000, stderr_tau_r100,
                           stderr_sigma),
        z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                  base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                  base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                  base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                  base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
        `p-value`=base::round(stats::pnorm(
          q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                    base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                    base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                    base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                    base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
          lower.tail=F)*2,3))
      return(results)
    }
  }
}

