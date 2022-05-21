df %>%
  ggplot(aes(x = preco_unitario, y = lances)) +
  geom_point(color = "black") +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm",
              se = FALSE, size = 2) +
  geom_jitter(width = 0.8, height = 0.1) +
  labs(y = "Lances em Itens de Oportunidades",
       x = "Quantidade de Fornecedores Notificados") +
  scale_color_manual("Label:",
                     values = "gold") +
  facet_wrap(~ zero) +
  theme_bw()

df_corr  <- df %>% 
  select(lances, qtd, preco_unitario, qtd_forn_notif, tempo_cot) %>% 
  mutate(lances = case_when(lances <= 1 ~ 0,
                            TRUE ~ log(lances)),
         qtd = log(qtd),
         preco_unitario = log(preco_unitario),
         qtd_forn_notif = case_when(qtd_forn_notif <= 1 ~ 0,
                            TRUE ~ log(qtd_forn_notif)),
         tempo_cot = log(tempo_cot))

df_corr_scaled <-  df_corr %>% scale() %>% as.data.frame()
 

df_scaled <- df %>%
  select(lances, qtd, preco_unitario, qtd_forn_notif, tempo_cot) %>%
  scale() %>% 
  as.data.frame()

PerformanceAnalytics::chart.Correlation(R = df_corr)

maiorQ1 <- function(numero){
  numero > 1
}

df_corr2 <- df %>%
  mutate(lances = case_when(lances <= 1 ~ 0,
                            TRUE ~ log(lances)),
         qtd = log(qtd),
         preco_unitario = log(preco_unitario),
         qtd_forn_notif = case_when(qtd_forn_notif <= 1 ~ 0,
                                    TRUE ~ log(qtd_forn_notif)),
         tempo_cot = log(tempo_cot))

DataExplorer::plot_correlation(df, title = "DF normal")
DataExplorer::plot_correlation(df_corr, title = "DF Log somente quanti")
DataExplorer::plot_correlation(df_corr2, title = "DF Log quanti e quali")
DataExplorer::plot_correlation(df_scaled, title = "DF quanti com scale e sem log")
DataExplorer::plot_correlation(df_corr_scaled, title = "DF Log com scale")
