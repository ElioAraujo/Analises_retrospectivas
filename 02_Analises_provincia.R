pacman::p_load(dplyr, magrittr, tidyverse, here, rio, lubridate,
               epitrix, zoo, gridExtra, grid, openxlsx)

if (!requireNamespace("ggthemr", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("cttobin/ggthemr")
}
library(ggthemr)
ggthemr("fresh")

if (!exists("one_suspected_case")) source(here::here("scripts", "0_functions.R"))

if (!exists("plot_flag")) plot_flag <- TRUE

if (!exists("dat")) {
  dat_base <- rio::import(here::here("data", "dados_preparados.xlsx"))
} else {
  dat_base <- dat %>% select(-starts_with("alert_"))
}

pop <- rio::import(here::here("data", "Pop_Moz.xlsx")) %>%
  select(clean_name, ano, populacao)

# Provincias e respectivos distritos a analisar separadamente
provincias <- c("SOFALA", "NAMPULA")

for (provincia in provincias) {

  dat_prov <- dat_base %>% filter(province == provincia)
  pasta_base <- file.path("results_provincias", str_to_title(provincia))

  if (!dir.exists(here::here(pasta_base))) dir.create(here::here(pasta_base), recursive = TRUE)

  list_df <- list()
  list_df_m <- list()

  message(sprintf("A analisar provincia: %s (%d distritos)", provincia, n_distinct(dat_prov$district)))

  # Limiar: um caso suspeito
  dat_prov$alert_colera <- one_suspected_case(dat_prov, "Colera", "colera", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_pfa <- one_suspected_case(dat_prov, "Paralisia_Flacida_Aguda", "pfa", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_peste <- one_suspected_case(dat_prov, "Peste", "peste", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_tetano_neonatal <- one_suspected_case(dat_prov, "Tetano_Neonatal", "tetano_neonatal", gen_plot = plot_flag, pasta_base = pasta_base)

  # Aumento >50% vs. tendencia recente, ou duplicacao em P2W (Diarreia, Disenteria)
  dat_prov$alert_disenteria_p50p <- unusual_inc_50p(dat_prov, "Disenteria", "disenteria", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_disenteria_doubling <- unusual_inc_doubling(dat_prov, "Disenteria", "disenteria", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_diarreia_p50p <- unusual_inc_50p(dat_prov, "Diarreia", "diarreia", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_diarreia_doubling <- unusual_inc_doubling(dat_prov, "Diarreia", "diarreia", gen_plot = plot_flag, pasta_base = pasta_base)

  # Cluster suspeito (Sarampo): >=5 casos suspeitos numa janela de 28 dias
  dat_prov$alert_sarampo <- susp_cluster_janela(dat_prov, "Sarampo", "sarampo", n_cluster = 5, dias_janela = 28,
                                            gen_plot = plot_flag, pasta_base = pasta_base)

  # Malaria: percentil 75, CSUM, media+2DP
  dat_prov$alert_malaria_75p <- above_percentile(dat_prov, "Malaria", "malaria", 0.75, gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_malaria_csum <- csum(dat_prov, "Malaria", "malaria", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_malaria_m2sd <- mean_2sd(dat_prov, "Malaria", "malaria", gen_plot = plot_flag, pasta_base = pasta_base)

  # Meningite: taxa de ataque > 3/100.000 (populacao por distrito e ano)
  dat_prov$alert_meningite <- attack_rate(dat_prov, "Meningite", "meningite", pop = pop, ar_cutoff = 3,
                                      gen_plot = plot_flag, pasta_base = pasta_base)

  # Doencas sem limiar formal: mesmo tratamento generico (>50% / duplicacao P2W)
  dat_prov$alert_raiva_p50p <- unusual_inc_50p(dat_prov, "Raiva", "raiva", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_raiva_doubling <- unusual_inc_doubling(dat_prov, "Raiva", "raiva", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_sindrome_febril_p50p <- unusual_inc_50p(dat_prov, "Sindrome_Febril", "sindrome_febril", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_sindrome_febril_doubling <- unusual_inc_doubling(dat_prov, "Sindrome_Febril", "sindrome_febril", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_mordeduras_animal_p50p <- unusual_inc_50p(dat_prov, "Mordeduras_Animal", "mordeduras_animal", gen_plot = plot_flag, pasta_base = pasta_base)
  dat_prov$alert_mordeduras_animal_doubling <- unusual_inc_doubling(dat_prov, "Mordeduras_Animal", "mordeduras_animal", gen_plot = plot_flag, pasta_base = pasta_base)

  # Consolidacao final desta provincia (mesma logica do 00_Executar_tudo.R)
  write.xlsx(list_df,   file = here::here(pasta_base, "alertas_por_distrito_por_ano.xlsx"))
  write.xlsx(list_df_m, file = here::here(pasta_base, "alertas_por_distrito_por_mes.xlsx"))

  dat_prov %<>% mutate_at(vars(contains("alert_")), unlist)
  rio::export(dat_prov, here::here(pasta_base, "dados_com_alertas.xlsx"))

  message(sprintf("Provincia concluida: %s -> %s", provincia, pasta_base))
}

message("Analises por provincia concluidas. Resultados em results_provincias/<Provincia>/")
