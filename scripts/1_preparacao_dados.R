# Converte Data_BES.xls (bruto, por doenca/faixa etaria) para o formato do pipeline

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(epitrix)
library(writexl)
library(ggplot2)

if (!requireNamespace("ggthemr", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("cttobin/ggthemr")
}
library(ggthemr)
ggthemr("fresh")

input_file <- here::here("data", "Data_BES.xls")
dat_raw <- read_excel(input_file)

dat_raw <- dat_raw %>%
  rename(
    country  = orgunitlevel1,
    province = orgunitlevel2,
    district = orgunitlevel3
  ) %>%
  select(-organisationunitname)

dat_raw <- dat_raw %>%
  mutate(
    semana_num = as.numeric(str_match(periodname, "^Week (\\d+)")[, 2]),
    ano_num    = as.numeric(str_match(periodname, "(\\d{4})-")[, 2]),
    data_inicio_semana = as.Date(str_match(periodname, "(\\d{4}-\\d{2}-\\d{2}) -")[, 2]),
    period     = paste0(ano_num, "W", semana_num)
  ) %>%
  select(-periodname)

# Mapa colunas BES -> doenca + faixa etaria
mapa_doencas <- tibble::tribble(
  ~col_original,                                    ~doenca,             ~faixa,        ~tipo,
  "BES - CÓLERA CASOS",                              "colera",            "todas",       "casos",
  "BES - CÓLERA ÓBITOS",                             "colera",            "todas",       "obitos",
  "BES - DIARREIA 0-4 anos, CASOS",                  "diarreia",          "0-4",         "casos",
  "BES - DIARREIA 0-4 anos, ÓBITOS",                 "diarreia",          "0-4",         "obitos",
  "BES - DIARREIA 5-14 anos, CASOS",                 "diarreia",          "5-14",        "casos",
  "BES - DIARREIA 5-14 anos, ÓBITOS",                "diarreia",          "5-14",        "obitos",
  "BES - DIARREIA 15+ anos, CASOS",                  "diarreia",          "15+",         "casos",
  "BES - DIARREIA 15+ anos, ÓBITOS",                 "diarreia",          "15+",         "obitos",
  "BES - DISENTERIA CASOS",                          "disenteria",        "todas",       "casos",
  "BES - DISENTERIA ÓBITOS",                         "disenteria",        "todas",       "obitos",
  "BES - MALÁRIA 0-4 anos, CASOS",                   "malaria",           "0-4",         "casos",
  "BES - MALÁRIA 0-4 anos, ÓBITOS",                  "malaria",           "0-4",         "obitos",
  "BES - MALÁRIA 5+ anos, CASOS",                    "malaria",           "5+",          "casos",
  "BES - MALÁRIA 5+ anos, ÓBITOS",                   "malaria",           "5+",          "obitos",
  "BES - MENINGITE 0-4 anos, CASOS",                 "meningite",         "0-4",         "casos",
  "BES - MENINGITE 0-4 anos, ÓBITOS",                "meningite",         "0-4",         "obitos",
  "BES - MENINGITE 5+ anos, CASOS",                  "meningite",         "5+",          "casos",
  "BES - MENINGITE 5+ anos, ÓBITOS",                 "meningite",         "5+",          "obitos",
  "BES - MORDEDURAS ANIMAL CASOS",                   "mordeduras_animal", "todas",       "casos",
  "BES - MORDEDURAS ANIMAL ÓBITOS",                  "mordeduras_animal", "todas",       "obitos",
  "BES - PARALISIA FLÁCIDA AGUDA CASOS",             "pfa",               "todas",       "casos",
  "BES - PARALISIA FLÁCIDA AGUDA ÓBITOS",            "pfa",               "todas",       "obitos",
  "BES - PESTE CASOS",                               "peste",             "todas",       "casos",
  "BES - PESTE ÓBITOS",                              "peste",             "todas",       "obitos",
  "BES - RAIVA CASOS",                               "raiva",             "todas",       "casos",
  "BES - RAIVA ÓBITOS",                              "raiva",             "todas",       "obitos",
  "BES - SARAMPO <9 Meses, CASOS",                   "sarampo",           "<9m",         "casos",
  "BES - SARAMPO <9 Meses, ÓBITOS",                  "sarampo",           "<9m",         "obitos",
  "BES - SARAMPO 9-23 Meses, CASOS",                 "sarampo",           "9-23m_vac",   "casos",
  "BES - SARAMPO 9-23 Meses, ÓBITOS",                "sarampo",           "9-23m_vac",   "obitos",
  "BES - SARAMPO 9-23 Meses Não Vacinados CASOS",    "sarampo",           "9-23m_nvac",  "casos",
  "BES - SARAMPO 9-23 Meses Não Vacinados ÓBITOS",   "sarampo",           "9-23m_nvac",  "obitos",
  "BES - SARAMPO 24+ Meses, CASOS",                  "sarampo",           "24m+",        "casos",
  "BES - SARAMPO 24+ Meses, ÓBITOS",                 "sarampo",           "24m+",        "obitos",
  "BES - SÍNDROME FEBRIL 0-4 anos, CASOS",           "sindrome_febril",   "0-4",         "casos",
  "BES - SÍNDROME FEBRIL 0-4 anos, ÓBITOS",          "sindrome_febril",   "0-4",         "obitos",
  "BES - SÍNDROME FEBRIL 5+ anos, CASOS",            "sindrome_febril",   "5+",          "casos",
  "BES - SÍNDROME FEBRIL 5+ anos, ÓBITOS",           "sindrome_febril",   "5+",          "obitos",
  "BES - TÉTANO RECÉM NASCIDOS CASOS",               "tetano_neonatal",   "todas",       "casos",
  "BES - TÉTANO RECÉM NASCIDOS ÓBITOS",              "tetano_neonatal",   "todas",       "obitos"
)

cols_ignoradas <- c("country", "province", "district", "period", "semana_num", "ano_num", "data_inicio_semana")
cols_dados <- setdiff(names(dat_raw), cols_ignoradas)
faltam <- setdiff(cols_dados, mapa_doencas$col_original)
if (length(faltam) > 0) {
  stop(paste("Colunas nao mapeadas em mapa_doencas:", paste(faltam, collapse = ", ")))
}

dat_long <- dat_raw %>%
  pivot_longer(
    cols = all_of(mapa_doencas$col_original),
    names_to = "col_original",
    values_to = "valor"
  ) %>%
  left_join(mapa_doencas, by = "col_original")

dat_agg <- dat_long %>%
  group_by(country, province, district, period, ano_num, semana_num, data_inicio_semana, doenca, tipo) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  mutate(nome_var = paste0(doenca, ifelse(tipo == "obitos", "_obitos", "")))

dat_wide <- dat_agg %>%
  select(-doenca, -tipo) %>%
  pivot_wider(names_from = nome_var, values_from = valor)

dat_final <- dat_wide %>%
  rename(year = ano_num, week = semana_num) %>%
  mutate(month = lubridate::month(data_inicio_semana)) %>%
  select(-data_inicio_semana) %>%
  select(country, province, district, period, year, week, month, everything())

names(dat_final) <- epitrix::clean_labels(names(dat_final))

output_file <- here::here("data", "dados_preparados.xlsx")
write_xlsx(dat_final, output_file)

message(sprintf("Ficheiro preparado guardado em: %s (%d linhas, %d colunas, %d distritos, %d provincias)",
                 output_file, nrow(dat_final), ncol(dat_final),
                 n_distinct(dat_final$district), n_distinct(dat_final$province)))

dat <- dat_final
list_df <- list()
list_df_m <- list()
