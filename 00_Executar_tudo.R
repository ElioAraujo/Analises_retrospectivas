rm(list = ls())

pacman::p_load(dplyr, magrittr, tidyverse, here, rio, lubridate,
                epitrix, zoo, gridExtra, grid, openxlsx)

plot_flag <- TRUE

ficheiros <- sort(list.files(here::here("scripts")), method = "radix")

inicio <- Sys.time()
for (ficheiro in ficheiros) {
  message(sprintf("A correr: %s", ficheiro))
  source(here::here("scripts", ficheiro))
}
print(Sys.time() - inicio)

if (!dir.exists(here::here("results"))) dir.create(here::here("results"))

write.xlsx(list_df,   file = here::here("results", "alertas_por_distrito_por_ano.xlsx"))
write.xlsx(list_df_m, file = here::here("results", "alertas_por_distrito_por_mes.xlsx"))

dat %<>% mutate_at(vars(contains("alert_")), unlist)
rio::export(dat, here::here("data", "dados_com_alertas.xlsx"))

dat_alertas <- dat %>%
  select(contains("alert_")) %>%
  mutate_at(vars(contains("alert_")), function(x) ifelse(is.na(x), 0, x) %>% unlist())

rio::export(dat_alertas, here::here("data", "apenas_alertas.xlsx"))

message("Pipeline concluido. Resultados em results/ e data/dados_com_alertas.xlsx")
