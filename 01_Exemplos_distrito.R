pacman::p_load(dplyr, magrittr, tidyverse, ggplot2, here, rio, lubridate,
               epitrix, ggthemr, zoo, gridExtra, grid, openxlsx, zoo)
ggthemr("fresh")

# Corre depois de 00_Executar_tudo.R. Se "dat" nao estiver na sessao, carrega do ficheiro final.
if (!exists("dat")) {
  dat <- rio::import(here::here("data", "dados_com_alertas.xlsx"))
}

# Distritos de exemplo (um por provincia, capitais/cidades)
district_names <- c("CIDADE DA BEIRA",
                     "DISTRITO DE NAMPULA",
                     "MATOLA",
                     "CIDADE DE CHIMOIO",
                     "CIDADE DE QUELIMANE",
                     "CIDADE DE PEMBA",
                     "DISTRITO DE LICHINGA",
                     "CIDADE DE TETE",
                     "CIDADE DE XAI-XAI",
                     "CIDADE DE INHAMBANE",
                     "DONDO",
                     "MOCUBA")

dat_alert <- dat %>% select(province, district, year, week, month, contains("alert_"))
alert_vars <- colnames(dat_alert)[str_detect(colnames(dat_alert), "alert_")]

dat_alert %<>% mutate_if(is.list, unlist)
dat_alert %<>% mutate_at(alert_vars, function(x){ifelse(is.na(x), 0, x)})

dat_alert %<>% gather(disease, alert, starts_with("alert_"))

dat_alert %<>% mutate(
  disease = toupper(str_replace_all(str_remove(disease, "alert_"), "_", " "))
)

if (!dir.exists(here::here("results", "distritos"))) dir.create(here::here("results", "distritos"), recursive = TRUE)

for(dist in district_names){
  p_overall <- dat_alert %>% filter(district==dist) %>% group_by(year, week) %>%
    summarize(n_alert = sum(alert, na.rm=T)) %>%
    ggplot(aes(week, n_alert)) + geom_bar(stat = 'identity') +
    facet_wrap(.~year, nrow = 1) +
    ggtitle(paste0(dist, " - Total de alertas gerados por semana")) +
    theme(strip.text = element_text(face = "bold"))+
    xlab("Semana") + ylab("Numero de alertas gerados por semana")

  rmarkdown::render(input = here::here("scripts_distrito", "1_resumo_distrito.Rmd"),
                    output_file = paste0(dist, " relatorio.html"),
                    output_dir = here::here("results", "distritos", dist))

}
