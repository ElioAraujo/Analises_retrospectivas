#################### Generate mock-up outputs for DG's dashboard ######################

# Install or load packages
pacman::p_load(dplyr, magrittr, tidyverse, ggplot2, here, rio, lubridate, 
               epitrix, ggthemr, zoo, gridExtra, grid, openxlsx, zoo, gt)
ggthemr("fresh")

dat <- rio::import(here::here("data", "dados_com_alertas.xlsx"))

# resumo de alertas da semana mais recente
dat %>% arrange(year, week) %>% select(year, week) %>% tail()

ultima_semana <- dat %>% filter(!is.na(week)) %>% arrange(year, week) %>% tail(1) %>% select(year, week)

dat_week <- dat %>% filter(year==ultima_semana$year, week==ultima_semana$week)
dat_week$total_alert <- dat_week %>% select(contains("alert_")) %>% apply(1, sum, na.rm=T)

tab_this_week <- dat_week %>% group_by(province) %>% summarize(total_alert = sum(total_alert, na.rm=T))

tab_this_week$verified <- (tab_this_week$total_alert * 
  sample(x = seq(0, 1, by = 0.01), size = nrow(tab_this_week), replace = F)) %>% round(0)

tab_this_week %<>% mutate(
  pending = total_alert - verified
)

tab_this_week$substantiated <- (tab_this_week$verified *   
  sample(x = seq(0, 1, by = 0.01), size = nrow(tab_this_week), replace = F)) %>% round(0)

tab_this_week %<>% mutate(
  discarded = verified - substantiated
)

tab_this_week %>% gt::gt(rowname_col = "province") %>%
  cols_label(
    total_alert = "Novos alertas",
    verified = "Verificados",
    pending = "Pendentes",
    substantiated = "Confirmados",
    discarded = "Descartados"
  ) %>%
  tab_spanner(label = md("**Em verificacao**"),
              columns = c(verified, pending),
              level = 2
  ) %>%
  tab_spanner(label = md("**Verificados**"),
              columns = c(substantiated, discarded),
              level = 2) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan")
    ),
    locations = cells_body(
      columns = verified,
      rows = verified==total_alert
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightpink")
    ),
    locations = cells_body(
      columns = pending,
      rows = pending>0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
    ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_stub()
  )


# Total acumulado no ano

dat_cum <- dat %>% filter(year==ultima_semana$year) 
dat_cum$total_alert <- dat_cum %>% select(contains("alert_")) %>% apply(1, sum, na.rm=T)

tab_this_cum <- dat_cum %>% group_by(province) %>% summarize(total_alert = sum(total_alert, na.rm=T))

tab_this_cum$verified <- (tab_this_cum$total_alert * 
  sample(x = seq(0.9, 1, by = 0.01), size = nrow(tab_this_cum), replace = F)) %>% round(0)

tab_this_cum %<>% mutate(
  pending = total_alert - verified
)

tab_this_cum$substantiated <- (tab_this_cum$verified *   
  sample(x = seq(0, 0.3, by = 0.01), size = nrow(tab_this_cum), replace = F)) %>% round(0)

tab_this_cum %<>% mutate(
  discarded = verified - substantiated
)

tab_this_cum %>% gt::gt(rowname_col = "province") %>%
  cols_label(
    total_alert = "Novos alertas",
    verified = "Verificados",
    pending = "Pendentes",
    substantiated = "Confirmados",
    discarded = "Descartados"
  ) %>%
  tab_spanner(label = md("**Em verificacao**"),
              columns = c(verified, pending),
              level = 2
  ) %>%
  tab_spanner(label = md("**Verificados**"),
              columns = c(substantiated, discarded),
              level = 2) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan")
    ),
    locations = cells_body(
      columns = verified,
      rows = verified==total_alert
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightpink")
    ),
    locations = cells_body(
      columns = pending,
      rows = pending>0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_stub()
  )

# Map

zambia <- sf::read_sf(here::here("data", "Shapefiles", "zmb_admbnda_adm2_dmmu_20201124.shp"))
zambia_province <-  sf::read_sf(here::here("data", "Shapefiles", "zmb_admbnda_adm1_dmmu_20201124.shp"))
zambia %<>% left_join(dat_week, by = join_by(ADM2_EN ==district))
ggplot(zambia) + geom_sf(aes(fill=total_alert)) + theme_void() +
  scale_fill_gradient(low ="white", name = "Number of new alerts") +
  geom_sf(data = zambia_province, aes(), fill = NA, color = "black", linewidth = 1.5) 
  
dat_cum2 <- dat_cum %>% group_by(district) %>% summarize(total_alert = sum(total_alert, na.rm=T))
zambia <- sf::read_sf(here::here("data", "Shapefiles", "zmb_admbnda_adm2_dmmu_20201124.shp"))
zambia_province <-  sf::read_sf(here::here("data", "Shapefiles", "zmb_admbnda_adm1_dmmu_20201124.shp"))
zambia %<>% left_join(dat_cum2, by = join_by(ADM2_EN ==district))
ggplot(zambia) + geom_sf(aes(fill=total_alert)) + theme_void() +
  scale_fill_gradient(low ="white", high = "#ab0748",name = "Cumulative number of alerts (YTD)") +
  geom_sf(data = zambia_province, aes(), fill = NA, color = "black", linewidth = 1.5) 

