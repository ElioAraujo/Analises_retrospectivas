by_district_by_month <- function(dat, alert_var){
  dat %>% group_by(province, district, year, month) %>% summarize(
    n_alert = sum(!!sym(alert_var), na.rm=T),
  )
}

by_district_by_year <- function(dat, alert_var){
  dat %>% group_by(year, province, district) %>% summarize(
    n_alert = sum(!!sym(alert_var), na.rm=T)
  ) %>% spread(key = year, value = n_alert)
}

summary_label_province <- function(dat, alert_var){
  dat_text1 <- dat %>% group_by(year, province) %>% summarize(
    n_total = sum(!!sym(alert_var), na.rm=T)
  )
  
  dat_text2 <-dat %>% group_by(year, province, week) %>% summarize(
    n_alert = sum(!!sym(alert_var), na.rm=T)
  ) %>% ungroup() %>% group_by(year, province) %>% 
    summarize(
      avg_n_alert = mean(n_alert, na.rm=T)
    )
  
  dat_text <- merge(dat_text1, dat_text2, by = c("year", "province")) %>%
    mutate(
      label = sprintf("N = %.0f, Average %.1f alerts per week",
                      n_total, avg_n_alert)
    )
  return(dat_text)
}

summary_label_country <- function(dat, alert_var){
  dat_text1 <- dat %>% group_by(year) %>% summarize(
    n_total = sum(!!sym(alert_var), na.rm=T)
  )
  
  dat_text2 <-dat %>% group_by(year, week) %>% summarize(
    n_alert = sum(!!sym(alert_var), na.rm=T)
  ) %>% ungroup() %>% group_by(year) %>% 
    summarize(
      avg_n_alert = mean(n_alert, na.rm=T)
    )
  
  dat_text <- merge(dat_text1, dat_text2, by = c("year")) %>%
    mutate(
      label = sprintf("N = %.0f, Average %.1f alerts per week",
                      n_total, avg_n_alert)
    )
  return(dat_text)
}

summary_label_district <- function(dat, alert_var){
  dat_text1 <- dat %>% group_by(year, district) %>% summarize(
    n_total = sum(!!sym(alert_var), na.rm=T)
  )
  
  dat_text2 <-dat %>% group_by(year, district, week) %>% summarize(
    n_alert = sum(!!sym(alert_var), na.rm=T)
  ) %>% ungroup() %>% group_by(year, district) %>% 
    summarize(
      avg_n_alert = mean(n_alert, na.rm=T)
    )
  
  dat_text <- merge(dat_text1, dat_text2, by = c("year", "district")) %>%
    mutate(
      label = sprintf("N = %.0f, Average %.1f alerts per week",
                      n_total, avg_n_alert)
    )
  return(dat_text)
}

plot_alert_summary <- function(dat, alert_var, title){
  
  # numa analise nacional (varias provincias): mostra pais -> provincia
  # numa analise scoped a uma so provincia (ex: results_provincias/Sofala): mostra provincia -> distrito
  n_provincias <- n_distinct(dat$province)
  
  if (n_provincias == 1){
    
    dat_text_top <- summary_label_province(dat, alert_var)
    dat_text_bottom <- summary_label_district(dat, alert_var)
    
    p_n_alerts <- gridExtra::grid.arrange(
      textGrob(title, gp = gpar(fontsize = 13, fontface = 'bold')),
      # total da provincia
      dat %>% group_by(year, province, week) %>% summarize(
        n_district = sum(!!sym(alert_var), na.rm=T)
      ) %>% ggplot(aes(week, n_district)) + geom_bar(stat = "identity") +
        theme_bw()+ 
        ylab("# of districts\nabove the alert threshold") +
        facet_grid(province~year) +
        geom_text(
          data    = dat_text_top,
          mapping = aes(x = -Inf, y = -Inf, label = label),
          hjust   = -0.1,
          vjust   = -30
        ),
      # por distrito
      dat %>% group_by(year, district, week) %>% summarize(
        n_alert = sum(!!sym(alert_var), na.rm=T)
      ) %>% ggplot(aes(week, n_alert)) + geom_bar(stat = "identity") +
        theme_bw()+
        ylab("# of alerts") +
        facet_grid(district~year)+
        geom_text(
          data    = dat_text_bottom,
          mapping = aes(x = -Inf, y = -Inf, label = label),
          hjust   = -0.1,
          vjust   = -5
        ),
      heights = c(0.1, 1, 2),
      nrow = 3)
    
    return(p_n_alerts)
  }
  
  # number of districts surpassing the alert per province
  
  dat_text_country <- summary_label_country(dat, alert_var)
  dat_text_province <- summary_label_province(dat, alert_var)
  
  p_n_alerts <- gridExtra::grid.arrange(
    textGrob(title, gp = gpar(fontsize = 13, fontface = 'bold')),
    # nation wide
    dat %>% group_by(year, country, province, week) %>% summarize(
      n_district = sum(!!sym(alert_var), na.rm=T)
    ) %>% ggplot(aes(week, n_district)) + geom_bar(stat = "identity") +
      theme_bw()+ 
      ylab("# of districts\nabove the alert threshold") +
      facet_grid(country~year) +
      geom_text(
        data    = dat_text_country,
        mapping = aes(x = -Inf, y = -Inf, label = label),
        hjust   = -0.1,
        vjust   = -30
      ),
    
    dat %>% group_by(year, province, week) %>% summarize(
      n_district = sum(!!sym(alert_var), na.rm=T)
    ) %>% ggplot(aes(week, n_district)) + geom_bar(stat = "identity") +
      theme_bw()+
      ylab("# of districts\nabove the alert threshold") +
      facet_grid(province~year)+
      geom_text(
        data    = dat_text_province,
        mapping = aes(x = -Inf, y = -Inf, label = label),
        hjust   = -0.1,
        vjust   = -5
      ),
    heights = c(0.1, 1, 2),
    nrow = 3)
  
  return(p_n_alerts)
}

## mark the weeks that pass the alert
one_suspected_case <- function(dat, disease, varname, gen_plot = F, pasta_base = "results"){
  
  alert_var <- paste0("alert_", epitrix::clean_labels(disease))
  if (!dir.exists(here::here(pasta_base, disease))) dir.create(here::here(pasta_base, disease), recursive = TRUE)
  dat %<>% mutate(
    {{alert_var}} := ifelse(!!sym(varname)>=1, 1, NA)
  )
  
  if(gen_plot){
  ## Example plot
  # All districts
  p_dist <- dat %>% ggplot() + 
    geom_line(aes(week, .data[[varname]], group = year, color = factor(year)), linewidth = 0.8) +
    geom_area(aes(x=week, y=1), fill = "#6a96de", alpha = 0.2)+
    geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.9) +
    facet_wrap(.~district, scales = "free_y") + 
    theme(legend.position = "top")+
    scale_color_brewer(palette = "Viridis", name = "Year")+
    ggtitle(paste0(disease, " (single suspected case)")) +
    ylab("Number of cases vs. alert threshold") + xlab("Week")
  
  ggsave(p_dist,
         filename = here::here(pasta_base,  disease, paste0("p_dist_", tolower(disease), ".png")),
         width = 20, height = 12, dpi = 200)
  
  
  # number of districts surpassing the alert per province
  p_n_alerts <- plot_alert_summary(dat, alert_var, paste0(disease, '  (single suspected case)'))
  
  ggsave(p_n_alerts,
         filename = here::here(pasta_base,  disease, paste0("p_n_alerts_", tolower(disease), ".png")),
         width = 15, height = 15, dpi = 200)
  
  p_dist
  p_n_alerts
  }
  
  name <- disease
  
  list_df[[name]] <<- by_district_by_year(dat, alert_var)
  list_df_m[[name]] <<- by_district_by_month(dat, alert_var)
  
  write.xlsx(list(district_by_year = list_df[[name]],
                  district_by_month = list_df_m[[name]]), 
             file = here::here(pasta_base, disease, paste0(name, "_alert_by_district.xlsx")))
  
  return(dat[,alert_var])
  
}


unusual_inc_50p <- function(dat, disease, varname, gen_plot = F, pasta_base = "results"){
  alert_var <- paste0("alert_", epitrix::clean_labels(disease))
  if (!dir.exists(here::here(pasta_base, disease))) dir.create(here::here(pasta_base, disease), recursive = TRUE)
  
  # 1.5 times increase vs. the recent trend
  dat %<>% arrange(province, district, year, week) %>%
    group_by(province, district) %>%
    mutate(
      p3w_avg = zoo::rollmean(!!sym(varname), k=3, fill=NA, align = "right")) %>%
    mutate(
      p3w_avg = lag(p3w_avg, 1, default = NA)
    ) %>%
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) > 1.5*p3w_avg, 1, NA)
    )
  
  if(gen_plot){
  ## Example plot
  # All districts
  p_dist <- dat %>% ggplot() + 
    geom_line(aes(week, .data[[varname]], group = year, color = factor(year)), linewidth = 0.8) +
    geom_area(aes(x=week, y=p3w_avg*1.5, fill = factor(year)), alpha = 0.5)+
    geom_line(aes(week, p3w_avg*1.5, group = year), color = "red", linetype = "dashed", linewidth = 0.7) +
    facet_wrap(.~district, scales = "free_y") + 
    theme(legend.position = "top")+
    scale_color_brewer(palette = "Set1", name = "Year")+
    scale_fill_brewer(palette = "Set1", name = "Year")+
    ggtitle(paste0(disease, " (>50% increase vs. P3W)")) +
    ylab("Number of cases vs. alert threshold") + xlab("Week")
  
  ggsave(p_dist,
         filename = here::here(pasta_base,  disease, paste0("p_dist_", tolower(disease), "_p50p.png")),
         width = 20, height = 12, dpi = 200)
  
  
  
  # number of districts surpassing the alert per province
  p_n_alerts <- plot_alert_summary(dat, alert_var, paste0(disease, '  (>50% increase vs. P3W)'))
  
  ggsave(p_n_alerts,
         filename = here::here(pasta_base,  disease, paste0("p_n_alerts_", tolower(disease), "_p50p.png")),
         width = 15, height = 15, dpi = 200)
  
  p_dist
  p_n_alerts
  }
  name <- paste0(disease, " 50p increase")
  
  list_df[[name]] <<- by_district_by_year(dat, alert_var)
  list_df_m[[name]] <<- by_district_by_month(dat, alert_var)
  
  write.xlsx(list(district_by_year = list_df[[name]],
                  district_by_month = list_df_m[[name]]), 
             file = here::here(pasta_base, disease, paste0(name, "_alert_by_district.xlsx")))
  
  return(dat[,alert_var])
  
}

unusual_inc_doubling <- function(dat, disease, varname, gen_plot = F, pasta_base = "results"){
  alert_var <- paste0("alert_", epitrix::clean_labels(disease))
  if (!dir.exists(here::here(pasta_base, disease))) dir.create(here::here(pasta_base, disease), recursive = TRUE)
  
  # 1.5 times increase vs. the recent trend
  dat %<>% arrange(province, district, year, week) %>%
    group_by(province, district) %>%
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) > 2*lag(!!sym(varname)) &
                                lag(!!sym(varname)) > 2*lag(!!sym(varname), 2), 1, NA)
    )
  
  name <- paste0(disease, " doubling p2w")
  
  list_df[[name]] <<- by_district_by_year(dat, alert_var)
  list_df_m[[name]] <<- by_district_by_month(dat, alert_var)
  
  write.xlsx(list(district_by_year = list_df[[name]],
                  district_by_month = list_df_m[[name]]), 
             file = here::here(pasta_base, disease, paste0(name, "_alert_by_district.xlsx")))
  
  if(gen_plot){
  ## Example plot
  # All districts
  p_dist <- dat %>% ggplot() + 
    geom_line(aes(week, .data[[varname]], group = year, color = factor(year)), linewidth = 0.8) +
    geom_area(aes(x=week, y=2*lag(.data[[varname]]), fill = factor(year)), alpha = 0.5)+
    geom_line(aes(week, 2*lag(.data[[varname]]), group = year), color = "red", linetype = "dashed", linewidth = 0.7) +
    facet_wrap(.~district, scales = "free_y") + 
    theme(legend.position = "top")+
    scale_color_brewer(palette = "Set1", name = "Year")+
    scale_fill_brewer(palette = "Set1", name = "Year")+
    ggtitle(paste0(disease, " (Doubling of cases in the P2W)")) +
    ylab("Number of cases vs. alert threshold") + xlab("Week")
  
  ggsave(p_dist,
         filename = here::here(pasta_base,  disease, paste0("p_dist_", tolower(disease), "_doubling.png")),
         width = 20, height = 12, dpi = 200)
  
  # number of districts surpassing the alert per province
  p_n_alerts <- plot_alert_summary(dat, alert_var, paste0(disease, '  (Doubling of cases in the P2W)'))
  
  ggsave(p_n_alerts,
         filename = here::here(pasta_base,  disease, paste0("p_n_alerts_", tolower(disease), "_doubling.png")),
         width = 15, height = 15, dpi = 200)
  
  p_dist
  p_n_alerts
  }
  
  return(dat[,alert_var])
  
}

susp_cluster <- function(dat, disease, varname, n_cluster, gen_plot=F, pasta_base = "results"){
  alert_var <- paste0("alert_", epitrix::clean_labels(disease))
  if (!dir.exists(here::here(pasta_base, disease))) dir.create(here::here(pasta_base, disease), recursive = TRUE)
  
  # 1.5 times increase vs. the recent trend
  dat %<>% arrange(province, district, year, week) %>%
    group_by(province, district) %>%
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) >= n_cluster, 1, NA)
    )
  
  if(gen_plot){
  ## Example plot
  # All districts
  p_dist <- dat %>% ggplot() + 
    geom_line(aes(week, .data[[varname]], group = year, color = factor(year)), linewidth = 0.8) +
    geom_area(aes(x=week, y=n_cluster),fill = "#6a96de", alpha = 0.2)+
    geom_hline(yintercept = n_cluster, color = "red", linetype = "dashed", linewidth = 0.9) +
    facet_wrap(.~district, scales = "free_y") + 
    theme(legend.position = "top")+
    scale_color_brewer(palette = "Set1", name = "Year")+
    ggtitle(paste0(disease, sprintf(" (Suspected cluster of n>=%.0f)", n_cluster))) +
    ylab("Number of cases vs. alert threshold") + xlab("Week")
  
  ggsave(p_dist,
         filename = here::here(pasta_base,  disease, paste0("p_dist_", tolower(disease), ".png")),
         width = 20, height = 12, dpi = 200)
  
  
  # number of districts surpassing the alert per province
  p_n_alerts <- plot_alert_summary(dat, alert_var, paste0(disease, sprintf(" (Suspected cluster of n>=%.0f)", n_cluster)))
  
  ggsave(p_n_alerts,
         filename = here::here(pasta_base,  disease, paste0("p_n_alerts_", tolower(disease), ".png")),
         width = 15, height = 15, dpi = 200)
  
  p_dist
  p_n_alerts
  }
  
  name <- disease
  
  list_df[[name]] <<- by_district_by_year(dat, alert_var)
  list_df_m[[name]] <<- by_district_by_month(dat, alert_var)
  
  write.xlsx(list(district_by_year = list_df[[name]],
                  district_by_month = list_df_m[[name]]), 
             file = here::here(pasta_base, disease, paste0(name, "_alert_by_district.xlsx")))
  
  return(dat[,alert_var])
  
}

## Cluster suspeito numa janela deslizante de N dias (documento tecnico MZ, Julho 2026: Sarampo = >=5 casos suspeitos em 28 dias)
## NOTA: o criterio adicional do documento (>=2 casos CONFIRMADOS) nao esta implementado aqui -
## o BES do SISMA nao tem uma coluna separada de "confirmados" para o Sarampo, so casos suspeitos por faixa etaria
susp_cluster_janela <- function(dat, disease, varname, n_cluster, dias_janela = 28, gen_plot = F, pasta_base = "results"){
  
  alert_var <- paste0("alert_", epitrix::clean_labels(disease))
  if (!dir.exists(here::here(pasta_base, disease))) dir.create(here::here(pasta_base, disease), recursive = TRUE)
  n_semanas <- ceiling(dias_janela / 7)
  
  dat %<>% arrange(province, district, year, week) %>%
    group_by(province, district) %>%
    mutate(
      casos_janela = zoo::rollapply(!!sym(varname), width = n_semanas, FUN = sum, na.rm = TRUE,
                                     partial = TRUE, align = "right")
    ) %>%
    mutate(
      {{alert_var}} := ifelse(casos_janela >= n_cluster, 1, NA)
    ) %>%
    ungroup()
  
  if(gen_plot){
  ## Example plot
  # All districts
  p_dist <- dat %>% ggplot() + 
    geom_line(aes(week, casos_janela, group = year, color = factor(year)), linewidth = 0.8) +
    geom_area(aes(x=week, y=n_cluster), fill = "#6a96de", alpha = 0.2)+
    geom_hline(yintercept = n_cluster, color = "red", linetype = "dashed", linewidth = 0.9) +
    facet_wrap(.~district, scales = "free_y") + 
    theme(legend.position = "top")+
    scale_color_brewer(palette = "Set1", name = "Year")+
    ggtitle(paste0(disease, sprintf(" (Cluster suspeito: >=%.0f casos em %.0f dias)", n_cluster, dias_janela))) +
    ylab(sprintf("Casos suspeitos acumulados em %.0f dias vs. limiar", dias_janela)) + xlab("Week")
  
  ggsave(p_dist,
         filename = here::here(pasta_base,  disease, paste0("p_dist_", tolower(disease), "_janela.png")),
         width = 20, height = 12, dpi = 200)
  
  # number of districts surpassing the alert per province
  p_n_alerts <- plot_alert_summary(dat, alert_var, paste0(disease, sprintf(" (Cluster suspeito: >=%.0f casos em %.0f dias)", n_cluster, dias_janela)))
  
  ggsave(p_n_alerts,
         filename = here::here(pasta_base,  disease, paste0("p_n_alerts_", tolower(disease), "_janela.png")),
         width = 15, height = 15, dpi = 200)
  
  p_dist
  p_n_alerts
  }
  
  name <- disease
  
  list_df[[name]] <<- by_district_by_year(dat, alert_var)
  list_df_m[[name]] <<- by_district_by_month(dat, alert_var)
  
  write.xlsx(list(district_by_year = list_df[[name]],
                  district_by_month = list_df_m[[name]]), 
             file = here::here(pasta_base, disease, paste0(name, "_alert_by_district.xlsx")))
  
  return(dat[,alert_var])
  
}

above_percentile <- function(dat, disease, varname, p_cutoff, gen_plot = F, pasta_base = "results"){
  
  alert_var <- paste0("alert_", epitrix::clean_labels(disease))
  if (!dir.exists(here::here(pasta_base, disease))) dir.create(here::here(pasta_base, disease), recursive = TRUE)
  
  # 1.5 times increase vs. the recent trend
  dat %<>% arrange(province, district, week, year) %>%
    group_by(province, district, week) %>%
    mutate(
      p_threshold =  quantile(c(lag(!!sym(varname)),
                                lag(!!sym(varname), 2),
                                lag(!!sym(varname), 3),
                                lag(!!sym(varname), 4),
                                lag(!!sym(varname), 5)), p_cutoff, na.rm=T)
    )%>%
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) > p_threshold, 1, NA)
    )
  
  if(gen_plot){
  ## Example plot
  # All districts
  p_dist <- dat %>% ggplot() + 
    geom_line(aes(week, .data[[varname]], group = year, color = factor(year)), linewidth = 0.8) +
    geom_area(aes(x=week, y=p_threshold), fill = "#6a96de", alpha = 0.5)+
    geom_line(aes(week, p_threshold, group = year), color = "red", linetype = "dashed", linewidth = 0.7) +
    facet_wrap(.~district, scales = "free_y") + 
    theme(legend.position = "top")+
    scale_color_brewer(palette = "Set1", name = "Year")+
    ggtitle(paste0(disease, sprintf(" (Above %.0fth percentile)", p_cutoff*100))) +
    ylab("Number of cases vs. alert threshold") + xlab("Week")
  
  ggsave(p_dist,
         filename = here::here(pasta_base,  disease, paste0("p_dist_", tolower(disease), p_cutoff*100, "p.png")),
         width = 20, height = 12, dpi = 200)
  
  
  # number of districts surpassing the alert per province
  p_n_alerts <- plot_alert_summary(dat, alert_var, paste0(disease, sprintf(" (Above %.0fth percentile)", p_cutoff*100)))
  
  ggsave(p_n_alerts,
         filename = here::here(pasta_base,  disease, paste0("p_n_alerts_", tolower(disease), p_cutoff*100, "p.png")),
         width = 15, height = 15, dpi = 200)
  
  p_dist
  p_n_alerts
  }
  
  name <- paste0(disease, p_cutoff*100, "p")
  
  list_df[[name]] <<- by_district_by_year(dat, alert_var)
  list_df_m[[name]] <<- by_district_by_month(dat, alert_var)
  
  write.xlsx(list(district_by_year = list_df[[name]],
                  district_by_month = list_df_m[[name]]), 
             file = here::here(pasta_base, disease, paste0(name, "_alert_by_district.xlsx")))
  
  
  return(dat[,alert_var])
  
}

csum <- function(dat, disease, varname, gen_plot = F, pasta_base = "results"){
  
  alert_var <- paste0("alert_", epitrix::clean_labels(disease))
  if (!dir.exists(here::here(pasta_base, disease))) dir.create(here::here(pasta_base, disease), recursive = TRUE)
  
  # 1.5 times increase vs. the recent trend
  dat %<>% arrange(province, district, year, week) %>%
    group_by(province, district) %>%
    mutate(
      p3w_avg = zoo::rollmean(!!sym(varname), k=3, fill=NA, align = "right")
      
    )%>% ungroup() %>%
    arrange(province, district, week, year) %>%
    group_by(province, district, week) %>%
    mutate(
      csum = lag(zoo::rollmean(p3w_avg, k=3, fill=NA, align = "right"))
    )%>% 
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) > csum, 1, NA)
    )
  
  if(gen_plot){
  ## Example plot
  # All districts
  p_dist <- dat %>% ggplot() + 
    geom_line(aes(week, .data[[varname]], group = year, color = factor(year)), linewidth = 0.8) +
    geom_area(aes(x=week, y=csum), fill = "#6a96de", alpha = 0.5)+
    geom_line(aes(week, csum, group = year), color = "red", linetype = "dashed", linewidth = 0.7) +
    facet_wrap(.~district, scales = "free_y") + 
    theme(legend.position = "top")+
    scale_color_brewer(palette = "Set1", name = "Year")+
    ggtitle(paste0(disease, "(C-SUM)")) +
    ylab("Number of cases vs. alert threshold") + xlab("Week")
  
  ggsave(p_dist,
         filename = here::here(pasta_base,  disease, paste0("p_dist_", tolower(disease), "_csum.png")),
         width = 20, height = 12, dpi = 200)
  
  # number of districts surpassing the alert per province
  p_n_alerts <- plot_alert_summary(dat, alert_var, paste0(disease, "(C-SUM)"))
  
  ggsave(p_n_alerts,
         filename = here::here(pasta_base,  disease, paste0("p_n_alerts_", tolower(disease), "_csum.png")),
         width = 15, height = 15, dpi = 200)
  
  p_dist
  p_n_alerts
  }
  
  name <- paste0(disease, "_csum")
  
  list_df[[name]] <<- by_district_by_year(dat, alert_var)
  list_df_m[[name]] <<- by_district_by_month(dat, alert_var)
  
  write.xlsx(list(district_by_year = list_df[[name]],
                  district_by_month = list_df_m[[name]]), 
             file = here::here(pasta_base, disease, paste0(name, "_alert_by_district.xlsx")))
  
  
  return(dat[,alert_var])
  
}

mean_2sd <- function(dat, disease, varname, gen_plot = F, pasta_base = "results"){
  
  alert_var <- paste0("alert_", epitrix::clean_labels(disease))
  if (!dir.exists(here::here(pasta_base, disease))) dir.create(here::here(pasta_base, disease), recursive = TRUE)
  
  # 1.5 times increase vs. the recent trend
  dat %<>% 
    arrange(province, district, week, year) %>%
    group_by(province, district, week) %>%
    mutate(
      mean = lag(zoo::rollmean(!!sym(varname), k=3, fill=NA, align = "right")),
      sd = lag(zoo::rollapply(!!sym(varname), width=3, fill = NA, sd, align = "right")),
      mean_2sd = mean+2*sd
    )%>%
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) > mean_2sd, 1, NA)
    )
  
  if(gen_plot){
  ## Example plot
  # All districts
  p_dist <- dat %>% ggplot() + 
    geom_line(aes(week, .data[[varname]], group = year, color = factor(year)), linewidth = 0.8) +
    geom_area(aes(x=week, y=mean_2sd), fill = "#6a96de", alpha = 0.5)+
    geom_line(aes(week, mean_2sd, group = year), color = "red", linetype = "dashed", linewidth = 0.7) +
    facet_wrap(.~district, scales = "free_y") + 
    theme(legend.position = "top")+
    scale_color_brewer(palette = "Set1", name = "Year")+
    ggtitle(paste0(disease, "(Mean + 2*SD)")) +
    ylab("Number of cases vs. alert threshold") + xlab("Week")
  
  ggsave(p_dist,
         filename = here::here(pasta_base,  disease, paste0("p_dist_", tolower(disease), "_m2sd.png")),
         width = 20, height = 12, dpi = 200)
  
  # number of districts surpassing the alert per province
  dat_text_country <- summary_label_country(dat, alert_var)
  dat_text_province <- summary_label_province(dat, alert_var)
  
  p_n_alerts <- plot_alert_summary(dat, alert_var, paste0(disease, " (Mean + 2*SD)"))
  
  ggsave(p_n_alerts,
         filename = here::here(pasta_base,  disease, paste0("p_n_alerts_", tolower(disease), "_m2sd.png")),
         width = 15, height = 15, dpi = 200)
  
  p_dist
  p_n_alerts
  }
  
  
  name <- paste0(disease, "_m2sd")
  
  list_df[[name]] <<- by_district_by_year(dat, alert_var)
  list_df_m[[name]] <<- by_district_by_month(dat, alert_var)
  
  write.xlsx(list(district_by_year = list_df[[name]],
                  district_by_month = list_df_m[[name]]), 
             file = here::here(pasta_base, disease, paste0(name, "_alert_by_district.xlsx")))
  
  
  return(dat[,alert_var])
  
}

attack_rate <- function(dat, disease, varname, pop, ar_cutoff, gen_plot=F, pasta_base = "results"){
  
  alert_var <- paste0("alert_", epitrix::clean_labels(disease))
  if (!dir.exists(here::here(pasta_base, disease))) dir.create(here::here(pasta_base, disease), recursive = TRUE)
  
  # join with the pop data, usando a populacao do proprio ano (pop tem colunas clean_name, ano, populacao)
  # anos fora do intervalo coberto pela projeccao populacional usam o ano mais proximo disponivel
  pop_anos <- sort(unique(pop$ano))
  dat %<>% mutate(
    clean_name = epitrix::clean_labels(district),
    ano_pop = pmin(pmax(year, min(pop_anos)), max(pop_anos))
  ) %>% left_join(pop, by = c("clean_name" = "clean_name", "ano_pop" = "ano")) %>%
    select(-clean_name, -ano_pop)
  
  # Calculate the attack rate
  dat %<>% 
    mutate(
      attack_rate = !!sym(varname)/populacao *100000
    )%>%
    mutate(
      {{alert_var}} := ifelse(attack_rate > ar_cutoff, 1, NA)
    )
  
  if(gen_plot){
  ## Example plot
  # All districts
  p_dist <- dat %>% ggplot() + 
    geom_line(aes(week, attack_rate, group = year, color = factor(year)), linewidth = 0.8) +
    geom_area(aes(x=week, y=ar_cutoff), fill = "#6a96de", alpha = 0.5)+
    geom_hline(yintercept = ar_cutoff, color = "red", linetype = "dashed", linewidth = 0.9) +
    facet_wrap(.~district, scales = "free_y") + 
    theme(legend.position = "top")+
    scale_color_brewer(palette = "Set1", name = "Year")+
    ggtitle(paste0(disease, " (Attack rate per 100,000 > ", ar_cutoff, ")")) +
    ylab("Attack rate vs. alert threshold") + xlab("Week")
  
  ggsave(p_dist,
         filename = here::here(pasta_base, disease, paste0("p_dist_", tolower(disease), ".png")),
         width = 20, height = 12, dpi = 200)
  
  # number of districts surpassing the alert per province
  dat_text_country <- summary_label_country(dat, alert_var)
  dat_text_province <- summary_label_province(dat, alert_var)
  
  p_n_alerts <- plot_alert_summary(dat, alert_var, paste0(disease, " (Attack rate per 100,000 > ", ar_cutoff, ")"))
  
  ggsave(p_n_alerts,
         filename = here::here(pasta_base, disease, paste0("p_n_alerts_", tolower(disease), ".png")),
         width = 15, height = 15, dpi = 200)
  
  p_dist
  p_n_alerts
  }
  
  name <- disease
  
  list_df[[name]] <<- by_district_by_year(dat, alert_var)
  list_df_m[[name]] <<- by_district_by_month(dat, alert_var)
  
  write.xlsx(list(district_by_year = list_df[[name]],
                  district_by_month = list_df_m[[name]]), 
             file = here::here(pasta_base, disease, paste0(name, "_alert_by_district.xlsx")))
  
  
  return(dat[,alert_var])
  
}

