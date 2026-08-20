# Meningite: taxa de ataque > 3/100.000 (populacao por distrito e ano, 2022-2026)

pop <- rio::import(here::here("data", "Pop_Moz.xlsx")) %>%
  select(clean_name, ano, populacao)

dat$alert_meningite <- attack_rate(dat, "Meningite", "meningite", pop = pop, ar_cutoff = 3, gen_plot = plot_flag)
