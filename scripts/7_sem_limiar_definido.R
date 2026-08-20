# Doencas sem limiar formal: mesmo tratamento generico (>50% / duplicacao P2W)

dat$alert_raiva_p50p    <- unusual_inc_50p(dat, "Raiva", "raiva", gen_plot = plot_flag)
dat$alert_raiva_doubling <- unusual_inc_doubling(dat, "Raiva", "raiva", gen_plot = plot_flag)

dat$alert_sindrome_febril_p50p    <- unusual_inc_50p(dat, "Sindrome_Febril", "sindrome_febril", gen_plot = plot_flag)
dat$alert_sindrome_febril_doubling <- unusual_inc_doubling(dat, "Sindrome_Febril", "sindrome_febril", gen_plot = plot_flag)

dat$alert_mordeduras_animal_p50p    <- unusual_inc_50p(dat, "Mordeduras_Animal", "mordeduras_animal", gen_plot = plot_flag)
dat$alert_mordeduras_animal_doubling <- unusual_inc_doubling(dat, "Mordeduras_Animal", "mordeduras_animal", gen_plot = plot_flag)
