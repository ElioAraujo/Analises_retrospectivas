# Limiar: um caso suspeito (Colera, PFA, Peste, Tetano Neonatal)

dat$alert_colera <- one_suspected_case(dat, "Colera", "colera", gen_plot = plot_flag)
dat$alert_pfa <- one_suspected_case(dat, "Paralisia_Flacida_Aguda", "pfa", gen_plot = plot_flag)
dat$alert_peste <- one_suspected_case(dat, "Peste", "peste", gen_plot = plot_flag)
dat$alert_tetano_neonatal <- one_suspected_case(dat, "Tetano_Neonatal", "tetano_neonatal", gen_plot = plot_flag)
