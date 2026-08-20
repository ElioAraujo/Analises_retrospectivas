# Malaria: percentil 75, CSUM, media+2DP

dat$alert_malaria_75p <- above_percentile(dat, "Malaria", "malaria", 0.75, gen_plot = plot_flag)
dat$alert_malaria_csum <- csum(dat, "Malaria", "malaria", gen_plot = plot_flag)
dat$alert_malaria_m2sd <- mean_2sd(dat, "Malaria", "malaria", gen_plot = plot_flag)
