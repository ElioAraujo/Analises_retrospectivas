# Cluster suspeito (Sarampo): >=5 casos suspeitos numa janela de 28 dias, conforme documento tecnico de Julho de 2026
# NOTA: o criterio adicional do documento (>=2 casos CONFIRMADOS) nao esta implementado -
# o BES do SISMA nao tem uma coluna separada de "confirmados" para o Sarampo (soma apenas casos suspeitos por faixa etaria)

dat$alert_sarampo <- susp_cluster_janela(dat, "Sarampo", "sarampo", n_cluster = 5, dias_janela = 28, gen_plot = plot_flag)
