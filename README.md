# Análises Retrospectivas — Limiares de Alerta (Moçambique)

Adaptação do pipeline de limiares de alerta da ZNPHI/RTSL (Zâmbia) para os
dados de vigilância BES do SISMA (MISAU), no âmbito da estratégia de
Vigilância Colaborativa 2026–2028.

## Estrutura (espelha o repositório original da Zâmbia)

```
Analises_retrospectivas/
├── 00_Executar_tudo.R          # corre todo o pipeline, script a script
├── 01_Exemplos_distrito.R      # gera os relatórios HTML por distrito (corre depois do 00)
├── 02_Analises_provincia.R     # reaplica as mesmas análises só a Sofala e Nampula, com saída separada (corre depois do 00)
├── data/
│   ├── Data_BES.xls            # export bruto do SISMA (000_dhis2_analytics.R)
│   ├── dados_preparados.xlsx   # dados no formato do pipeline (1_preparacao_dados.R)
│   └── Pop_Moz.xlsx            # população por distrito/ano 2022-2026 (usada na Meningite)
├── scripts/
│   ├── 000_dhis2_analytics.R   # download dos dados via API do SISMA
│   ├── 0_functions.R           # funções de regras de alerta (iguais à Zâmbia)
│   ├── 1_preparacao_dados.R    # BES bruto -> formato do pipeline
│   └── 2..8_*.R                # regras de alerta por doença (a construir)
├── scripts_distrito/
│   └── 1_resumo_distrito.Rmd   # relatório HTML por distrito
├── script_others/
│   ├── alert_defining_functions.R
│   └── dashboard_mock-up.R     # mock-up do painel para a Direcção-Geral
└── results/                    # saídas do pipeline (alertas por distrito/ano/mês)
```

## Estado actual

- [x] `000_dhis2_analytics.R` — download automatizado via API do SISMA
- [x] `1_preparacao_dados.R` — conversão do export BES para o formato do pipeline
- [x] `2_um_caso_suspeito.R` — Cólera, PFA, Peste, Tétano Neonatal (idêntico à Zâmbia)
- [x] `3_tendencia_crescente.R` — Diarreia, Disenteria (idêntico à Zâmbia: diarreia com/sem sangue)
- [x] `4_cluster_suspeito.R` — Sarampo: **≥5 casos suspeitos numa janela de 28 dias** (`susp_cluster_janela()`, conforme o documento técnico de limiares de Julho de 2026, não a regra semanal simples da Zâmbia). O critério de ≥2 casos confirmados do mesmo documento **não está implementado** — o BES do SISMA não distingue "confirmados" no Sarampo, só casos suspeitos por faixa etária
- [x] `5_malaria.R` — Malária, percentil75/CSUM/média+2DP (idêntico à Zâmbia)
- [x] `6_meningite.R` — Meningite, taxa de ataque, com `data/Pop_Moz.xlsx` preenchido (população por distrito e ano, 2022–2026)
- [x] `7_sem_limiar_definido.R` — Raiva, Síndrome Febril, Mordeduras Animal (idêntico à Zâmbia: doenças sem limiar formal)
- [x] `00_Executar_tudo.R` — consolidação final em `results/` e `data/dados_com_alertas.xlsx` (idêntico à Zâmbia); `plot_flag <- TRUE` para gerar todos os gráficos
- [x] `01_Exemplos_distrito.R` + `scripts_distrito/1_resumo_distrito.Rmd` — relatório HTML por distrito (12 distritos de exemplo, um por província)
- [x] Tema `ggthemr("fresh")` aplicado (cores consistentes com o modelo da Zâmbia) em `1_preparacao_dados.R`
- [x] Linha horizontal vermelha do limiar esperado em todos os gráficos de `0_functions.R` (fixa para 1 caso suspeito/cluster≥5/taxa de ataque; acompanhando o limiar dinâmico nos restantes)
- [ ] `script_others/dashboard_mock-up.R` — tabelas prontas e traduzidas; bloco do mapa comentado, **pendente shapefiles administrativos de Moçambique** (nível distrito e província) em `data/Shapefiles/`
- [x] `02_Analises_provincia.R` — reaplica todas as regras de alerta (2..7) só aos distritos de **Sofala** e **Nampula**, cada uma na sua própria pasta em `results_provincias/<Provincia>/` (mesma estrutura de `results/`: PNGs, xlsx por doença, consolidação anual/mensal). As funções em `0_functions.R` ganharam o parâmetro `pasta_base` (por omissão `"results"`, sem alterar o comportamento do pipeline nacional) para permitir esta separação
- Nota: `8_anthrax_special.R` da Zâmbia não tem equivalente — Antraz não está no BES do SISMA
