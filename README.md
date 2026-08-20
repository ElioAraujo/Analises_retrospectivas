# Análises Retrospectivas — Limiares de Alerta para Vigilância Epidemiológica

Pipeline em R para detecção automática de sinais de alerta em doenças de
notificação obrigatória, a partir dos dados semanais do **BES (Boletim
Epidemiológico Semanal) do SISMA**, no âmbito da estratégia de Vigilância
Colaborativa do MISAU (2026–2028), em parceria com o RTSL.

O pipeline aplica, por distrito e por semana, um conjunto de regras
estatísticas — uma para cada doença ou grupo de doenças — que comparam o
número de casos observados com um limiar esperado. Quando o valor observado
ultrapassa esse limiar, é gerado um alerta.

---

## Como correr o pipeline

1. Abrir `Analises_retrospectivas.Rproj` no RStudio.
2. Correr `00_Executar_tudo.R` — este script executa, por ordem, todos os
   ficheiros da pasta `scripts/`: descarrega os dados do SISMA, prepara-os,
   aplica todas as regras de alerta e consolida os resultados em
   `results/`. No final, chama automaticamente `02_Analises_provincia.R`,
   que repete a mesma análise só para Sofala e Nampula, com resultados em
   `results_provincias/` (ver secção "Análises por província").
3. (Opcional) Correr `01_Exemplos_distrito.R` — gera um relatório HTML por
   distrito de exemplo, com o histórico de alertas de todas as doenças.

O interruptor `plot_flag` (definido em `00_Executar_tudo.R`) controla se os
gráficos são gerados (`TRUE`) ou não (`FALSE`, mais rápido, só produz as
tabelas de alerta).

---

## Estrutura de pastas

```
Analises_retrospectivas/
├── 00_Executar_tudo.R          # corre todo o pipeline; no fim chama o 02 automaticamente
├── 01_Exemplos_distrito.R      # relatório HTML por distrito (corre depois do 00, manual)
├── 02_Analises_provincia.R     # mesma análise, só para Sofala e Nampula, saída separada (corre automaticamente no fim do 00; também pode ser corrido à parte)
│
├── data/
│   ├── Data_BES.xls            # export bruto do SISMA (gerado por 000_dhis2_analytics.R)
│   ├── dados_preparados.xlsx   # dados já no formato do pipeline (uma linha por distrito/semana)
│   ├── dados_com_alertas.xlsx  # dados_preparados.xlsx + uma coluna alert_ por doença
│   └── Pop_Moz.xlsx            # população por distrito e ano (2022-2026), usada na Meningite
│
├── scripts/                    # o motor do pipeline, corrido por ordem pelo 00_Executar_tudo.R
│   ├── 000_dhis2_analytics.R   # descarrega o BES via API do SISMA
│   ├── 0_functions.R           # todas as funções de regra de alerta (ver secção abaixo)
│   ├── 1_preparacao_dados.R    # BES bruto -> formato "uma linha por distrito/semana"
│   ├── 2_um_caso_suspeito.R    # aplica a regra "1 caso suspeito"
│   ├── 3_tendencia_crescente.R # aplica as regras de aumento >50% e duplicação
│   ├── 4_cluster_suspeito.R    # aplica a regra de cluster (Sarampo)
│   ├── 5_malaria.R             # aplica as 3 regras específicas da Malária
│   ├── 6_meningite.R           # aplica a regra de taxa de ataque (Meningite)
│   └── 7_sem_limiar_definido.R # doenças sem limiar oficial ainda definido
│
├── scripts_distrito/
│   └── 1_resumo_distrito.Rmd   # modelo do relatório HTML gerado pelo 01_Exemplos_distrito.R
│
├── script_others/
│   ├── alert_defining_functions.R  # cópia comentada das funções de regra, só para consulta/estudo
│   └── dashboard_mock-up.R         # protótipo do painel semanal para a Direcção-Geral
│
├── results/                    # saída do pipeline nacional: uma pasta por doença
│   ├── <Doenca>/
│   │   ├── p_dist_<doenca>.png            # gráfico: casos observados vs. limiar, por distrito
│   │   ├── p_n_alerts_<doenca>.png        # gráfico: nº de distritos em alerta, por semana/província
│   │   └── <Doenca>_alert_by_district.xlsx # tabela: nº de alertas por distrito, por ano e por mês
│   ├── alertas_por_distrito_por_ano.xlsx  # todas as doenças juntas, uma folha por doença
│   └── alertas_por_distrito_por_mes.xlsx
│
└── results_provincias/         # saída do 02_Analises_provincia.R (mesma estrutura de results/)
    ├── Sofala/
    └── Nampula/
```

---

## Lógica geral: como uma doença passa a ter alerta

Cada doença (ou grupo de doenças) é processada em 3 passos, sempre dentro
de uma só função de `scripts/0_functions.R`:

1. **Calcular o limiar esperado** para cada combinação distrito/semana —
   pode ser um número fixo (ex: 1 caso) ou um valor que varia semana a
   semana, calculado a partir do histórico recente daquele distrito.
2. **Comparar** o valor observado nessa semana com o limiar. Se o valor
   observado ultrapassar o limiar, a coluna `alert_<doenca>` recebe `1`
   nessa linha (caso contrário, fica vazia).
3. **Exportar** os resultados: gráfico(s) em PNG (se `gen_plot = TRUE`) e
   uma tabela `.xlsx` com o número de alertas por distrito, por ano e por
   mês.

Todas as funções recebem os dados (`dat`), o nome da doença (`disease`,
usado nos títulos e nos nomes dos ficheiros) e o nome da coluna de casos
(`varname`). O parâmetro `pasta_base` (por omissão `"results"`) define
onde os ficheiros de saída são gravados — é o que permite ao
`02_Analises_provincia.R` reaproveitar exactamente as mesmas funções, mas
gravando em `results_provincias/<Provincia>/` em vez de `results/`.

Em todos os gráficos, o **limiar esperado é sempre desenhado a vermelho**
— uma linha horizontal quando o limiar é um número fixo, ou uma linha que
acompanha o valor calculado semana a semana quando o limiar é dinâmico.

---

## As funções de regra (`scripts/0_functions.R`)

### `one_suspected_case()` — 1 caso suspeito
**Usada para:** Cólera, PFA, Peste, Tétano Neonatal.
**Regra:** um único caso suspeito, numa semana, num distrito, já é motivo
de alerta (`casos >= 1`). O limiar é fixo e igual a 1 em todas as semanas.

### `unusual_inc_50p()` — aumento >50% face à tendência recente
**Usada para:** Diarreia, Disenteria, Raiva, Síndrome Febril, Mordeduras
de Animal.
**Regra:** calcula a média de casos das 3 semanas anteriores (`p3w_avg`,
desfasada 1 semana para não incluir a própria semana em análise) e
multiplica por 1,5. Se os casos da semana actual ultrapassarem esse
valor, gera alerta. O limiar é dinâmico — sobe e desce com o histórico
recente de cada distrito.

### `unusual_inc_doubling()` — duplicação de casos em 2 semanas
**Usada para:** as mesmas doenças de `unusual_inc_50p()`, como critério
alternativo.
**Regra:** compara os casos da semana actual com o dobro dos casos da
semana anterior (`casos_semana_atual > 2 × casos_semana_anterior`). Tal
como na regra anterior, o limiar é dinâmico.

### `susp_cluster()` — cluster suspeito (regra semanal simples)
**Regra:** conta os casos suspeitos numa única semana e compara com um
número fixo (`n_cluster`). Está disponível no ficheiro mas não é usada
por nenhuma doença de momento — foi substituída pela função seguinte
para o Sarampo.

### `susp_cluster_janela()` — cluster suspeito numa janela de dias
**Usada para:** Sarampo (`n_cluster = 5`, `dias_janela = 28`).
**Regra:** em vez de olhar só para uma semana, soma os casos suspeitos
das últimas 4 semanas (28 dias, com `zoo::rollapply`) e gera alerta
quando essa soma acumulada atinge o limiar. É a versão mais correcta da
regra de cluster para doenças em que o critério oficial é definido por
período (dias), e não por semana isolada.
> Nota: o critério adicional "ou ≥2 casos confirmados" (do documento
> técnico de limiares) não está implementado — o BES do SISMA não regista
> uma contagem de casos *confirmados* separada da de casos suspeitos.

### `above_percentile()` — acima do percentil 75
**Usada para:** Malária.
**Regra:** para cada distrito/semana, calcula o percentil 75 dos casos
das 5 semanas anteriores e compara com o valor observado. Se o valor
observado for maior, gera alerta. Pensada para doenças endémicas em que
só interessa sinalizar picos claramente fora do habitual.

### `csum()` — soma cumulativa de médias móveis
**Usada para:** Malária.
**Regra:** calcula médias móveis sucessivas dos casos (uma segunda
suavização sobre a média de 3 semanas) para detectar tendências de subida
sustentadas, menos sensível a picos isolados de uma única semana do que
`unusual_inc_50p()`.

### `mean_2sd()` — média + 2 desvios-padrão
**Usada para:** Malária.
**Regra:** calcula a média e o desvio-padrão dos casos das semanas
anteriores e define o limiar como `média + 2×desvio-padrão` — um critério
estatístico clássico de detecção de valores atípicos ("outliers").

### `attack_rate()` — taxa de ataque
**Usada para:** Meningite (`ar_cutoff = 3` por 100.000 habitantes).
**Regra:** converte os casos em taxa por 100.000 habitantes, usando a
população do distrito **no mesmo ano** da semana em análise (ficheiro
`data/Pop_Moz.xlsx`, colunas `clean_name`, `ano`, `populacao`). Anos
anteriores a 2022 ou posteriores a 2026 usam a população do ano mais
próximo disponível, por não haver projecção populacional para esses anos.
Gera alerta quando a taxa ultrapassa o limiar.

### Funções auxiliares
- `by_district_by_year()` / `by_district_by_month()` — resumem o número
  de alertas por distrito, agregados por ano ou por mês; usadas por todas
  as funções de regra para montar as tabelas `.xlsx` de saída.
- `summary_label_province()` / `summary_label_country()` — calculam o
  texto-resumo (total e média de alertas por semana) mostrado nos
  gráficos de tipo `p_n_alerts`.
- `plot_alert_summary()` — monta o gráfico de barras "número de distritos
  em alerta por semana", com um painel por província (ou por país).

---

## Análises por província

`02_Analises_provincia.R` corre exactamente as mesmas regras descritas
acima, mas com os dados filtrados a um só conjunto de distritos de cada
vez — actualmente Sofala e Nampula. Corre automaticamente no fim de
`00_Executar_tudo.R` (também pode ser corrido sozinho, a qualquer altura,
desde que `data/dados_preparados.xlsx` já exista). Os resultados de cada
província ficam isolados em `results_provincias/<Provincia>/`, sem
misturar nem sobrepor os ficheiros do resultado nacional em `results/`.
Para incluir outra província, basta acrescentar o nome (tal como aparece
na coluna `province` dos dados) ao vector `provincias` no topo do script.

---

## Limitações conhecidas

- **Mapa do painel da Direcção-Geral** (`script_others/dashboard_mock-up.R`):
  as tabelas de alerta estão prontas, mas o bloco do mapa está comentado —
  falta o shapefile administrativo de Moçambique (distrito e província)
  em `data/Shapefiles/`.
- **Sarampo — casos confirmados:** o BES do SISMA não distingue casos
  confirmados dos suspeitos, pelo que só o critério de casos suspeitos em
  janela de 28 dias está implementado.
