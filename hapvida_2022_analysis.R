###### Prelimiinares

setwd("C:/Users/ricar/OneDrive/Documentos/Insper/MPA/Dissertacao/Eficiencia_Pos_Fusao/2.Dados")

library(basedosdados)
library(dplyr)
library(stringr)
library(ggplot2)
library(writexl)
library(glue)
library(tidyr)

set_billing_id("analisemacro-451912")

# Tabela completa de aquisições Hapvida (mantida para referência)
aquisicao_completa <- tibble::tribble(
  ~id_estabelecimento_cnes, ~ano_aquisicao,
  "9069747", 2011,
  "0003832", 2011,
  "7371985", 2012,
  "9046720", 2012,
  "2399989", 2012,
  "3007898", 2014,
  "2018152", 2014,
  "3556239", 2017,
  "3101126", 2019,
  "9966900", 2019,
  "4193423", 2019,
  "9490604", 2019,
  "0653101", 2019,
  "2426099", 2019,
  "2518406", 2019,
  "9357661", 2019,
  "7975023", 2019,
  "2615754", 2019,
  "2825309", 2019,
  "3517918", 2020,
  "3000362", 2020,
  "0027847", 2021,
  "0026727", 2021,
  "9978089", 2021,
  "2081113", 2021,
  "0027871", 2021,
  "3309959", 2022,
  "2084368", 2022,
  "9554157", 2022,
  "2079887", 2022,
  "2080001", 2022,
  "9769641", 2022,
  "7044666", 2022,
  "2078066", 2022,
  "9738754", 2022,
  "2082101", 2022,
  "2758083", 2022,
  "2587343", 2022,
  "3000907", 2022,
  "3119289", 2022,
  "0507237", 2022,
  "3314014", 2022,
  "3016501", 2022,
  "7335733", 2023,
  "2519887", 2023
) %>%
  mutate(id_estabelecimento_cnes = as.character(id_estabelecimento_cnes))

# ANÁLISE RESTRITA ÀS AQUISIÇÕES DE 2022
aquisicao <- aquisicao_completa %>%
  filter(ano_aquisicao == 2022)

# =========================
# 0) Criar CNES never-treated (controles)
# =========================

# 0.1 CNES tratados: apenas as aquisições de 2022
cnes_tratados <- unique(aquisicao$id_estabelecimento_cnes)

# 0.2 Puxa universo de hospitais no CNES para obter IDs de controle
query_ids_controle <- "
SELECT DISTINCT id_estabelecimento_cnes
FROM `basedosdados.br_ms_cnes.estabelecimento`
WHERE indicador_atencao_hospitalar = 1
  AND ano BETWEEN 2015 AND 2025
  AND tipo_natureza_administrativa IN ('7')
"

ids_universo <- read_sql(query_ids_controle, billing_project_id = get_billing_id()) |>
  mutate(id_estabelecimento_cnes = as.character(id_estabelecimento_cnes))

# 0.3 Never treated = universo menos TODOS os hospitais Hapvida (qualquer coorte)
# (exclui mesmo os de outras coortes para evitar contaminação no grupo controle)
cnes_todos_hapvida <- unique(aquisicao_completa$id_estabelecimento_cnes)
cnes_controle <- setdiff(ids_universo$id_estabelecimento_cnes, cnes_todos_hapvida)

# 0.4 Lista final: tratados (2022) + controles (never-treated)
cnes <- c(cnes_tratados, cnes_controle)

# 0.5 SQL IN (...)
cnes_sql <- paste(sprintf("'%s'", cnes), collapse = ", ")




# Busca a tabela "Profissionais"

query <- glue("
SELECT
  dados.ano           AS ano,
  dados.mes           AS mes,
  dados.id_estabelecimento_cnes AS id_estabelecimento_cnes,
  dados.cbo_2002_original       AS cbo_2002_original
FROM `basedosdados.br_ms_cnes.profissional` AS dados
WHERE dados.id_estabelecimento_cnes IN ({cnes_sql})
")

df <- read_sql(query, billing_project_id = get_billing_id())

##### Filtrando o número de médicos

medicos_mensal <- df %>%
  mutate(cbo_str = as.character(cbo_2002_original)) %>%
  filter(str_sub(cbo_str, 1, 4) %in% c("2251","2252","2253")) %>%
  group_by(id_estabelecimento_cnes, ano, mes) %>%
  summarise(num_medicos = n(), .groups = "drop") %>%
  arrange(id_estabelecimento_cnes, ano, mes)

medicos_mensal


##### Variável N° de médicos exportada para o excel

write_xlsx(
  list(
    "medicos_mensal_todos" = medicos_mensal,
    "cnes_3309959"         = g_3309959
  ),
  path = "medicos_cnes.xlsx"
)

##### Busca a tabela Serviço Especializado

query_servicos <- glue("
WITH
dicionario_tipo_servico_especializado AS (
  SELECT
    chave AS chave_tipo_servico_especializado,
    valor AS descricao_tipo_servico_especializado
  FROM `basedosdados.br_ms_cnes.dicionario`
  WHERE nome_coluna = 'tipo_servico_especializado'
    AND id_tabela   = 'servico_especializado'
)
SELECT
  dados.ano  AS ano,
  dados.mes  AS mes,
  dados.id_estabelecimento_cnes AS id_estabelecimento_cnes,
  descr.descricao_tipo_servico_especializado AS tipo_servico_especializado
FROM `basedosdados.br_ms_cnes.servico_especializado` AS dados
LEFT JOIN dicionario_tipo_servico_especializado AS descr
  ON dados.tipo_servico_especializado = descr.chave_tipo_servico_especializado
WHERE dados.id_estabelecimento_cnes IN ({cnes_sql})
ORDER BY id_estabelecimento_cnes, ano, mes
")


df_servicos <- read_sql(query_servicos, billing_project_id = get_billing_id())

servicos_mensal <- df_servicos |>
  group_by(id_estabelecimento_cnes, ano, mes) |>
  summarise(n_servicos = n_distinct(tipo_servico_especializado), .groups = "drop") |>
  arrange(id_estabelecimento_cnes, ano, mes)


g_3309959 <- servicos_mensal |>
  filter(id_estabelecimento_cnes == "3309959") |>
  mutate(
    ano = as.integer(ano),
    mes = as.integer(mes),
    data = as.Date(sprintf('%04d-%02d-01', ano, mes))
  ) |>
  arrange(data)

write_xlsx(
  list(
    "cnes_3309959"         = g_3309959,
    "servicos_mensal"      = servicos_mensal
  ),
  path = "servicos_cnes.xlsx"
)

##### Gera a variável de percentual (%) de leitos de UTI

query_leitos <- glue("
WITH
dicionario_tipo_especialidade_leito AS (
  SELECT
    chave AS chave_tipo_especialidade_leito,
    valor AS descricao_tipo_especialidade_leito
  FROM `basedosdados.br_ms_cnes.dicionario`
  WHERE nome_coluna = 'tipo_especialidade_leito'
    AND id_tabela   = 'leito'
),
dicionario_tipo_leito AS (
  SELECT
    chave AS chave_tipo_leito,
    valor AS descricao_tipo_leito
  FROM `basedosdados.br_ms_cnes.dicionario`
  WHERE nome_coluna = 'tipo_leito'
    AND id_tabela   = 'leito'
)
SELECT
  dados.ano  AS ano,
  dados.mes  AS mes,
  dados.id_estabelecimento_cnes AS id_estabelecimento_cnes,
  descr1.descricao_tipo_especialidade_leito AS tipo_especialidade_leito,
  descr2.descricao_tipo_leito               AS tipo_leito,
  dados.quantidade_total AS quantidade_total
FROM `basedosdados.br_ms_cnes.leito` AS dados
LEFT JOIN dicionario_tipo_especialidade_leito AS descr1
  ON dados.tipo_especialidade_leito = descr1.chave_tipo_especialidade_leito
LEFT JOIN dicionario_tipo_leito AS descr2
  ON dados.tipo_leito = descr2.chave_tipo_leito
WHERE dados.id_estabelecimento_cnes IN ({cnes_sql})
ORDER BY id_estabelecimento_cnes, ano, mes
")

leitos_raw <- read_sql(query_leitos, billing_project_id = get_billing_id())


unique(leitos_raw$tipo_especialidade_leito)


pct_uti_mensal <- leitos_raw %>%
  mutate(
    id_estabelecimento_cnes = as.character(id_estabelecimento_cnes),
    ano  = as.integer(ano),
    mes  = as.integer(mes),
    quantidade_total = as.numeric(quantidade_total),
    espec = str_squish(str_to_lower(coalesce(tipo_especialidade_leito, "")))
  ) %>%
  mutate(
    flag_uti = str_detect(espec, "(^|\\W)uti\\b") |
      str_detect(espec, "\\buco\\b") |
      str_detect(espec, "coronarian") |
      str_detect(espec, "queimad"),
    flag_intermed = str_detect(espec, "unidade de cuidados intermediarios")
  ) %>%
  mutate(is_uti = flag_uti & !flag_intermed) %>%
  group_by(id_estabelecimento_cnes, ano, mes) %>%
  summarise(
    leitos_uti   = sum(ifelse(is_uti, quantidade_total, 0), na.rm = TRUE),
    leitos_total = sum(quantidade_total, na.rm = TRUE),
    pct_uti      = if_else(leitos_total > 0, 100 * leitos_uti / leitos_total, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(id_estabelecimento_cnes, ano, mes)

pct_uti_mensal


write_xlsx(
  list(
    "percentual_uti"      = pct_uti_mensal
  ),
  path = "pct_uti.xlsx"
)

##### Complexidade média dos leitos (índice 1–5)

complexidade_mensal <- leitos_raw %>%
  mutate(
    id_estabelecimento_cnes = as.character(id_estabelecimento_cnes),
    ano  = as.integer(ano),
    mes  = as.integer(mes),
    qtd  = as.numeric(quantidade_total),
    espec = str_squish(str_to_lower(coalesce(tipo_especialidade_leito, "")))
  ) %>%
  mutate(
    categoria = case_when(
      str_detect(espec, "(^|\\W)uti\\b") |
        str_detect(espec, "\\buco\\b") |
        str_detect(espec, "coronarian") |
        str_detect(espec, "queimad") ~ "uti",

      str_detect(espec, "unidade de cuidados intermediarios") |
        str_detect(espec, "unidade intermediaria") ~ "intermediario",

      str_detect(espec, "pediatr") |
        str_detect(espec, "neonatal") |
        str_detect(espec, "neonatolog") |
        str_detect(espec, "obstetricia clinica") ~ "pedi_obst",

      str_detect(espec, "obstetricia cirurgica") ~ "cirurgica",

      str_detect(espec, "cirurgia") |
        str_detect(espec, "cirurgica") |
        str_detect(espec, "ortopediatraumatologia") |
        str_detect(espec, "ortopedia") |
        str_detect(espec, "traumato") |
        str_detect(espec, "toracica") |
        str_detect(espec, "buco maxilo") |
        str_detect(espec, "plastica") ~ "cirurgica",

      espec != "" ~ "clinica",

      TRUE ~ "outros"
    ),
    peso_complex = case_when(
      categoria == "uti"          ~ 5,
      categoria == "cirurgica"    ~ 4,
      categoria == "clinica"      ~ 3,
      categoria == "pedi_obst"    ~ 2,
      categoria %in% c("intermediario", "outros") ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(id_estabelecimento_cnes, ano, mes) %>%
  summarise(
    leitos_total = sum(qtd, na.rm = TRUE),
    score_total  = sum(peso_complex * qtd, na.rm = TRUE),
    complexidade_media = if_else(leitos_total > 0,
                                 score_total / leitos_total,
                                 NA_real_),
    .groups = "drop"
  ) %>%
  arrange(id_estabelecimento_cnes, ano, mes)

complexidade_mensal

write_xlsx(
  list(
    "complexidade_leitos"  = complexidade_mensal
  ),
  path = "complexidade.xlsx"
)

mean(complexidade_mensal$complexidade_media)



##### Criando a variável de Densidade de equipamentos médicos avançados por leito

query_equip_detalhe <- glue("
WITH
dicionario_id_equipamento AS (
    SELECT
        chave AS chave_id_equipamento,
        valor AS descricao_id_equipamento
    FROM `basedosdados.br_ms_cnes.dicionario`
    WHERE
        nome_coluna = 'id_equipamento'
        AND id_tabela = 'equipamento'
),
dicionario_tipo_equipamento AS (
    SELECT
        chave AS chave_tipo_equipamento,
        valor AS descricao_tipo_equipamento
    FROM `basedosdados.br_ms_cnes.dicionario`
    WHERE
        nome_coluna = 'tipo_equipamento'
        AND id_tabela = 'equipamento'
)
SELECT
    dados.ano AS ano,
    dados.mes AS mes,
    dados.id_estabelecimento_cnes AS id_estabelecimento_cnes,
    descr_id.descricao_id_equipamento   AS id_equipamento,
    descr_tipo.descricao_tipo_equipamento AS tipo_equipamento,
    dados.quantidade_equipamentos AS quantidade_equipamentos
FROM `basedosdados.br_ms_cnes.equipamento` AS dados
LEFT JOIN dicionario_id_equipamento AS descr_id
    ON dados.id_equipamento = descr_id.chave_id_equipamento
LEFT JOIN dicionario_tipo_equipamento AS descr_tipo
    ON dados.tipo_equipamento = descr_tipo.chave_tipo_equipamento
WHERE dados.id_estabelecimento_cnes IN ({cnes_sql})
")

equip_raw_detalhe <- read_sql(query_equip_detalhe, billing_project_id = get_billing_id())

unique(equip_raw_detalhe$id_equipamento)

equip_avancado_mensal <- equip_raw_detalhe %>%
  mutate(
    id_estabelecimento_cnes = as.character(id_estabelecimento_cnes),
    ano  = as.integer(ano),
    mes  = as.integer(mes),
    qtd  = as.numeric(quantidade_equipamentos),
    id_equip_clean = str_squish(str_to_lower(coalesce(id_equipamento, "")))
  ) %>%
  mutate(
    eh_avancado = id_equip_clean %in% c(
      "pet/ct",
      "ressonancia magnetica",
      "tomografo computadorizado"
    )
  ) %>%
  group_by(id_estabelecimento_cnes, ano, mes) %>%
  summarise(
    n_equip_avancados = sum(ifelse(eh_avancado, qtd, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(id_estabelecimento_cnes, ano, mes)

equip_avancado_mensal

densidade_mensal <- pct_uti_mensal %>%
  select(id_estabelecimento_cnes, ano, mes, leitos_total) %>%
  left_join(
    equip_avancado_mensal,
    by = c("id_estabelecimento_cnes", "ano", "mes")
  ) %>%
  mutate(
    n_equip_avancados = replace_na(n_equip_avancados, 0),
    dens_equip = if_else(
      leitos_total > 0,
      n_equip_avancados / leitos_total,
      NA_real_
    )
  ) %>%
  arrange(id_estabelecimento_cnes, ano, mes)

densidade_mensal


# =============================================================
# NOVAS VARIÁVEIS — Event Study
# =============================================================

# -------------------------------------------------------------
# A) Profissionais completo
# -------------------------------------------------------------

query_prof_completo <- glue("
SELECT
  dados.ano                        AS ano,
  dados.mes                        AS mes,
  dados.id_estabelecimento_cnes    AS id_estabelecimento_cnes,
  COUNT(*)                                                           AS n_prof_total,
  SUM(CAST(dados.carga_horaria_hospitalar AS FLOAT64))               AS ch_total,
  COUNTIF(dados.indicador_vinculo_contratado_sus = 1)                AS n_prof_sus
FROM `basedosdados.br_ms_cnes.profissional` AS dados
WHERE dados.id_estabelecimento_cnes IN ({cnes_sql})
GROUP BY dados.ano, dados.mes, dados.id_estabelecimento_cnes
ORDER BY id_estabelecimento_cnes, ano, mes
")

prof_completo_mensal <- read_sql(query_prof_completo, billing_project_id = get_billing_id()) %>%
  mutate(
    id_estabelecimento_cnes = as.character(id_estabelecimento_cnes),
    ano           = as.integer(ano),
    mes           = as.integer(mes),
    n_prof_total  = as.integer(n_prof_total),
    n_prof_sus    = as.integer(n_prof_sus),
    prop_prof_sus = if_else(n_prof_total > 0,
                            n_prof_sus / n_prof_total,
                            NA_real_)
  )

# -------------------------------------------------------------
# B) Leitos SUS
# -------------------------------------------------------------

query_leitos_sus <- glue("
SELECT
  dados.ano                        AS ano,
  dados.mes                        AS mes,
  dados.id_estabelecimento_cnes    AS id_estabelecimento_cnes,
  SUM(CAST(dados.quantidade_sus   AS FLOAT64)) AS leitos_sus,
  SUM(CAST(dados.quantidade_total AS FLOAT64)) AS leitos_total_check
FROM `basedosdados.br_ms_cnes.leito` AS dados
WHERE dados.id_estabelecimento_cnes IN ({cnes_sql})
GROUP BY dados.ano, dados.mes, dados.id_estabelecimento_cnes
ORDER BY id_estabelecimento_cnes, ano, mes
")

leitos_sus_mensal <- read_sql(query_leitos_sus, billing_project_id = get_billing_id()) %>%
  mutate(
    id_estabelecimento_cnes = as.character(id_estabelecimento_cnes),
    ano  = as.integer(ano),
    mes  = as.integer(mes),
    prop_leitos_sus = if_else(
      leitos_total_check > 0,
      leitos_sus / leitos_total_check,
      NA_real_
    )
  )

# -------------------------------------------------------------
# C) Salas cirúrgicas
# -------------------------------------------------------------

query_salas <- glue("
SELECT
  dados.ano                     AS ano,
  dados.mes                     AS mes,
  dados.id_estabelecimento_cnes AS id_estabelecimento_cnes,

  COALESCE(CAST(dados.quantidade_sala_cirurgia_centro_cirurgico              AS INT64), 0) +
  COALESCE(CAST(dados.quantidade_sala_cirurgia_ambulatorial_centro_cirurgico AS INT64), 0) +
  COALESCE(CAST(dados.quantidade_sala_cirurgia_centro_obstetrico             AS INT64), 0)
    AS salas_cirurgicas_principais,

  COALESCE(CAST(dados.quantidade_sala_cirurgia_centro_cirurgico              AS INT64), 0) +
  COALESCE(CAST(dados.quantidade_sala_cirurgia_ambulatorial_centro_cirurgico AS INT64), 0) +
  COALESCE(CAST(dados.quantidade_sala_cirurgia_centro_obstetrico             AS INT64), 0) +
  COALESCE(CAST(dados.quantidade_sala_cirurgia_ambulatorial                  AS INT64), 0) +
  COALESCE(CAST(dados.quantidade_sala_pequena_cirurgia_urgencia              AS INT64), 0) +
  COALESCE(CAST(dados.quantidade_sala_pequena_cirurgia_ambulatorial          AS INT64), 0)
    AS salas_cirurgicas_total

FROM `basedosdados.br_ms_cnes.estabelecimento` AS dados
WHERE dados.id_estabelecimento_cnes IN ({cnes_sql})
  AND dados.indicador_atencao_hospitalar = 1
  AND dados.ano BETWEEN 2010 AND 2025
ORDER BY id_estabelecimento_cnes, ano, mes
")

salas_mensal <- read_sql(query_salas, billing_project_id = get_billing_id()) %>%
  mutate(
    id_estabelecimento_cnes      = as.character(id_estabelecimento_cnes),
    ano                          = as.integer(ano),
    mes                          = as.integer(mes),
    salas_cirurgicas_principais  = as.integer(salas_cirurgicas_principais),
    salas_cirurgicas_total       = as.integer(salas_cirurgicas_total)
  )


##### Montando a base completa

# ==========================================================
# 1) Painel mensal BALANCEADO (ids x meses) + merges
# ==========================================================

ANO_INI <- 2010
ANO_FIM <- 2025

grid_mensal <- tidyr::expand_grid(
  id_estabelecimento_cnes = unique(cnes),
  ano = ANO_INI:ANO_FIM,
  mes = 1:12
) %>%
  mutate(
    id_estabelecimento_cnes = as.character(id_estabelecimento_cnes),
    ano  = as.integer(ano),
    mes  = as.integer(mes),
    data = as.Date(sprintf("%04d-%02d-01", ano, mes))
  )

densidade_clean <- densidade_mensal %>%
  select(id_estabelecimento_cnes, ano, mes, dens_equip, n_equip_avancados)

painel_mensal <- grid_mensal %>%
  left_join(medicos_mensal, by = c("id_estabelecimento_cnes","ano","mes")) %>%
  left_join(
    prof_completo_mensal %>%
      select(id_estabelecimento_cnes, ano, mes, n_prof_total, ch_total, prop_prof_sus),
    by = c("id_estabelecimento_cnes","ano","mes")
  ) %>%
  left_join(servicos_mensal,  by = c("id_estabelecimento_cnes","ano","mes")) %>%
  left_join(pct_uti_mensal,   by = c("id_estabelecimento_cnes","ano","mes")) %>%
  left_join(
    complexidade_mensal %>% select(id_estabelecimento_cnes, ano, mes, complexidade_media),
    by = c("id_estabelecimento_cnes","ano","mes")
  ) %>%
  left_join(densidade_clean,  by = c("id_estabelecimento_cnes","ano","mes")) %>%
  left_join(salas_mensal,     by = c("id_estabelecimento_cnes","ano","mes")) %>%
  mutate(
    num_medicos                 = replace_na(num_medicos, 0),
    n_prof_total                = replace_na(n_prof_total, 0),
    n_servicos                  = replace_na(n_servicos, 0),
    n_equip_avancados           = replace_na(n_equip_avancados, 0),
    salas_cirurgicas_principais = replace_na(salas_cirurgicas_principais, 0),
    salas_cirurgicas_total      = replace_na(salas_cirurgicas_total, 0),
    razao_prof_leito = if_else(leitos_total > 0, n_prof_total / leitos_total, NA_real_),
    carga_hor_leito  = if_else(leitos_total > 0, ch_total     / leitos_total, NA_real_)
  ) %>%
  arrange(id_estabelecimento_cnes, ano, mes)

# ==========================================================
# 2) Colar tratamento/coorte: never-treated = g=0
#    event_time = 0  →  primeiro ano PÓS-aquisição (g+1)
# ==========================================================

painel_mensal <- painel_mensal %>%
  left_join(aquisicao, by = "id_estabelecimento_cnes") %>%
  mutate(
    id_estabelecimento_cnes = as.character(id_estabelecimento_cnes),
    ano           = as.integer(ano),
    mes           = as.integer(mes),
    ano_aquisicao = as.integer(ano_aquisicao),
    tratado       = if_else(!is.na(ano_aquisicao), 1L, 0L),
    g             = if_else(tratado == 1L, ano_aquisicao, 0L),
    # post começa 1 ano APÓS a aquisição
    post          = if_else(tratado == 1L & ano >= g + 1L, 1L, 0L),
    # event_time = 0 é o primeiro ano pós-aquisição (g+1)
    event_time    = if_else(tratado == 1L, ano - (g + 1L), NA_integer_),
    event_time_trim = if_else(
      tratado == 1L,
      pmax(pmin(event_time, 5), -5),
      NA_integer_
    )
  )

cat("\n--- Checagens ---\n")
cat("IDs totais:",        n_distinct(painel_mensal$id_estabelecimento_cnes), "\n")
cat("IDs tratados:",      n_distinct(painel_mensal$id_estabelecimento_cnes[painel_mensal$tratado == 1]), "\n")
cat("IDs never-treated:", n_distinct(painel_mensal$id_estabelecimento_cnes[painel_mensal$g == 0]), "\n")

# ==========================================================
# 3) Colapsar anual
# ==========================================================

painel_anual <- painel_mensal %>%
  group_by(id_estabelecimento_cnes, ano, tratado, ano_aquisicao, g) %>%
  summarise(
    num_medicos                 = mean(num_medicos,                 na.rm = TRUE),
    n_prof_total                = mean(n_prof_total,                na.rm = TRUE),
    ch_total                    = mean(ch_total,                    na.rm = TRUE),
    prop_prof_sus               = mean(prop_prof_sus,               na.rm = TRUE),
    razao_prof_leito            = mean(razao_prof_leito,            na.rm = TRUE),
    carga_hor_leito             = mean(carga_hor_leito,             na.rm = TRUE),
    n_servicos                  = mean(n_servicos,                  na.rm = TRUE),
    leitos_uti                  = mean(leitos_uti,                  na.rm = TRUE),
    leitos_total                = mean(leitos_total,                na.rm = TRUE),
    pct_uti                     = mean(pct_uti,                     na.rm = TRUE),
    complexidade_media          = mean(complexidade_media,          na.rm = TRUE),
    dens_equip                  = mean(dens_equip,                  na.rm = TRUE),
    n_equip_avancados           = mean(n_equip_avancados,           na.rm = TRUE),
    salas_cirurgicas_principais = mean(salas_cirurgicas_principais, na.rm = TRUE),
    salas_cirurgicas_total      = mean(salas_cirurgicas_total,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # post começa 1 ano APÓS a aquisição
    post            = if_else(tratado == 1L & ano >= g + 1L, 1L, 0L),
    # event_time = 0 é o primeiro ano pós-aquisição (g+1)
    event_time      = if_else(tratado == 1L, ano - (g + 1L), NA_integer_),
    event_time_trim = if_else(tratado == 1L, pmax(pmin(event_time, 5), -5), NA_integer_)
  ) %>%
  arrange(id_estabelecimento_cnes, ano)

# ==========================================================
# 4) Salvar bases
# ==========================================================

saveRDS(painel_mensal, "painel_mensal_hapvida_2022_nevertreated_2010_2025.rds")
saveRDS(painel_anual,  "painel_anual_hapvida_2022_nevertreated_2010_2025.rds")

write_xlsx(
  list(
    "painel_mensal" = painel_mensal,
    "painel_anual"  = painel_anual
  ),
  path = "painel_hapvida_2022_nevertreated.xlsx"
)

# ==========================================================
# 5) Event Study — uma estimativa por variável
# ==========================================================
# Janela: -5 a +2  (dados chegam até 2025)
# et =  0 → 2023 (primeiro ano pós-aquisição)
# et = -1 → 2022 (ano da aquisição — período de referência)
# et = -2 → 2021,  et = -3 → 2020, ...
# et =  1 → 2024,  et =  2 → 2025
# ==========================================================

library(fixest)
library(ggplot2)

# Cria et para TODAS as unidades (controles incluídos).
# Para controles tratado=0, então i(et_trim, tratado) = 0 sempre —
# eles apenas identificam os efeitos fixos de ano e unidade.
painel_es <- painel_anual %>%
  mutate(
    et      = ano - 2023L,
    et_trim = pmax(pmin(et, 2L), -5L)   # janela: -5 (≤2018) até +2 (2025)
  )

outcomes <- list(
  num_medicos                 = "Número de Médicos",
  n_prof_total                = "Total de Profissionais",
  ch_total                    = "Carga Horária Hospitalar Total",
  prop_prof_sus               = "Proporção Profissionais SUS",
  razao_prof_leito            = "Razão Profissionais / Leito",
  carga_hor_leito             = "Carga Horária por Leito",
  n_servicos                  = "Número de Serviços Especializados",
  leitos_total                = "Total de Leitos",
  leitos_uti                  = "Leitos de UTI",
  pct_uti                     = "% Leitos UTI",
  complexidade_media          = "Complexidade Média dos Leitos",
  dens_equip                  = "Densidade de Equipamentos Avançados",
  n_equip_avancados           = "Equipamentos Avançados (n)",
  salas_cirurgicas_principais = "Salas Cirúrgicas Principais",
  salas_cirurgicas_total      = "Salas Cirúrgicas Total"
)

# Estima event study para cada outcome (ref = -1 = ano da aquisição = 2022)
modelos_es <- lapply(names(outcomes), function(y) {
  f <- as.formula(
    paste0(y, " ~ i(et_trim, tratado, ref = -1) | id_estabelecimento_cnes + ano")
  )
  feols(f, data = painel_es, cluster = ~id_estabelecimento_cnes)
})
names(modelos_es) <- names(outcomes)

# ----------------------------------------------------------
# Função auxiliar: extrai coeficientes e monta ggplot
# ----------------------------------------------------------
plot_es <- function(modelo, titulo, ref = -1L) {
  ct <- coeftable(modelo, keep = "et_trim") |>
    as.data.frame() |>
    tibble::rownames_to_column("termo") |>
    mutate(
      et       = as.integer(str_extract(termo, "-?\\d+")),
      ci_low   = Estimate - 1.96 * `Std. Error`,
      ci_high  = Estimate + 1.96 * `Std. Error`
    )

  # Adiciona o período de referência (coef = 0 por construção)
  ref_row <- tibble(
    termo = NA_character_, et = ref,
    Estimate = 0, `Std. Error` = 0,
    `t value` = NA_real_, `Pr(>|t|)` = NA_real_,
    ci_low = 0, ci_high = 0
  )

  dados_plot <- bind_rows(ct, ref_row) |> arrange(et)

  ggplot(dados_plot, aes(x = et, y = Estimate)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
                fill = "steelblue", alpha = 0.2) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_point(color = "steelblue", size = 2.5) +
    geom_vline(xintercept = -0.5, linetype = "dashed",
               color = "firebrick", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted",
               color = "gray40", linewidth = 0.7) +
    scale_x_continuous(
      breaks = seq(min(dados_plot$et), max(dados_plot$et), 1),
      labels = function(x) ifelse(x == ref, paste0(x, "\n(ref)"), x)
    ) +
    labs(
      title    = titulo,
      subtitle = "Estimador TWFE — SE clusterizado por hospital",
      x        = "Anos em relação à aquisição  (0 = 2023 | −1 = 2022, ref. | +1 = 2024 | +2 = 2025)",
      y        = "Coeficiente estimado (vs. ano da aquisição)",
      caption  = "Faixa azul: IC 95%  |  Linha vermelha: início do tratamento"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "gray40", size = 10),
      panel.grid.minor = element_blank()
    )
}

# ----------------------------------------------------------
# 5a) Exibe no painel do RStudio (um por vez — use Next/Prev)
# ----------------------------------------------------------
plots_es <- lapply(names(outcomes), function(nm) {
  plot_es(modelos_es[[nm]], titulo = paste("Event Study —", outcomes[[nm]]))
})
names(plots_es) <- names(outcomes)

# Exibe todos sequencialmente no painel de plots
for (p in plots_es) print(p)

# ----------------------------------------------------------
# 5b) Salva PNGs individuais em event_studies/
# ----------------------------------------------------------
dir.create("event_studies", showWarnings = FALSE)

for (nm in names(outcomes)) {
  ggsave(
    filename = file.path("event_studies", paste0("es_", nm, ".png")),
    plot     = plots_es[[nm]],
    width    = 10, height = 5.5, dpi = 150
  )
}
message("PNGs salvos em: event_studies/")

# ----------------------------------------------------------
# 5c) PDF com todos os gráficos (um por página)
# ----------------------------------------------------------
pdf("event_study_hapvida_2022.pdf", width = 10, height = 5.5)
for (p in plots_es) print(p)
dev.off()
message("PDF salvo: event_study_hapvida_2022.pdf")

# ----------------------------------------------------------
# 5d) Tabela de coeficientes exportada para Excel
# ----------------------------------------------------------
coefs_es <- lapply(names(modelos_es), function(nm) {
  coeftable(modelos_es[[nm]]) |>
    as.data.frame() |>
    tibble::rownames_to_column("termo") |>
    mutate(
      variavel = nm,
      et       = as.integer(str_extract(termo, "-?\\d+"))
    )
}) |>
  bind_rows() |>
  select(variavel, et, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)

write_xlsx(
  list("event_study_coefs" = coefs_es),
  path = "event_study_coeficientes.xlsx"
)
message("Coeficientes exportados: event_study_coeficientes.xlsx")
