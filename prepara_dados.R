library(dplyr)
library(readr)

# 1. Carregar base principal de preço + benchmark
pbj <- read_csv("price_benchmark_join.csv", show_col_types = FALSE)

# Opcional: puxar model_family e modality de price_snapshots sem multiplicar linhas
ps <- read_csv("price_snapshots_subset.csv", show_col_types = FALSE) %>%
  select(model_id, price_tier, model_family, modality) %>%
  distinct(model_id, price_tier, .keep_all = TRUE)

# 2. Padronizar e evitar duplicação (many-to-many)
df_limpo <- pbj %>%
  left_join(ps, by = c("model_id", "price_tier")) %>%
  mutate(
    modelo = coalesce(model_display_name, arena_model_name, model_id),
    provedor = coalesce(provider, arena_organization, "Desconhecido"),
    categoria_arena = coalesce(arena_category, "Geral"),
    data_snap = as.Date(snapshot_date)
  ) %>%
  filter(!is.na(modelo)) %>%
  # Se houver mais de um snapshot/linha por modelo e tier, prioriza o mais recente e maior rating
  arrange(desc(data_snap), desc(arena_rating)) %>%
  group_by(modelo, provedor, price_tier, categoria_arena) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    modelo,
    provedor,
    model_family,
    modality,
    price_tier,
    categoria_arena,
    data_snap,
    input_usd_per_1m,
    output_usd_per_1m,
    cached_input_usd_per_1m,
    scenario_10k_1k_usd,
    arena_rating,
    arena_rank,
    arena_vote_count,
    arena_rating_per_usd_10k_1k,
    points_per_dollar_proxy,
    benchmark_license
  )

write_rds(df_limpo, "dados_llm.rds")
message("Salvo com sucesso: ", nrow(df_limpo), " linhas em dados_llm.rds")
