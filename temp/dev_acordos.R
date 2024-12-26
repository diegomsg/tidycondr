# dev acordos

acord <- read_contas("data_raw/acordos.xlsx")
pacord <- partition_contas(acord)
pacord <- pacord[grepl("028", pacord$code),]

pacord_part <- pacord$cells[1][[1]]

tbl <- partition_028_split_groups(pacord_part)

#-----------

cob <- tbl$cobrancas_originais[1][[1]]
parc <- tbl$parcelas_do_acordo[1][[1]]

# cobrancas
partition_028_cobrancas(cob)
tbl |>
  mutate(
    cobrancas_originais = lapply(cobrancas_originais, partition_028_cobrancas))

# parcelas
assert_tidyxl(cob)

first_lvl_group_rows_id <- get_partition_corners_rows(cob, 1:5, 6)
first_lvl_group_rows <- tibble(
  "first_lvl" = unpivotr::partition_dim(
    cob$row,
    first_lvl_group_rows_id,
    bound = "upper"))

bind_cols(
  first_lvl_group_rows,
  cob) |>
  tail(-1) |>
  behead("up", "head") |>
  behead("right-down", "acrescimo") |>
  mutate(total = last(acrescimo)) |>
  head(-2) |>
  group_by(first_lvl) |>
  mutate(subtotal = last(character)) |>
  filter(row < max(row)) |>
  ungroup() |>
  pivot_wider(
    id_cols = -c(col:date),
    names_from = head,
    values_from = character) |>
  janitor::clean_names() |>
  relocate(c(subtotal, acrescimo, total), .after = last_col()) |>
  mutate(
    numero = as.integer(numero),
    across(
      composicao:total,
      ~ parse_number(
        .x,
        locale = locale(
          decimal_mark = ",",
          grouping_mark = "."))),
    vencimento = dmy(vencimento),
    competencia = my(competencia)) |>
  select(-c(1:2)) |>
  tidyr::fill(numero:competencia, .direction = "down") |>
  mutate(
    tx_atualizacao = total / (total - acrescimo),
    across(
      composicao:subtotal,
      ~ .x * tx_atualizacao,
      .names = "{.col}_atualizado"),
    composicao_prop = composicao_atualizado / total)

# split code

# cobrancas
tidy_base$cobrancas_originais[1][[1]] |>
  rectify()

tidy_base$cobrancas_originais[1][[1]] |>
  tail(-1) |>
  behead("up", "head") |>
  behead("right-down", "acrescimo") |>
  mutate(total = last(acrescimo)) |>
  head(-2) |>
  #
  # behead("right-down", "acrescimo") |>
  # behead("left-down", "subtotal") |> View()
  # mutate(valor = coalesce(numeric, date, character)) |>
  pivot_wider(
    id_cols = row,
    names_from = head,
    values_from = character
  ) |>
  janitor::clean_names() |>
  mutate(subtotal = if_else(is.na(descricao), numero, NA)) |>
  tidyr::fill(subtotal, .direction = "up") |>
  filter(!is.na(descricao)) |>
  tidyr::fill(numero:competencia)
# change types
#make function
