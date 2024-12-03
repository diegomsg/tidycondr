pcontas <- read_contas("data_raw/pcontas.xlsx")
pcontas_part <- partition_contas(pcontas)
pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]

tbl <- pcontas_part[1,4][[1]][[1]]

# details rows
# get receitas e despesas block

tbl_groups <- partition_020_groups(tbl)

# receitas tbl

tbl_groups_receitas <- tbl_groups[tbl_groups$info == "Receitas",]$cells[[1]]

# receitas level groups

first_lvl_group_pat <- " \\([0-9]{1,3},?[0-9]*\\%\\)$"
first_lvl_group_rows_id <- tbl_groups_receitas$row[
  grepl(first_lvl_group_pat,
        tbl_groups_receitas$character)]
first_lvl_group_rows <- tibble(
  "first_lvl_partition" = unpivotr::partition_dim(
    tbl_groups_receitas$row,
    first_lvl_group_rows_id,
    bound = "upper"))

second_lvl_group_rows_id <- get_partition_corners_rows(tbl_groups_receitas, 1, 2:4)
second_lvl_group_rows <- tibble(
  "second_lvl_partition" = unpivotr::partition_dim(
    tbl_groups_receitas$row,
    second_lvl_group_rows_id,
    bound = "upper"))


# receitas resumo

bind_cols(
  first_lvl_group_rows,
  second_lvl_group_rows,
  tbl_groups_receitas
) |>
 filter(
   !if_all(
     c(first_lvl_partition, second_lvl_partition),
     is.na)) |>
  behead("up", "head") |>
  tidyr::replace_na(list(head = "prop"))

### group by first_lvl and second_lvl
### processs


rec_resum <- nested_info$cells[1][[1]] |>
  behead("up", "head") |>
  tidyr::replace_na(list(head = "prop")) |>
  partition(first_lvl_group_corners) |>
  select(grupo = character, cells) |>
  tidyr::unnest(cells) |>
  filter(!(row %in% first_lvl_group_corners$row)) |>
  partition(second_lvl_group_corners) |>
  select(subgrupo = character, cells) |>
  tidyr::unnest(cells) |>
  filter(
    !(row %in% first_lvl_group_corners$row)
    & !(row %in% second_lvl_group_corners$row)) |>
  mutate(
    value = coalesce(
      character,
      as.character(numeric))) |>
  select(-c(col:character)) |>
  pivot_wider(
    names_from = head,
    values_from = value) |>
  select(-row) |>
  janitor::clean_names() |>
  tidyr::separate_wider_regex(
    grupo,
    c(
      grupo = ".*",
      prop_grupo = first_lvl_group_pat)) |>
  mutate(
    across(c(prop_grupo, prop, valor),
    ~ parse_number(
      .x,
      locale = locale(
        decimal_mark = ",",
        grouping_mark = "."))),
    grupo_sum_row = receitas == paste0("Total de ", grupo),
    subgrupo_sum_row = receitas == paste0("Total de ", subgrupo)) |>
  filter(!grupo_sum_row) |>
  mutate(
    prop_subgrupo = if_else(
      subgrupo_sum_row,
      prop,
      NA)) |>
  tidyr::fill(prop_subgrupo, .direction = "up") |>
  filter(!subgrupo_sum_row) |>
  relocate(
    grupo, subgrupo, competencia, receitas, valor, prop, subgrupo_sum_row,
    prop_subgrupo, grupo_sum_row, prop_grupo) |>
  head(-1) |>
  select(-c(subgrupo_sum_row, grupo_sum_row)) |>
  mutate(
    mes = if_else(
      grepl("[0-9]{1,2}/[0-9]{2,4}", competencia),
      my(competencia),
      NA),
    .after = competencia)

# despesas

# return
tibble(
  "chapter" = c("Resumo movimentação", "Receitas resumo"),
  "data" = list(
    partition_020_summary(tbl),
    rec_resum))

