pcontas <- read_contas("ex/pcontas.xlsx")
pcontas_part <- partition_contas(pcontas)
pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]

tbl <- pcontas_part[1,4][[1]][[1]]

rec_tbl <- rectify(tbl)

# details rows
# get receitass e despesas block

## head row
head_values <- list(
  c("Receitas", "Competência", NA, "Valor"),
  c("Despesas",	"Liquidação", "Documento",	"Forma de Pgto.",	NA,	"Valor"))

head_rows <- head_rows(head_values, rec_tbl, verbose = FALSE)

# partition data
partition_cells <- tbl[tbl$row %in% head_rows & tbl$col == 1L,]

nested_info <- partition(tbl, partition_cells) |>
  select(info = character, cells)

first_lvl_group_pat <- " \\([0-9]{1,3},?[0-9]*\\%\\)$"
first_lvl_group_rows <-
  tbl[tbl$col == 1
      & grepl(first_lvl_head_pat, tbl$character),]

second_lvl_group_rows_id <- get_partition_corners_rows(tbl, 1, 2:6)
second_lvl_group_rows <-
  tbl[tbl$col == 1
      & tbl$row %in% second_lvl_group_rows_id
      & !(tbl$row %in% first_lvl_group_rows$row),]

rec_resum <- nested_info$cells[1][[1]] |>
  behead("up", "head") |>
  tidyr::replace_na(list(head = "prop_subgrupo")) |>
  partition(first_lvl_group_rows) |>
  select(grupo = character, cells) |>
  tidyr::unnest(cells) |>
  filter(!(row %in% first_lvl_group_rows$row)) |>
  partition(second_lvl_group_rows) |>
  select(subgrupo = character, cells) |>
  tidyr::unnest(cells) |>
  filter(
    !(row %in% first_lvl_group_rows$row)
    & !(row %in% second_lvl_group_rows$row)) |>
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
      part_grupo = first_head_pat)) |>
  mutate(
    part_grupo = parse_number(
      part_grupo,
      locale = locale(
        decimal_mark = ",",
        grouping_mark = ".")),
    grupo_sum_row = receitas == paste0("Total de ", grupo),
    subgrupo_sum_row = receitas == paste0("Total de ", subgrupo))

# return
tibble(
  "chapter" = c("Resumo movimentação"),
  "data" = list(partition_020_summary(tbl)))

