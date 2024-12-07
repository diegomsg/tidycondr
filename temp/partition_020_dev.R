pcontas <- read_contas("data_raw/pcontas.xlsx")
pcontas_part <- partition_contas(pcontas)
pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]

tbl <- pcontas_part[1,4][[1]][[1]]

partition_020_summary(tbl)
partition_020_groups(tbl) |>
  filter(info == "Receitas") |>
  mutate(code = glue::glue("020_{info}")) |>
  call_partition_funs(
    .cells_col = data)

# error in get_partition_corners_rows, to much cols

# return
resumo <- tibble(
  "chapter" = c("Resumo movimentação"),
  "data" = list(
    partition_020_summary(tbl)))

analitico <- bind_cols(
  "chapter" = c("Relatório analítico"),
  "data" = partition_020_groups(tbl)) |>
  filter(info == "Receitas") |>
  mutate(
    data = lapply(data, partition_020_receitas))

bind_rows(resumo, analitico) |>
  select(chapter, info, data)
