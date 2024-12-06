pcontas <- read_contas("data_raw/pcontas.xlsx")
pcontas_part <- partition_contas(pcontas)
pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]

tbl <- pcontas_part[1,4][[1]][[1]]

partition_020(tbl)

# details rows
# get receitas e despesas block

tbl_groups <- partition_020_groups(tbl)

# receitas tbl

tbl_groups_receitas <- tbl_groups[tbl_groups$info == "Receitas",]$cells[[1]]

rec_analitico <- partition_020_receitas(tbl_groups_receitas)

tbl_groups[tbl_groups$info == "Receitas",] |>
  mutate(data = lapply(cells, partition_020_receitas)) |>
  View()

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
