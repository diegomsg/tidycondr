pcontas <- read_contas("data_raw/pcontas.xlsx")
pcontas_part <- partition_contas(pcontas)
pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]

tbl <- pcontas_part[1,4][[1]][[1]]

rec_tbl <- rectify(tbl)

# main corners rows
summary_rows <- get_partition_corners_rows(
  tbl, filled_cols = 1:2, empty_cols = 3:6)

# summary tbl
summ_tbl <- tbl |>
  filter(
    row %in% summary_rows) |>
  rectify() |>
  na.omit() |>
  select(2:3) |>
  set_names(c("info_txt", "valor")) |>
  mutate(
    info = if_else(
      startsWith(info_txt, "S"),
      "Saldo",
      "Mov. Líquido"),
    date = dmy(info_txt, quiet = TRUE),
    valor = parse_number(
      valor,
      locale = locale(
        decimal_mark = ",",
        grouping_mark = "."
      ))) |>
  relocate(info_txt, info, date, valor)

# details rows
# get receitass e despesas block

## head row
head_values <- list(
  c("Receitas", "Competência", NA, "Valor"),
  c("Despesas",	"Liquidação", "Documento",	"Forma de Pgto.",	NA,	"Valor"))

head_cols <- sapply(head_values, length) |>
  max()
names(head_values) <- names(rec_tbl)[1:head_cols + 1]

# aux funs

vec_to_tibl <- function(vec, names_from_tbl) {
  vec_names <- names(names_from_tbl)[1:length(vec) + 1]
  names(vec) <- vec_names
  t(vec) |>
    as_tibble()
}

head_rows <- function(head_values, tbl_to_extract_rows) {
  head_values |>
    lapply(vec_to_tibl, tbl_to_extract_rows) |>
    bind_rows() |>
    inner_join(tbl_to_extract_rows) |>
    pull(last_col())
}

head_values |>
  head_rows(rec_tbl)

lapply(head_values, t)

head_tbl <- t(head_values) |>
  as_tibble()

#from here
inner_join(head_tbl,
  rec_tbl) |>


tbl[tbl$character == "Despesas",] |>
  rectify()

get_partition_corners_rows(
  tbl, filled_cols = c(1:2, 4), empty_cols = 3)

tbl |>
  rectify() |>
  View()

# partition data
corners_main <- tbl |>
  filter(
    row %in% summary_rows,
    col == 1L)

partition(tbl, corners = corners_main, align = "top_left") |>
  View()


# return
tibble(
  "chapter" = c("Resumo movimentação"),
  "data" = list(summ_tbl))

