#' Partition 020 Report into Groups Receitas / Despesas
#'
#' Split groups Receitas / Despesas partition of financial report
#'  (Demonstrativo de receitas e despesas analítico).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @examples
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]
#'
#' partition_020_summary(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_020_groups))
#'
partition_020_groups <- function(tbl) {
  assert_tidyxl(tbl)

  rec_tbl <- rectify(tbl)

  ## head row
  head_values <- list(
    c("Receitas", "Competência", NA, "Valor"),
    c("Despesas",	"Liquidação", "Documento",	"Forma de Pgto.",	NA,	"Valor"))

  head_rows <- head_rows(head_values, rec_tbl, verbose = FALSE)

  row_groups <- tibble(
    "partition" = unpivotr::partition_dim(
      tbl$row,
      head_rows,
      bound = "upper"))

  bind_cols(row_groups, tbl) |>
    tidyr::nest(data = -partition) |>
    filter(!is.na(partition)) |>
    mutate(
      info = sapply(partition, info_cell, tbl = tbl)) |>
    select(info, data)
}

info_cell <- function(row, tbl, col = 1L) {
  tbl[tbl$row == row & tbl$col == col,]$character
}
