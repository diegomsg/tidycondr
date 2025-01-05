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

  rec_tbl <- unpivotr::rectify(tbl)

  ## head row
  head_values <- list(
    c("Receitas", "Competência", NA, "Valor"),
    c("Despesas",	"Liquidação", "Documento",	"Forma de Pgto.",	NA,	"Valor"))

  head_rows <- head_rows(head_values, rec_tbl, verbose = FALSE)

  row_groups <- tibble::tibble(
    "partition" = unpivotr::partition_dim(
      tbl$row,
      head_rows,
      bound = "upper"))

  cbind(row_groups, tbl) |>
    tidyr::nest(data = -partition) |>
    dplyr::filter(!is.na(partition)) |>
    dplyr::mutate(
      info = sapply(partition, info_cell, tbl = tbl)) |>
    dplyr::select(info, data)
}

info_cell <- function(row, tbl, col = 1L) {
  tbl[tbl$row == row & tbl$col == col,]$character
}

partition_020_analit <- function(tbl) {
  tidy_tbl <- try(
    partition_020_groups(tbl),
    silent = TRUE)

  if (inherits(tidy_tbl, "try-error")) {
    cli::cli_alert_warning("Unable to retrieve analitical info from partition.")
    tibble::tibble()
  }  else {
    return(tidy_tbl)
  }
}
