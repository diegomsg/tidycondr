#' Process 020 Summary Report Partition
#'
#' Process summary partition of financial report
#'  (Demonstrativo de receitas e despesas analítico).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' pcontas <- system.file("extdata/pcontas.xlsx", package = "tidycondr") |>
#'  read_contas()
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]
#'
#' partition_020_summary(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  dplyr::mutate(processed = purrr::map(cells, partition_020_summary))
#'
partition_020_summary <- function(tbl) {
  assert_tidyxl(tbl)

  summary_rows <- get_partition_corners_rows(
    tbl,
    filled_cols = 1:2,
    empty_cols = 3:6)

  tbl |>
    filter(
      row %in% summary_rows) |>
    unpivotr::rectify() |>
    stats::na.omit() |>
    select(2:3) |>
    purrr::set_names(c("info_txt", "valor")) |>
    mutate(
      info = if_else(
        startsWith(info_txt, "S"),
        "Saldo",
        "Mov. Líquido"),
      date = lubridate::dmy(info_txt, quiet = TRUE),
      valor = readr::parse_number(
        valor,
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = "."
        ))) |>
    relocate(info_txt, info, date, valor)
}
