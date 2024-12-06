#' Process 020 Summary Report Partition
#'
#' Process summary partition of financial report
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
#'  mutate(processed = purrr::map(cells, partition_020_summary))
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
}
