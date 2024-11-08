#' Process 016B Report Partition
#'
#' Process financial summary (Resumo financeiro).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Original tibble with new column `processed_data`
#' @export
#'
#' @examples
#' pcontas <- read_contas("ex/pcontas.xlsx")
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- filter(pcontas_part, code == "016B")
#'
#' partition_016b(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_016b))
#'
partition_016b <- function(tbl) {
  stopifnot("Not a tidyxl tibble." = check_tidyxl(tbl))

  tbl |>
    behead("up-left", "period_txt") |>
    behead("up", "info") |>
    behead("left", "conta") |>
    select(period_txt, conta, info, valor = character) |>
    mutate(
      valor = readr::parse_number(
        gsub("^\\(", "-", valor),
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = ".")),
      period_start = lubridate::dmy(
        stringr::str_extract(
          period_txt,
          "(\\d{2}/\\d{2}/\\d{4}).*(\\d{2}/\\d{2}/\\d{4})",
          group = 1)),
      period_end = lubridate::dmy(
        stringr::str_extract
        (period_txt,
          "(\\d{2}/\\d{2}/\\d{4}).*(\\d{2}/\\d{2}/\\d{4})",
          group = 2))) |>
    relocate(period_txt, period_start, period_end)
}
