#' Process 037* Report Partition
#'
#' Process financial summary (Resumo financeiro).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Original tibble with new column `processed_data`
#' @importFrom janitor clean_names
#' @importFrom tidyr separate_wider_delim
#' @export
#'
#' @examples
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("037", pcontas_part$code),]
#'
#' partition_037(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_037))
#'
partition_037 <- function(tbl) {
  assert_tidyxl(tbl)

  start_row <- tbl$row[1]
  period_txt <- tbl$character[[1]]
  head <-  tbl[tbl$row == start_row + 1L,]$character |>
    tail(3)

  tbl |>
    filter(row > start_row + 1) |>
    rectify() |>
    select(-1) |>
    set_names(head) |>
    janitor::clean_names() |>
    tidyr::separate_wider_delim(
      lancamento,
      delim = " ",
      names = c("lancamento_cod", "lancamento_desc"),
      too_few = "align_end",
      too_many = "merge",
      cols_remove = FALSE) |>
    mutate(
      across(
        c(valor, percent),
        ~ readr::parse_number(
          gsub("^\\(", "-", .x),
          locale = readr::locale(
            decimal_mark = ",",
            grouping_mark = "."))))
}
