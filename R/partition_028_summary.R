#' Process 028* Report Partition Details
#'
#' Process financial summary (Acordo detalhado).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy nested data.
#'
#' @export
#'
#' @examples
#' acord <- read_contas("extdata/acordos.xlsx")
#' pacord_part <- partition_contas(acord)
#' pacord_part <- pacord_part[grepl("028", pacord_part$code),]
#'
#' partition_028_summary(pacord_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_028_summary))
#'
partition_028_summary <- function(tbl) {
  assert_tidyxl(tbl)

  datas <- tbl$character[1] |>
    stringi::stri_extract_all_regex("[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}") |>
    unlist() |>
    strptime("%d/%m/%Y") |>
    as.Date()

  tibble::tibble(
    info_date = c("start_month", "end_month"),
    info_data = c("mes_inicio", "mes_fim"),
    date = datas)
}
