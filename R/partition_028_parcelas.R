#' Process 028 parcelas Report Partition
#'
#' Process parcelas_do_acordo partition of financial report.
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' acordos <- system.file("extdata/acordos.xlsx", package = "tidycondr") |>
#'   read_contas()
#' pacord <- partition_contas(acordos)
#' pacord <- pacord[grepl("028", pacord$code),]
#' pacord_part <- pacord$cells[1][[1]]
#' tbl <- partition_028_split_groups(pacord_part)
#' parc <- tbl$parcelas_do_acordo[1][[1]]
#' partition_028_parcelas(parc)
#'
partition_028_parcelas <- function(parc) {
  assert_tidyxl(parc)

  parc |>
    filter(!is_blank) |>
    ##partition_028_parcelas
    unpivotr::rectify() |>
    select(-1) |>
    janitor::row_to_names(2, remove_rows_above = TRUE) |>
    janitor::clean_names() |>
    mutate(
      across(
        emitido:pago,
        last,
        .names = "{.col}_total")) |>
    head(-1) |>
    mutate(
      obs_id = stringi::stri_extract_first(observacao, regex = "\\d+"),
      obs_n = stringi::stri_extract_last(observacao, regex = "\\d+"),
      .after = observacao,
      .keep = "unused") |>
    mutate(
      across(
        vencimento:liquidacao,
        lubridate::dmy),
      across(
        c(numero, obs_id:obs_n),
        as.integer),
      renegociado = stringi::stri_detect_regex(pago, "[a-z|A-Z]"),
      across(
        starts_with("emitido") | starts_with("pago"),
        \(x) readr::parse_number(
          x,
          locale = readr::locale(
            decimal_mark = ",",
            grouping_mark = ".")) |>
          suppressWarnings())) |>
    filter(!is.na(numero))
}
