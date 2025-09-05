#' Process 065* Report Partition
#'
#' Process financial summary (Receitas liquidadas por data de crédito).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @export
#'
#' @examples
#' pcontas <- system.file("extdata/pcontas.xlsx", package = "tidycondr") |>
#'  read_contas()
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("065", pcontas_part$code),]
#'
#' partition_065(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_065))
#'
partition_065 <- function(tbl) {
  assert_tidyxl(tbl)

  unpivotr::behead(tbl, "up-left", "info") |>
  unpivotr::behead("up", "head") |>
  dplyr::mutate(
    character = dplyr::coalesce(character, as.character(numeric))) |>
  unpivotr::behead_if(
    grepl("Cobranças", character),
    direction = "left-down",
    name = "total_qtd") |>
  unpivotr::behead("down", "total_row") |>
  unpivotr::behead("down", "subtotal_row") |>
  unpivotr::behead("left-up", "credito") |>
  dplyr::mutate(value = dplyr::coalesce(character, as.character(numeric))) |>
  dplyr::select(-c(col:character, total_row, subtotal_row)) |>
  tidyr::pivot_wider(
    names_from = head,
    values_from = value) |>
  dplyr::select(-c(row)) |>
  janitor::clean_names() |>
  dplyr::filter(!is.na(codigo)) |>
  dplyr::mutate(
    referencia = paste0(
      "01/",
      stringi::stri_extract_first_regex(
        info,
        "[[:alpha:]|0-9]*/[0-9]{2,4}")) |>
      as.Date("%d/%b/%Y"),
    compet = lubridate::my(compet),
    dplyr::across(c(total_qtd, codigo, atraso_d:creditado), ~ readr::parse_number(
      gsub("^\\(", "-", .x),
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."))),
    dplyr::across(c(credito, venc, liquidacao), lubridate::dmy)) |>
  dplyr::select(referencia, compet, codigo, credito, unidade, liquidacao,
                venc, atraso_d, pago, tarifa, creditado)
}
