#' Process 060* Report Partition
#'
#' Process financial summary (Demonstrativo de inadimplência, por unidade e anual).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy nested data.
#'
#' @export
#'
#' @examples
#' pcontas <- system.file("extdata/pcontas.xlsx", package = "tidycondr") |>
#'  read_contas()
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("060", pcontas_part$code),]
#'
#' partition_060(pcontas_part[1,4][[1]][[1]])
#' partition_060(pcontas_part[2,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_060))
#'
partition_060 <- function(tbl) {
  assert_tidyxl(tbl)

  if (length(tbl$character) == 1) {
    partition_060_short(tbl)
  } else {
    partition_060_long(tbl)
  }
}

partition_060_short <- function(tbl) {
  datas <- tbl$character |>
    stringi::stri_extract_all_regex("[a-z|A-Z]+/[0-9]{2,4}") |>
    unlist() |>
    paste0("1/", ... = _) |>
    strptime("%d/%B/%Y") |>
    as.Date()

  tibble::tibble(
    info_date = c("start_month", "end_month"),
    info_data = c("mes_inicio", "mes_fim"),
    date = datas)
}

partition_060_long <- function(tbl) {
  datas <- tbl$character[1] |>
    stringi::stri_extract_all_regex("[a-z|A-Z]+/[0-9]{2,4}") |>
    unlist() |>
    paste0("1/", ... = _) |>
    strptime("%d/%B/%Y") |>
    as.Date()

  params_txt <- tbl$character |>
    stats::na.omit() |>
    tail(4) |>
    tibble::as_tibble_col("txt") |>
    tidyr::separate_wider_delim(
      cols = txt,
      delim = ": ",
      names = c("param", "valor")) |>
    dplyr::mutate(
      param = sub("^Valores atualizados em$", "Atualização", param))

  params_tidy <- params_txt |>
    tidyr::pivot_wider(
      names_from = param,
      values_from = valor) |>
    janitor::clean_names() |>
    dplyr::mutate(
      atualizacao = lubridate::dmy(atualizacao),
      dplyr::across(
        -atualizacao,
        ~ readr::parse_number(
          .x,
          locale = readr::locale(
            decimal_mark = ".",
            grouping_mark = " ")) / 100)) |>
    dplyr::rename_with(
      .cols = -atualizacao,
      ~ paste0(.x, "_am"))

  list(
    datas = datas,
    params_txt = params_txt,
    params_tidy = params_tidy
  )
}
