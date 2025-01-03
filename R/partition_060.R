#' Process 060* Report Partition
#'
#' Process financial summary (Demonstrativo de inadimplência, por unidade e anual).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy nested data.
#'
#' @importFrom tibble as_tibble_col
#' @importFrom tidyr separate_wider_delim
#' @importFrom janitor clean_names
#'
#' @export
#'
#' @examples
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
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
    str_extract_all("[a-z|A-Z]+/[0-9]{2,4}") |>
    unlist() |>
    paste0("1/", ... = _) |>
    strptime("%d/%B/%Y") |>
    as.Date()

  tibble(
    info_date = c("start_month", "end_month"),
    info_data = c("mes_inicio", "mes_fim"),
    date = datas)
}

partition_060_long <- function(tbl) {
  datas <- tbl$character[1] |>
    str_extract_all("[a-z|A-Z]+/[0-9]{2,4}") |>
    unlist() |>
    paste0("1/", ... = _) |>
    strptime("%d/%B/%Y") |>
    as.Date()

  params_txt <- tbl$character |>
    tail(4) |>
    tibble::as_tibble_col("txt") |>
    tidyr::separate_wider_delim(
      cols = txt,
      delim = ": ",
      names = c("param", "valor")) |>
    mutate(
      param = sub("^Valores atualizados em$", "Atualização", param))

  params_tidy <- params_txt |>
    pivot_wider(
      names_from = param,
      values_from = valor) |>
    janitor::clean_names() |>
    mutate(
      atualizacao = dmy(atualizacao),
      across(
        -atualizacao,
        ~ parse_number(
          .x,
          locale = locale(
            decimal_mark = ".",
            grouping_mark = " ")) / 100)) |>
    rename_with(
      .cols = -atualizacao,
      ~ paste0(.x, "_am"))

  list(
    datas = datas,
    params_txt = params_txt,
    params_tidy = params_tidy
  )
}
