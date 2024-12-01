#' Process 065* Report Partition
#'
#' Process financial summary (Receitas liquidadas por data de crédito).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @importFrom unpivotr behead_if
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
#' @examples
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("065", pcontas_part$code),]
#'
#' partition_065(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_065))
#'
partition_065 <- function(tbl) {
  stopifnot("Not a tidyxl tibble." = check_tidyxl(tbl))

  tbl |>
    behead("up-left", "info") |>
    behead("up", "head") |>
    unpivotr::behead_if(
      grepl("Cobranças", character),
      direction = "left-down",
      name = "total_qtd") |>
    behead("down", "total_row") |>
    behead("down", "subtotal_row") |>
    behead("left-up", "credito") |>
    mutate(value = coalesce(character, as.character(numeric))) |>
    select(-c(col:character, total_row, subtotal_row)) |>
    tidyr::pivot_wider(
      names_from = head,
      values_from = value) |>
    select(-c(row)) |>
    janitor::clean_names() |>
    filter(!is.na(codigo)) |>
    mutate(
      referencia = lubridate::my(info, locale = Sys.getlocale("LC_TIME")),
      compet = lubridate::my(compet),
      across(c(total_qtd, codigo, pago:creditado), ~ readr::parse_number(
        gsub("^\\(", "-", .x),
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = "."))),
      across(c(credito, venc, liquidacao), lubridate::dmy)) |>
    select(referencia, compet, codigo, credito, unidade, liquidacao, venc,
           atraso_d, pago, tarifa, creditado)
}
