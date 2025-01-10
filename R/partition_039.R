#' Process 039* Report Partition
#'
#' Process financial summary (Evolução de despesa anual).
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
#' pcontas_part <- pcontas_part[grepl("039", pcontas_part$code),]
#'
#' partition_039(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_039))
#'
partition_039 <- function(tbl) {
  assert_tidyxl(tbl)

  datas <- tbl$character |>
    strsplit(" até ") |>
    unlist() |>
    paste0("1/", ... = _) |>
    strptime("%d/%B/%Y") |>
    as.Date()

  tibble(
    info_date = c("start_month", "end_month"),
    info_data = c("mes_inicio", "mes_fim"),
    date = datas)
}
