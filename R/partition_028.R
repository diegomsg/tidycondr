#' Process 028 Report Partition
#'
#' Process partitions of financial report:
#'  - Acordos
#'  - Cobranças originais do acordo
#'  - Parcelas do acordo
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @importFrom glue glue
#' @export
#'
#' @examples
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("028", pcontas_part$code),]
#'
#' partition_028(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_028))
#'
partition_028 <- function(tbl) {

  summary <- partition_028_summary(pacord_part) |>
    select(info_date, date) |>
    pivot_wider(
      names_from = info_date,
      values_from = date)

  groups <- partition_028_split_groups(pacord_part) |>
    mutate(
      cobrancas_originais  = lapply(cobrancas_originais, partition_028_cobrancas),
      parcelas_do_acordo   = lapply(parcelas_do_acordo , partition_028_parcelas))

  summary |>
    bind_cols(groups)
}
