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
#' @export
#'
#' @examples
#' acordos <- system.file("extdata/acordos.xlsx", package = "tidycondr") |>
#'   read_contas()
#' acordos_part <- partition_contas(acordos)
#' acordos_part <- acordos_part[grepl("028", acordos_part$code),]
#' partition_028(acordos_part[1,4][[1]][[1]])
#' acordos_part |>
#'   mutate(processed = purrr::map(cells, partition_028))
#'
partition_028 <- function(tbl) {

  summary <- partition_028_summary(tbl) |>
    dplyr::select(info_date, date) |>
    tidyr::pivot_wider(
      names_from = info_date,
      values_from = date)

  groups <- partition_028_split_groups(tbl) |>
    dplyr::mutate(
      cobrancas_originais  = lapply(cobrancas_originais, partition_028_cobrancas),
      parcelas_do_acordo   = lapply(parcelas_do_acordo , partition_028_parcelas),
      renegociado          = sapply(parcelas_do_acordo, \(x) any(x$renegociado)))

  cbind(summary, groups)
}
