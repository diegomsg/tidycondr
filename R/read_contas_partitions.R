#' Contas xlsx Partitions Contents
#'
#' @description
#' Read [tidyxl] data from `xlsx`file and retrieve all partitions using
#'  partition_*() for each report code.
#'
#' @param contas_file xlsx filepath
#' @param .progress show [purrr] progress bar?
#'
#' @return tidy partitions.
#'
#' @export
#'
#' @examples
#' read_contas_partitions("data_raw/pcontas.xlsx", .progress = TRUE)

read_contas_partitions <- function(contas_file, .progress = TRUE) {
  contas_file |>
    read_contas() |>
    partition_contas() |>
    call_partition_funs(
      .code_col = code,
      .cells_col = cells,
      .progress = .progress) |>
    subset(select = -cells)
}
