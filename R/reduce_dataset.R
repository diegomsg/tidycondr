#' Reduce datasets tibble to one row per report
#'
#' @description
#' Bind rows from tibbles in dataset. It reduces from multiple rows in dataset,
#'  to one row for each file. If unnest is true, list collumns will be unnested.
#'
#' @param dataset tibble dataset from [read_contas_partitions()] over multiple
#'  files.
#' @param unnest unnest one level list columns.
#'
#' @return tibble
#'
#' @export
#'
#' @examples
#' read_acordos_partitions("extdata/acordos.xlsx", .progress = TRUE) |>
#'   reduce_dataset()
#'
reduce_dataset <- function(dataset, unnest = FALSE) {
  bind_tbl <- Reduce(rbind, dataset, simplify = TRUE)

  if (unnest) {
    list_cols <- which(sapply(bind_tbl, is.list))
    bind_tbl <- tidyr::unnest(bind_tbl, cols = names(list_cols))
  }

  bind_tbl
}

#' Reduce acordos datasets tibble to one row per report
#'
#' @description
#' Bind rows from tibbles in dataset. It reduces from multiple rows in dataset,
#'  one row for each file
#'
#' @param dataset tibble dataset from [read_contas_partitions()].
#'
#' @return tibble
#'
#' @export
#'
#' @examples
#' read_acordos_partitions("extdata/acordos.xlsx", .progress = TRUE) |>
#'   reduce_acordos()
#'
reduce_acordos <- function(dataset) {
  acordos_reduce <- reduce_dataset(dataset)
  acordos_reduce$ano <- sapply(
    acordos_reduce$cells_parts,
    \(x) x$start_month |>
      format("%Y") |>
      as.integer() |>
      unique())

  cols <- names(acordos_reduce)
  cols <- c(cols[cols != "cells_parts"], "cells_parts")

  acordos_reduce[cols]
}
