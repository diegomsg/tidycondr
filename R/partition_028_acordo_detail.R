#' Process 028* Report Partition Details
#'
#' @param tbl_acordo acordo partition tibble header
#'
#' @return pivoted wider tibble for futuro unnest
#' @export
#'
partition_028_acordo_detail <- function(tbl_acordo) {
  tbl_acordo <- tbl_acordo |>
    rectify() |>
    select(-1) |>
    tail(-2) |>
    set_names(c("head", "info")) |>
    pivot_wider(
      names_from = head,
      values_from = "info") |>
    janitor::clean_names()

  names(tbl_acordo)[1] <- "unidade"

  tbl_acordo
}
