#' Get Rows for Corners Partitions
#'
#' @param tbl Tidyxl tibble
#' @param filled_cols numeric vector for non empty columns, using original data
#' @param empty_cols numeric vector for empty columns, using original data
#'
#' @return numeric vector for rows
#'
get_partition_corners_rows <- function(tbl, filled_cols, empty_cols) {
  assert_tidyxl(tbl)
  stopifnot(
    "`filled_cols` and `empty_cols` must be numeric vectors." =
    all(
      sapply(c(filled_cols, filled_cols), is.numeric),
      sapply(c(filled_cols, filled_cols), is.vector)))

  rec_tbl <- rectify(tbl)

  # include `row/col` collumn from `rectify()`
  filled_cols <- filled_cols + 1
  empty_cols <- empty_cols + 1

  # limite cols intervals
  max_cols <- 1:ncol(rec_tbl)
  filled_cols <- intersect(filled_cols, max_cols)
  empty_cols <- intersect(empty_cols, max_cols)

  condition_filled <- apply(!is.na(rec_tbl[filled_cols]), 1, all)
  condition_empty <- apply(is.na(rec_tbl[empty_cols]), 1, all)

  rec_tbl[condition_filled & condition_empty,][1][[1]]
}
