#' Get Group Row by Position Id
#'
#' @param cells Tidy cells.
#' @param row Row number.
#' @param col Col number.
#' @param col_info Type of, as column name in tidy cells.
#'
#' @returns Character vector.
#' @export
#'
get_group_row <- function(cells, row, col = 1, col_info = "character") {
  assert_tidyxl(cells)

  info <- cells[cells$row == row & cells$col == 1,] |>
    dplyr::pull(!!col_info) |>
    unlist()

  if (rlang::is_empty(info)) {
    NA
  } else {
    info
  }
}
