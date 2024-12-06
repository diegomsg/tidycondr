get_group_row <- function(cells, row, col = 1, col_info = "character") {
  assert_tidyxl(cells)

  info <- cells[cells$row == row & cells$col == 1,] |>
    pull(!!col_info) |>
    unlist()

  if (is_empty(info)) {
    NA
  } else {
    info
  }
}
