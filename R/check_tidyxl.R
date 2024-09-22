#' Check a tidyxl tibble
#'
#' Check if it`s a simple [tidyxl::xlsx_cells()] with selectec cols.
#'
#' @param tbl tibble
#'
#' @return logical
#'
check_tidyxl <- function(tbl) {
  names(tbl) |>
    identical(c("row", "col", "is_blank", "content", "data_type", "error",
      "logical", "numeric", "date", "character"))
}
