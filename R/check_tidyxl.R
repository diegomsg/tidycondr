#' Check a tidyxl tibble
#'
#' Check if it`s a simple [tidyxl::xlsx_cells()] with selected cols.
#'
#' @param tbl tibble
#'
#' @return logical
#'
#' @examples
#' pcontas <- system.file("data_raw/pcontas.xlsx", package = "tidycondr") |>
#'  read_contas()
#' check_tidyxl(pcontas)
#'
#' acordos <- system.file("data_raw/acordos.xlsx", package = "tidycondr") |>
#'  read_contas()
#' check_tidyxl(acordos)
#'
check_tidyxl <- function(tbl) {
  needed_cols <- c(
    "row", "col", "is_blank", "content", "data_type", "error", "logical",
    "numeric", "date", "character")

  all(needed_cols %in% names(tbl))
}

#' Assert if  it is a tidyxl tibble
#'
#' If it`s not a simple [tidyxl::xlsx_cells()] with selected cols,
#'   stop execution.
#'
#' @param tbl tibble
#'
#' @return stop or not
#'
#' @examples
#' pcontas <- system.file("data_raw/pcontas.xlsx", package = "tidycondr") |>
#'  read_contas()
#' assert_tidyxl(pcontas)
#'
#' acordos <- system.file("data_raw/acordos.xlsx", package = "tidycondr") |>
#'  read_contas()
#' assert_tidyxl(acordos)
#'
assert_tidyxl <- function(tbl) {
  stopifnot("Not a tidyxl tibble." = check_tidyxl(tbl))
}
