#' Check a tidyxl tibble
#'
#' Check if it`s a simple [tidyxl::xlsx_cells()] with selected cols.
#'
#' @param tbl tibble
#'
#' @return logical
#'
#' @examples
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
#' check_tidyxl(pcontas)
#'
#' acordos <- read_contas("data_raw/acordos.xlsx")
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
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
#' assert_tidyxl(pcontas)
#'
#' acordos <- read_contas("data_raw/acordos.xlsx")
#' assert_tidyxl(acordos)
#'
assert_tidyxl <- function(tbl) {
  stopifnot("Not a tidyxl tibble." = check_tidyxl(tbl))
}
