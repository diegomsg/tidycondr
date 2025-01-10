#' Check a tidyxl tibble
#'
#' Check if it`s a simple [tidyxl::xlsx_cells()] with selected cols.
#'
#' @param tbl tibble
#'
#' @return logical
#'
#' @export
#'
#' @examples
#' pcontas <- system.file("extdata/pcontas.xlsx", package = "tidycondr")
#' check_tidyxl(pcontas)
#'
#' acordos <- system.file("extdata/acordos.xlsx", package = "tidycondr")
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
#' @export
#'
#' @examples
#' pcontas <- system.file("extdata/pcontas.xlsx", package = "tidycondr")
#' pcontas <- read_contas(pcontas)
#' assert_tidyxl(pcontas)
#'
#' acordos <- system.file("extdata/acordos.xlsx", package = "tidycondr")
#' acordos <- read_contas(acordos)
#' assert_tidyxl(acordos)
#'
assert_tidyxl <- function(tbl) {
  stopifnot("Not a tidyxl tibble." = check_tidyxl(tbl))
}
