#' Contas xlsx contents
#'
#' @description
#' Read [tidyxl] data from `xlsx`file.
#'
#' @details
#' Outputs tibble with selected variables using [tidyxl::xlsx_cells()]:
#' * `row` The row number of a cell address (integer).
#' * `col` The column number of a cell address (integer).
#' * `is_blank` Whether or not the cell has a value
#' * `content` Raw cell value before type conversion, useful for debugging.
#' * `data_type` The type of a cell, referring to the following columns: error,
#'     logical, numeric, date, character, blank.
#' * `error` The error value of a cell.
#' * `logical` The boolean value of a cell.
#' * `numeric` The numeric value of a cell.
#' * `date` The date value of a cell.
#' * `character` The string value of a cell.
#'
#' @param contas_file
#'
#' @return [tidyxl::xlsx_cells()] tibble with selected variables.
#' @seealso [tidyxl::xlsx_cells()] which this function wraps.
#'
#' @importFrom tidyxl xlsx_cells
#' @export
#'
read_contas <- function(contas_file) {
  xlsx_cells(contas_file)[c("row", "col", "is_blank", "content", "data_type",
    "error", "logical", "numeric", "date", "character")]
}
