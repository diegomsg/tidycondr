#' Retrive Column Names from Tibble
#'
#' @param vec Character vector sequence of cell values in row
#' @param names_from_tbl Tibble to retrive column names from
#'
#' @return Tibble with col names and values in vector.
#'
vec_to_tibl <- function(vec, names_from_tbl) {
  vec_names <- names(names_from_tbl)[1:length(vec) + 1]
  names(vec) <- vec_names
  t(vec) |>
    as_tibble(.name_repair = "check_unique")
}

#' Rows Indices for Sequence Cell Values
#'
#' @param head_values Vector or list of vectors
#' @param tbl_to_extract_rows Tibble to retrive column names from
#'
#' @return Rows indices numeric vector
#'
head_rows_base <- function(head_values, tbl_to_extract_rows) {
  head_values |>
    lapply(vec_to_tibl, tbl_to_extract_rows) |>
    bind_rows() |>
    inner_join(tbl_to_extract_rows) |>
    pull(last_col())
}

#' Rows Indices for Sequence Cell Values
#'
#' @param head_values Vector or list of vectors
#' @param tbl_to_extract_rows Tibble to retrive column names from
#' @param verbose Verbose for `dplyr::inner_join()` messages
#'
#' @return Rows indices numeric vector
#' @export
#'
head_rows <- function(head_values, tbl_to_extract_rows, verbose = TRUE) {
  if(verbose) {
    head_rows_base(head_values, tbl_to_extract_rows)
  } else {
    suppressMessages(
      head_rows_base(head_values, tbl_to_extract_rows))
  }
}
