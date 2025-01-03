#' Call Partition Functions
#'
#' @param tbl partitioned tibble with required cols
#' @param .code_col code column to pass to `call_partition_funs()`
#' @param .cells_col cells column
#' @param .progress progress bar, default `TRUE`
#'
#' @import rlang
#' @return paritioned cells
#' @export
#'
#' @examples
#' parts <- read_contas("data_raw/pcontas.xlsx") |>
#'   partition_contas()
#'
#' parts[grepl("016", parts$code),] |>
#'   call_partition_funs(.code_col = code, .cells_col = cells)
#'
call_partition_funs <- function(
    tbl, .code_col = code, .cells_col = cells, .progress = FALSE) {
  stopifnot(".progress must be logical." = is.logical(.progress))

  mutate(
    tbl,
    cells_parts = purrr::map2(
      .x = {{ .code_col }},
      .y = {{ .cells_col }},
      .f = ~ get_partition_fun(.x)(list(.y)),
      .progress = .progress)
  )
}
