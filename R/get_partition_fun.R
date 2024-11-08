#' Get Partition Function from String
#'
#' @param str code for cells block
#'
#' @return function as `partition_*` replacing *
#' @export
#'
#' @examples
#' parts <- read_contas("data_raw/pcontas.xlsx") |>
#'   partition_contas()
#'
#' partition_function("016B")(list(parts[1, 4][[1]][[1]]))
#'
get_partition_fun <- function(str) {
  fun_code <- strtrim(str, 3)
  fun_name <- paste0("partition_", fun_code)
  function(x) {
    do.call(fun_name, x)
  }
}
