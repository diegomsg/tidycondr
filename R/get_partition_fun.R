#' Get Partition Function from String
#'
#' @param str code for cells block
#'
#' @return function as `partition_*` replacing *
#' @export
#'
#' @examples
#' parts <- system.file("extdata/pcontas.xlsx", package = "tidycondr") |.
#'  read_contas() |>
#'  partition_contas()
#'
#' partition_function("016B")(list(parts[1, 4][[1]][[1]]))
#'
get_partition_fun <- function(str) {
  fun_code <- if (
    c("Receitas$", "Despesas$") |>
      sapply(grepl, str, TRUE, USE.NAMES = FALSE) |>
      any()
    ) {str}
  else {strtrim(str, 3)}

  fun_code <- casefold(fun_code)
  fun_name <- paste0("partition_", fun_code)

  function(x) {
    do.call(fun_name, x)
  }
}
