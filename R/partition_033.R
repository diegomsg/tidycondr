#' Process 033* Report Partition
#'
#' Process financial summary (Cheques emitidos).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Original tibble with new column `processed_data`
#'
#' @export
#'
partition_033 <- function(tbl) {
  assert_tidyxl(tbl)

  NULL
}
