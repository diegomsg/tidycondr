#' Split Contas in Partitions
#'
#' Get [unpivotr::partition()] for each report in [read_contas()] tibble.
#'
#' @param tbl A tidyxl tibble from [read_contas()].
#'
#' @return A tibble with nested cols, 1 row per report
#' @export
#'
#' @examples
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
#' partition_contas(pcontas)
#'
partition_contas <- function(tbl) {
  assert_tidyxl(tbl)

  regpat <- "^[0-9]+[A-Z].*[\\(][0-9]+[\\)]"
  corners <- tbl[grepl(regpat, tbl$character),]
  data <- unpivotr::partition(tbl, corners)["cells"]

  data$title <- sapply(data$cells, pull_title)
  data$subtitle <- sapply(data$cells, pull_subtitle)
  data$code <- sapply(data$cells, pull_code)
  data$cells <- lapply(
    data$cells,
    \(x) {x[-c(1:2),]}) #drop rows 1:2

  data[c("code", "title", "subtitle", "cells")]
}
