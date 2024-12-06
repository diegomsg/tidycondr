#' Pull Info from Partition
#'
#' Pull info from a tidyxl from a tibble, previously partitioned.
#'
#' @param df A tidyxl tibble
#'
#' @return Title, subtitle or code character vector.
#' @name pull_info
NULL

#' @rdname pull_info
pull_title <- function(df) {
  assert_tidyxl(tbl)
  df$character[1]
}

#' @rdname pull_info
#' @importFrom stringr str_to_sentence
pull_subtitle <- function(df) {
  assert_tidyxl(tbl)
  str <- gsub("[[:punct:]]", "", df$character[2])
  stringr::str_to_sentence(str)
}

#' @rdname pull_info
pull_code <- function(df) {
  assert_tidyxl(tbl)
  stringr::str_extract(df$character[1], "^[0-9]+[A-Z]")
}
