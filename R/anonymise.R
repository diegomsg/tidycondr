#' Anonymise tidyxl data
#'
#' Make all characters anonymous, ignoring numbers, dates, and numbers
#'  stored as characters.
#'
#' @param tbl tidyxl tibble
#' @param skip_rows integer vector of rows to skip
#' @param big_mark passed to [is_numeric_like()]
#' @param decimal_mark passed to [is_numeric_like()]
#'
#' @return tidyxl tibble
#' @export
#'
#' @examples
#' xlsx_cells("data_raw/pcontas.xlsx") |>
#'  anonymise(
#'    skip_rows = c(
#'      2:4, 12:14, 16, 89, 91, 229, 231, 233, 235:237, 252:254,
#'      264:266, 389,391:400, 402:403, 405:406, 408:410))
#'
anonymise <- function(tbl, skip_rows = NULL,
                      big_mark = ".", decimal_mark = ",") {
  stopifnot("Not a tidyxl tibble." = check_tidyxl(tbl))

  if(!missing(skip_rows)) {
    stopifnot("Must be integer vector of rows." = is.numeric(skip_rows))
    skip_rows <- as.integer(skip_rows)
  }

  # split 2 parts
  unchanged <- subset(tbl, row %in% skip_rows)
  to_change <- subset(tbl, !(row %in% skip_rows))

  # cells index to be anonimised, ignoring numbers
  number_like_cells <- c(
    sapply(to_change$character, is_numeric_like,
           big_mark = big_mark, decimal_mark = decimal_mark),
    use.names = FALSE)
  na_cells <- is.na(to_change$character)

  anomnym_ind <- !number_like_cells & !na_cells

  # change to anonymous chars
  to_change$character[anomnym_ind] <-
    sapply(
      to_change$character[anomnym_ind],
      replace_rand_case,
      USE.NAMES = FALSE)

  # bind split parts
  anonym_raw <- rbind(unchanged, to_change)

  # return sorted as tidyxl
  anonym_raw[order(anonym_raw$row, anonym_raw$col),]
}
