#' Replace Characters with Random Characters
#'
#' Replace chars in string with random chars with case matching.
#' @param string Atomic string vector
#'
#' @return Atomic string vector
#'
#' @export
#'
#' @examples
#' str <- "String a ser testada Com NOME 12456"
#' replace_rand_case(str)
#'
#' str_list <- str_list <- list(
#'  "String a ser testada Com NOME 12456",
#'  "String COM data: 13/06/1985",
#'  "Outra STRING com número e $pecial CHAR #$%")
#'  replace_rand_case(str_list[[1]])
#'  lapply(str_list, replace_rand_case)
#'  sapply(str_list, replace_rand_case)
#'
replace_rand_case <- function(string) {
  stopifnot("String must be a vector." = is.vector(string) & !is.list(string))

  chars <- strsplit(string, "")

  low_ind <- which(grepl("[a-z]", chars[[1]]))
  low_rand <- sample(letters, size = length(low_ind), replace = TRUE)

  upp_ind <- which(grepl("[A-Z]", chars[[1]]))
  upp_rand <- sample(LETTERS, size = length(upp_ind), replace = TRUE)

  chars[[1]][low_ind] <- low_rand
  chars[[1]][upp_ind] <- upp_rand

  stringr::str_flatten(chars[[1]])
}
