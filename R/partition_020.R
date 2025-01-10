#' Process 020 Report Partition
#'
#' Process partitions of financial report:
#'  - Resumo movimentação
#'  - Demonstrativo de receitas e despesas analítico
#'  - Demonstrativo analítico c fundo de caixa
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @export
#'
#' @examples
#' pcontas <- system.file("extdata/pcontas.xlsx", package = "tidycondr") |>
#'  read_contas()
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]
#'
#' partition_020(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  dplyr::mutate(processed = purrr::map(cells, partition_020))
#'
partition_020 <- function(tbl) {
  resumo <- tibble::tibble(
    "chapter" = c("Resumo movimentação"),
    "data" = list(
      partition_020_summary(tbl)))

  groups <- partition_020_analit(tbl)

  analitico <- if (rlang::is_empty(groups)) {
    groups
  } else {
    groups |>
      dplyr::mutate(code = glue::glue("020_{info}")) |>
      call_partition_funs(
        .cells_col = data) |>
      cbind(
        "chapter" = c("Relatório analítico")) |>
      dplyr::select(chapter, info, data)
  }

  rbind(resumo, analitico) |>
    dplyr::relocate(data, .after = dplyr::last_col())
}
