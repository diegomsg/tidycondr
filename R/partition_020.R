#' Process 020 Report Partition
#'
#' Process partitions of financial report:
#'  - Resumo movimentação
#'  - Demonstrativo de receitas e despesas analítico (em implementação)
#'  - Demonstrativo analítico c fundo de caixa  (em implementação)
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @importFrom glue glue
#' @export
#'
#' @examples
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]
#'
#' partition_020(pcontas_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_020))
#'
partition_020 <- function(tbl) {
  resumo <- tibble(
    "chapter" = c("Resumo movimentação"),
    "data" = list(
      partition_020_summary(tbl)))

  groups <- partition_020_analit(tbl)

  analitico <- if (is_empty(groups)) {
    groups
  } else {
    groups |>
      mutate(code = glue::glue("020_{info}")) |>
      call_partition_funs(
        .cells_col = data) |>
      bind_cols(
        "chapter" = c("Relatório analítico")) |>
      select(chapter, info, data)
  }

  bind_rows(resumo, analitico) |>
    relocate(data, .after = last_col())
}
