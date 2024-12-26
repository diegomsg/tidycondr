#' Process 028 Cobrancas Report Partition
#'
#' Process cobrancas_originais partition of financial report.
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @importFrom tidyr fill
#' @importFrom janitor clean_names
#' @importFrom unpivotr partition_dim
#' @export
#'
#' @examples
#' acord <- read_contas("data_raw/acordos.xlsx")
#' pacord <- partition_contas(acord)
#' pacord <- pacord[grepl("028", pacord$code),]
#' pacord_part <- pacord$cells[1][[1]]
#' tbl <- partition_028_split_groups(pacord_part)
#' cob <- tbl$cobrancas_originais[1][[1]]
#' partition_028_cobrancas(cob)
#'
partition_028_cobrancas <- function(cob) {
  assert_tidyxl(cob)

  first_lvl_group_rows_id <- get_partition_corners_rows(cob, 1:5, 6)
  first_lvl_group_rows <- tibble(
    "first_lvl" = unpivotr::partition_dim(
      cob$row,
      first_lvl_group_rows_id,
      bound = "upper"))

  bind_cols(
    first_lvl_group_rows,
    cob) |>
    tail(-1) |>
    behead("up", "head") |>
    behead("right-down", "acrescimo") |>
    mutate(total = last(acrescimo)) |>
    head(-2) |>
    group_by(first_lvl) |>
    mutate(subtotal = last(character)) |>
    filter(row < max(row)) |>
    ungroup() |>
    pivot_wider(
      id_cols = -c(col:date),
      names_from = head,
      values_from = character) |>
    janitor::clean_names() |>
    relocate(c(subtotal, acrescimo, total), .after = last_col()) |>
    mutate(
      numero = as.integer(numero),
      across(
        composicao:total,
        ~ parse_number(
          .x,
          locale = locale(
            decimal_mark = ",",
            grouping_mark = "."))),
      vencimento = dmy(vencimento),
      competencia = my(competencia)) |>
    select(-c(1:2)) |>
    tidyr::fill(numero:competencia, .direction = "down") |>
    mutate(
      tx_atualizacao = total / (total - acrescimo),
      across(
        composicao:subtotal,
        ~ .x * tx_atualizacao,
        .names = "{.col}_atualizado"),
      composicao_prop = composicao_atualizado / total)
}
