#' Partition 028 Report into Groups Cobrancas Renegociadas / Parcelas Definidas
#'
#' Split groups Cobrancas Renegociadas / Parcelas Definidas partition of
#'  financial report (cobrancas_originais e parcelas_do_acordo ).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy nested data.
#'
#' @importFrom unpivotr partition_dim
#' @importFrom tidyr nest separate_wider_regex fill
#' @importFrom janitor clean_names
#' @export
#'
#' @examples
#' acord <- read_contas("data_raw/acordos.xlsx")
#' pacord_part <- partition_contas(acord)
#' pacord_part <- pacord_part[grepl("028", pacord_part$code),]
#'
#' partition_028_split_groups(pacord_part[1,4][[1]][[1]])
#'
#' pcontas_part |>
#'  mutate(processed = purrr::map(cells, partition_028_split_groups))
#'
partition_028_split_groups <- function(tbl) {
  assert_tidyxl(tbl)

  # partitions index
  first_lvl_group_pat <- "^Acordo [0-9]+$"
  first_lvl_group_rows_id <- tbl$row[
    grepl(first_lvl_group_pat,
          tbl$character)]
  first_lvl_group_rows <- tibble(
    "first_lvl_partition" = unpivotr::partition_dim(
      tbl$row,
      first_lvl_group_rows_id,
      bound = "upper"))

  cobrancas_lvl_group_pat <- "^Cobranças originais$"
  cobrancas_lvl_group_rows_id <- tbl$row[
    grepl(cobrancas_lvl_group_pat,
          tbl$character)]
  cobrancas_lvl_group_rows <- tibble(
    "cobrancas_lvl_partition" = unpivotr::partition_dim(
      tbl$row,
      cobrancas_lvl_group_rows_id,
      bound = "upper"))

  parcelas_lvl_group_pat <- "^Parcelas do acordo$"
  parcelas_lvl_group_rows_id <- tbl$row[
    grepl(parcelas_lvl_group_pat,
          tbl$character)]
  parcelas_lvl_group_rows <- tibble(
    "parcelas_lvl_partition" = unpivotr::partition_dim(
      tbl$row,
      parcelas_lvl_group_rows_id,
      bound = "upper"))

  # processing
  tidy_base <- bind_cols(
    first_lvl_group_rows,
    cobrancas_lvl_group_rows,
    parcelas_lvl_group_rows,
    tbl
  ) |> group_by(
    first_lvl_partition,
    cobrancas_lvl_partition,
    parcelas_lvl_partition) |>
    tidyr::nest() |>
    ungroup() |>
    tail(-1) |>
    select(first_lvl_partition, data) |>
    mutate(
      info = sapply(data, pull_title)
    ) |>
    tidyr::separate_wider_regex(
      info,
      c(
        info = "[[:alpha:]|[:punct:]|\\s]*",
        acordo_id = "[0-9]*"),
      too_few = "align_start") |>
    mutate(
      info = trimws(info, "both"),
      acordo_id = as.numeric(acordo_id)) |>
    tidyr::fill(acordo_id) |>
    # pivot info to collumns
    pivot_wider(
      id_cols = acordo_id,
      names_from = info,
      values_from = data) |>
    janitor::clean_names() |>
    mutate(
      acordo_detail = lapply(acordo, partition_028_acordo_detail),
      .keep = "unused",
      .after = acordo_id) |>
    tidyr::unnest(acordo_detail)

  tidy_base
}
