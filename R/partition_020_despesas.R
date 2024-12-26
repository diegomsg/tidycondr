#' Process 020 Despesas Analítico Report Partition
#'
#' Process despesas partition of financial report
#'  (Demonstrativo de receitas e despesas analítico |>
#'    despesas).
#'
#' @param tbl A tidyxl tibble from [partition_contas()]
#'
#' @return Tidy data.
#'
#' @examples
#' pcontas <- read_contas("data_raw/pcontas.xlsx")
#' pcontas_part <- partition_contas(pcontas)
#' pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]
#'
#' tbl_groups <- partition_020_groups(tbl)
#'
#' partition_020_despesas(
#'  tbl_groups[tbl_groups$info == "Despesas",]$data[[1]])
#'
partition_020_despesas <- function(tbl) {
  assert_tidyxl(tbl)

  # receitas level groups
  first_lvl_group_pat <- " \\([0-9]{1,3},?[0-9]*\\%\\)$"
  first_lvl_group_rows_id <- tbl$row[
    grepl(first_lvl_group_pat,
          tbl$character)]
  first_lvl_group_rows <- tibble(
    "first_lvl_partition" = unpivotr::partition_dim(
      tbl$row,
      first_lvl_group_rows_id,
      bound = "upper"))

  second_lvl_group_rows_id <- get_partition_corners_rows(tbl, 1, 2:4)
  second_lvl_group_rows <- tibble(
    "second_lvl_partition" = unpivotr::partition_dim(
      tbl$row,
      second_lvl_group_rows_id,
      bound = "upper"))

  # receitas resumo
  bind_cols(
    first_lvl_group_rows,
    second_lvl_group_rows,
    tbl
  ) |>
    behead("up", "head") |>
    tidyr::replace_na(list(head = "prop")) |>
    filter(
      !if_all(
        c(first_lvl_partition, second_lvl_partition),
        is.na)) |>
    group_by(first_lvl_partition, second_lvl_partition) |>
    tidyr::nest() |>
    mutate(
      grupo = purrr::map2_chr(
        data, first_lvl_partition,
        get_group_row),
      subgrupo = purrr::map2_chr(
        data, second_lvl_partition,
        get_group_row)) |>
    ungroup() |>
    tidyr::fill(grupo, subgrupo, .direction = "down") |>
    filter(
      first_lvl_partition != second_lvl_partition,
      !is.na(first_lvl_partition),
      !is.na(second_lvl_partition)) |>
    mutate(
      data = purrr::map2(
        data, second_lvl_partition,
        \(x, y) x[x$row != y,]
      )) |>
    tidyr::unnest(data) |>
    mutate(
      value = coalesce(
        character,
        as.character(numeric))) |>
    select(-c(col:character)) |>
    pivot_wider(
      names_from = head,
      values_from = value) |>
    select(-row) |>
    janitor::clean_names() |>
    filter(!is.na(liquidacao)) |>
    slice_head(n = -2) |>
    tidyr::separate_wider_regex(
      grupo,
      c(
        grupo = ".*",
        prop_grupo = first_lvl_group_pat)) |>
    rename(liquidacao_txt = liquidacao) |>
    mutate(
      across(c(prop, valor),
             ~ gsub("\\(", "-", .x) |>
               parse_number(
                 locale = locale(
                   decimal_mark = ",",
                   grouping_mark = "."))),
      prop_grupo = parse_number(
        prop_grupo,
        locale = locale(
          decimal_mark = ",",
          grouping_mark = ".")),
      liquidacao_data = if_else(
        grepl("[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}", liquidacao_txt),
        dmy(liquidacao_txt, quiet = TRUE),
        my(liquidacao_txt, quiet = TRUE)),
      liquidacao_mes = lubridate::floor_date(liquidacao_data, "month")) |>
    select(-c(first_lvl_partition, second_lvl_partition)) |>
    relocate(valor, prop, prop_grupo, .after = last_col()) |>
    relocate(liquidacao_data, liquidacao_mes, .after = liquidacao_txt)
}
