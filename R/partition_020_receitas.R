#' Process 020 Receitas Analítico Report Partition
#'
#' Process receitas partition of financial report
#'  (Demonstrativo de receitas e despesas analítico |>
#'    receitas).
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
#' partition_020_receitas(
#'  tbl_groups[tbl_groups$info == "Receitas",]$cells[[1]])
#'
partition_020_receitas <- function(tbl) {
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
    filter(first_lvl_partition != second_lvl_partition) |>
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
    filter(!is.na(competencia)) |>
    tidyr::separate_wider_regex(
      grupo,
      c(
        grupo = ".*",
        prop_grupo = first_lvl_group_pat)) |>
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
      mes = if_else(
        grepl("[0-9]{1,2}/[0-9]{2,4}", competencia),
        my(competencia, quiet = TRUE),
        NA),
      acordo = grepl("acordo", competencia, ignore.case = TRUE),
      .after = competencia) |>
    select(-c(first_lvl_partition, second_lvl_partition)) |>
    relocate(valor, prop, prop_grupo, .after = last_col())
}
