pcontas <- read_contas("data_raw/pcontas.xlsx")
pcontas_part <- partition_contas(pcontas)
pcontas_part <- pcontas_part[grepl("060", pcontas_part$code),]
pcontas_part$subtitle

tbl <- pcontas_part[2,4][[1]][[1]]
tbl$character

partition_060_short <- function(tbl) {
  assert_tidyxl(tbl)

  datas <- tbl$character |>
    str_extract_all("[a-z|A-Z]+/[0-9]{2,4}") |>
    unlist() |>
    paste0("1/", ... = _) |>
    strptime("%d/%B/%Y") |>
    as.Date()

  tibble(
    info_date = c("start_month", "end_month"),
    info_data = c("mes_inicio", "mes_fim"),
    date = datas)
}



params <- if (length(tbl$character) == 1) {
  partition_060_short(tbl)
} else {
  # partition_060_long()

}
