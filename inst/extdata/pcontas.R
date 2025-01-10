library(tidyxl)
library(unpivotr)

# raw xlsx from restricted xlsx
pcontas_raw <- xlsx_cells("ex/pcontas.xlsx")

# header rows to skip
skip_rows <- c(
  2:4, 12:14, 16, 89, 91, 229, 231, 233, 235:237, 252:254, 264:266, 389,
  391:400, 402:403, 405:406, 408:410)

# anonymise
pcontas_anonym <- anonymise(pcontas_raw, skip_rows)
pcontas_anonym <- rectify(pcontas_anonym)
pcontas_anonym <- pcontas_anonym[-1]

# write
writexl::write_xlsx(
  pcontas_anonym,
  path = "extdata/pcontas.xlsx",
  col_names = FALSE,
  format_headers = FALSE)

usethis::use_data(pcontas_anonym, overwrite = TRUE)
