library(tidyxl)
library(unpivotr)

# raw xlsx from restricted xlsx
acordos_raw <- xlsx_cells("ex/acordos.xlsx")

# header rows to skip
skip_rows <- c(
  2:5, 7:10, 26:44, 46:49, 62:74, 76:79, 83:92, 94:97, 114:124, 126:129,
  134:140)

# anonymise
acordos_anonym <- anonymise(acordos_raw, skip_rows)
acordos_anonym <- rectify(acordos_anonym)
acordos_anonym <- acordos_anonym[-1]

# write
writexl::write_xlsx(
  acordos_anonym,
  path = "extdata/acordos.xlsx",
  col_names = FALSE,
  format_headers = FALSE)

usethis::use_data(acordos_anonym, overwrite = TRUE)
