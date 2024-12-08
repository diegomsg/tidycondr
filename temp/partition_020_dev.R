pcontas <- read_contas("ex/pcontas.xlsx")
pcontas_part <- partition_contas(pcontas)
pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]

tbl <- pcontas_part[2,4][[1]][[1]]
