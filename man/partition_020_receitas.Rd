% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partition_020_receitas.R
\name{partition_020_receitas}
\alias{partition_020_receitas}
\title{Process 020 Receitas Analítico Report Partition}
\usage{
partition_020_receitas(tbl)
}
\arguments{
\item{tbl}{A tidyxl tibble from \code{\link[=partition_contas]{partition_contas()}}}
}
\value{
Tidy data.
}
\description{
Process receitas partition of financial report
(Demonstrativo de receitas e despesas analítico |>
receitas).
}
\examples{
pcontas <- read_contas("data_raw/pcontas.xlsx")
pcontas_part <- partition_contas(pcontas)
pcontas_part <- pcontas_part[grepl("020", pcontas_part$code),]

tbl_groups <- partition_020_groups(tbl)

partition_020_receitas(
 tbl_groups[tbl_groups$info == "Receitas",]$data[[1]])

}
