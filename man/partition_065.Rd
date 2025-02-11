% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partition_065.R
\name{partition_065}
\alias{partition_065}
\title{Process 065* Report Partition}
\usage{
partition_065(tbl)
}
\arguments{
\item{tbl}{A tidyxl tibble from \code{\link[=partition_contas]{partition_contas()}}}
}
\value{
Tidy data.
}
\description{
Process financial summary (Receitas liquidadas por data de crédito).
}
\examples{
pcontas <- system.file("extdata/pcontas.xlsx", package = "tidycondr") |>
 read_contas()
pcontas_part <- partition_contas(pcontas)
pcontas_part <- pcontas_part[grepl("065", pcontas_part$code),]

partition_065(pcontas_part[1,4][[1]][[1]])

pcontas_part |>
 mutate(processed = purrr::map(cells, partition_065))

}
