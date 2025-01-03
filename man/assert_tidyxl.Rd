% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_tidyxl.R
\name{assert_tidyxl}
\alias{assert_tidyxl}
\title{Assert if  it is a tidyxl tibble}
\usage{
assert_tidyxl(tbl)
}
\arguments{
\item{tbl}{tibble}
}
\value{
stop or not
}
\description{
If it`s not a simple \code{\link[tidyxl:xlsx_cells]{tidyxl::xlsx_cells()}} with selected cols,
stop execution.
}
\examples{
pcontas <- read_contas("data_raw/pcontas.xlsx")
assert_tidyxl(pcontas)

acordos <- read_contas("data_raw/acordos.xlsx")
assert_tidyxl(acordos)

}
