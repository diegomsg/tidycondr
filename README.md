
# tidycondr

<!-- badges: start -->
<!-- badges: end -->

[![en](https://img.shields.io/badge/lang-en-red.svg)](https://github.com/diegomsg/tidycondr/blob/master/README.md)
[![pt-br](https://img.shields.io/badge/lang-pt--br-green.svg)](https://github.com/diegomsg/tidycondr/blob/master/README.pt-br.md)

The goal of tidycondr is to provide tools to tidy *Superlogica* condo financial management reports data from messy `xlsx` data files to tidy data.

## Superlogica

It`s a well known provider for condo services management, specially financial data for accountability.

## Installation

You can install the development version of tidycondr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("diegomsg/tidycondr")
```

## Example

This is a basic example tidying up files from superlogica systems:

### Load library

``` r
# load lybrary
library(tidycondr)
```

### Load files contents with `{tidyxl}` wrapper function

``` r
## pcontas
# read pcontas file
system.file("extdata/pcontas.xlsx", package = "tidycondr") |>
  read_contas()

## acordos
# read acordos file
system.file("extdata/acordos.xlsx", package = "tidycondr") |>
  read_contas()
```

### Partition subreports blocks

``` r
## pcontas
# partition pcontas
pcontas <- system.file("extdata/pcontas.xlsx", package = "tidycondr") |>
  read_contas()
partition_contas(pcontas)

## acordos
# partition acordos
acordos <- system.file("extdata/acordos.xlsx", package = "tidycondr") |>
  read_contas()
```

### Do it all at one step

``` r
## pcontas
# partition all pcontas
system.file("extdata/pcontas.xlsx", package = "tidycondr") |>
  read_contas_partitions()
```

### Anonymize excel files

Anonymize character contents, keeping values and selected rows (normaly headers) as is.

```r
system.file("extdata/pcontas.xlsx", package = "tidycondr") |>
  tidyxl::xlsx_cells() |>
  anonymise(
    skip_rows = c(
      2:4, 12:14, 16, 89, 91, 229, 231, 233, 235:237, 252:254,
      264:266, 389,391:400, 402:403, 405:406, 408:410)) |>
  unpivotr::rectify()
```
