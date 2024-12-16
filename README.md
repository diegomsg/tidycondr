
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
read_contas("data_raw/pcontas.xlsx")

## acordos
# read acordos file
read_contas("data_raw/acordos.xlsx")
```

### Partition subreports blocks

``` r
## pcontas
# partition pcontas
pcontas <- read_contas("data_raw/pcontas.xlsx")
partition_contas(pcontas)

## acordos
# partition acordos
acordos <- read_contas("data_raw/acordos.xlsx")
```

### Do it all at one step

``` r
## pcontas
# partition all pcontas
read_contas_partitions("data_raw/pcontas.xlsx")
```

### Anonymize excel files

Anonymize character contents, keeping values and selected rows (normaly headers) as is.

```r
tidyxl::xlsx_cells("data_raw/pcontas.xlsx") |>
  anonymise(
    skip_rows = c(
      2:4, 12:14, 16, 89, 91, 229, 231, 233, 235:237, 252:254,
      264:266, 389,391:400, 402:403, 405:406, 408:410)) |>
  unpivotr::rectify()
```
