# tidycondr

<!-- badges: start -->
<!-- badges: end -->

[![en](https://img.shields.io/badge/lang-en-red.svg)](https://github.com/diegomsg/tidycondr/blob/master/README.md)
[![pt-br](https://img.shields.io/badge/lang-pt--br-green.svg)](https://github.com/diegomsg/tidycondr/blob/master/README.pt-br.md)

O objetivo do tidycondr é fornecer ferramentas para ler dados de relatórios financeiros de gestão de condomínio da *Superlogica*, transformando arquivos de dados `xlsx` bagunçados em dados organizados.

## Superlogica

É um provedor conhecido de serviços de gestão de condomínios, especialmente de dados financeiros para prestação de contas.

## Instalação

Você pode instalar a versão de desenvolvimento do tidycondr a partir do [GitHub](https://github.com/) com:

``` r
library(tidycondr)
## basic example code
```

## Exemplos

Essse é um exemplo básico para "tidy up" arquivos dos sistemas superlógica:

### Carregar biblioteca

``` r
# carregar biblioteca
library(tidycondr)
```
### Carregar conteúdo dos arquivos célula a célula com pacote `{tidyxl}`

``` r
## pcontas
# lê arquivo pcontas
read_contas("data_raw/pcontas.xlsx")

## acordos
# lê arquivo acordos
read_contas("data_raw/acordos.xlsx")
```

### Subdivide em blocos de subrelatórios

``` r
## pcontas
# partition pcontas
pcontas <- read_contas("data_raw/pcontas.xlsx")
partition_contas(pcontas)

## acordos
# partition acordos
acordos <- read_contas("data_raw/acordos.xlsx")
```

### Faça tudo de uma só vez

``` r
## pcontas
# partition all pcontas
read_contas_partitions("data_raw/pcontas.xlsx")
```

### Anonimização - mascara dados

Anonimiza conteúdo textual, mantendo dados numéricos, datas e linhas slecionadass (normalmente cabeçalhos) como estão.

```r
tidyxl::xlsx_cells("data_raw/pcontas.xlsx") |>
  anonymise(
    skip_rows = c(
      2:4, 12:14, 16, 89, 91, 229, 231, 233, 235:237, 252:254,
      264:266, 389,391:400, 402:403, 405:406, 408:410)) |>
  unpivotr::rectify()
```
