
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parsenetwork

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of parsenetwork is to …

## Installation

You can install the released version of parsenetwork from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("parsenetwork")
```

Or install the development version from GitHub with:

``` r
install.packages("remotes")
remotes::install_github("celehs/parsenetwork")
```

## Usage

### Build the database

``` r
library(kesernetwork)
# required steps
data2db("edge", "path/to/db", "path/to/datafile")
data2db("dict_cui", "path/to/db", "path/to/datafile")
data2db("dict_codified", "path/to/db", "path/to/datafile")
# optional steps
data2db("synonyms", "path/to/db", "path/to/datafile")
data2db("rollup", "path/to/db", "path/to/datafile")
data2db("table1", "path/to/db", "path/to/datafile", title = "the title of the data")
```

### Run app

This is a basic example which shows you how to run the `parsenetwork`
app. Remember you need to get access to the data and save it to your
local computer. In order to guarantee some dependencies are loaded, you
must use `library(parsenetwork)` beforehand, instead of directly running
`parsenetwork::run_app()`.

``` r
library(parsenetwork)
run_app(db_path = "path/to/db")
```

See the [getting started
guide](https://celehs.github.io/parsenetwork/articles/main.html) to
learn how to use parsenetwork.
