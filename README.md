
<!-- README.md is generated from README.Rmd. Please edit that file -->

# typer

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/JSzitas/typer.svg?branch=master)](https://travis-ci.org/JSzitas/typer)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/JSzitas/typer?branch=master&svg=true)](https://ci.appveyor.com/project/JSzitas/typer)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/JSzitas/typer/branch/master/graph/badge.svg)](https://codecov.io/gh/JSzitas/typer?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/typer)](https://CRAN.R-project.org/package=typer)
[![R build
status](https://github.com/JSzitas/typer/workflows/R-CMD-check/badge.svg)](https://github.com/JSzitas/typer/actions)
<!-- badges: end -->

**NOTE: More interesting things coming soon.**

**typer** introduces a new type coercion operator, and fixes some
unexpected behaviour in base R.

## Installation

You can install the stable version of typer from
[CRAN](https://CRAN.R-project.org) with

``` r
install.packages("typer")
```

The latest, development version is also available from
[github](https://github.com/JSzitas/typer) using the *devtools* package

``` r
devtools::install_github("JSzitas/typer")
```

## Introduction

**typer** introduces a new typing operator, **%t%**, and further
simplifies type coercion in R, by fixing some of the unintuitive things
that occur in **R**. Thus it seeks to provide a more intuitive (and
bracket free\!) way to coerce types.

## Examples

Consider the classic problem with as.numeric.

``` r
as.numeric(c(0.7,"0,6","0,5","10"))
# will return NA where 0,6 and 0,5
# though it seems rather unambiguous that those vector elements should be coerced to type numeric
# as_numeric to the rescue

as_numeric(c(0.7,"0,6","0,5","10"))

# it will also give warnings where this is not possible

as_numeric(c(0.7,"0,6","0,5","10","chicken"))

# and it also works as a shorthand for the apply/lapply commands when used with matrices / data.frames / data.tables / lists
as_numeric(list(0.7,"0,6","0,5","10","chicken"))

as_numeric(data.frame(matrix(data = c(0.7,"0,6","0,5","10"), nrow = 2)))
```

There are similar methods for coercing to type character and type
factor.

There is also a new operator\!

``` r
# the package namesake, typer
A <- c(0.7,"0,6","0,5","10")
A %t% "num"
A %t% "numeric"
# typer accepts both a character string, as well as its shorthand

# works by default with the functions for coercion defined within itself (where possible)
A %t% "char"

# and works with user defined variables as shorthands, ie: 
nm <- "numeric"

A %t% nm
```

Happy typing\!

## Contributing

If you would like to contribute a pull request, please do contribute\!
All contributions will be considered for acceptance, provided they are
justifiable and the code is reasonable, regardless of anything related
to the person submitting the pull request. Please keep things civil -
there is no need for negativity. However, this package is intended to be
completely dependency free. Thus, please do keep the that in mind when
submitting pull requests.
