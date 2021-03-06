
<!-- README.md is generated from README.Rmd. Please edit that file -->
ntsar <img src="man/figures/logo.png" align="right" alt="" width="360" />
=========================================================================

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/bklimowski/ntsar.svg?branch=master)](https://travis-ci.org/bklimowski/tsar) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Codecov test coverage](https://codecov.io/gh/bklimowski/ntsar/branch/master/graph/badge.svg)](https://codecov.io/gh/bklimowski/tsar?branch=master) <!-- badges: end -->

**N**onlinear **T**ime **S**eries **A**nalysis with **R** is package dedicated to provide tools derived from dynamical system theory for purpose of analysis nonlinear properties of time series. Developed version will contain things like:

-   Estimation of embedding dimension and time lag
-   Attractor reconstruction with machine learning
-   Complex network approaches - visibility graph, transfer and recourrence networks
-   Measures of fractal dimension, entropy production and irreversibility

Installation
------------

``` r
devtools::install_github("bklimowski/ntsar")
```

Example
-------

Creation of [recurrence matrix](https://en.wikipedia.org/wiki/Recurrence_plot) from time series.

``` r
library(ntsar)
set.seed(448)
n = 500
periodic_series <- (sin(1:n/16) + cos(1:n/10)/2 + tan(1:n/20)) 

plot(periodic_series, type = 'l')
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

recurrence_matrix(periodic_series, 0.5) %>% 
  image(col=gray.colors(2, start = 1, end = 0),
        xaxt='n',
        yaxt='n')
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />
