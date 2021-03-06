
<!-- README.md is generated from README.Rmd. Please edit that file -->

# door <img src="man/figures/logo-door.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Brief Plan (Document)

[google
docs](https://docs.google.com/document/d/1spvHMKAeZqtVlwB3mpOnxBor37mqt6d_-EJyUbdJRqM/edit)

## Brief Plan (UI Design)

![Import plan
001](https://user-images.githubusercontent.com/6457691/169475152-de090c90-7150-46e3-a616-c32f9529c39a.png)

![Import plan
002](https://user-images.githubusercontent.com/6457691/169475165-51271dff-bef6-419e-bd00-146a39bf0505.png)

![Import plan
003](https://user-images.githubusercontent.com/6457691/169475168-ff9ac032-e2b4-4653-bb90-6b5b86949872.png)

![Import plan
004](https://user-images.githubusercontent.com/6457691/169475172-27c9ef35-54b3-45f0-9002-e2a80849bf97.png)

![Import plan
005](https://user-images.githubusercontent.com/6457691/169475175-14d48d4c-ff94-4108-a238-4fc0ac515fd3.png)

![Import plan
006](https://user-images.githubusercontent.com/6457691/169475178-a997c22c-c5c7-4e25-8505-2df08dd9085b.png)

![Import plan
007](https://user-images.githubusercontent.com/6457691/169475180-e521721a-2cf9-4142-90ce-18fab92f994e.png)

![Import plan
008](https://user-images.githubusercontent.com/6457691/169475181-19c526da-b0f5-4200-aea0-0460ec51928a.png)

The goal of door is to …

## Installation

You can install the development version of door from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("statgarten/door")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(door)
## basic example code
door::run_app()
#> Loading required package: shiny
#> 
#> Listening on http://127.0.0.1:7386
```

<img src="man/figures/README-example-1.png" width="100%" />

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.
