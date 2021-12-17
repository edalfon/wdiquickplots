
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wdiquickplots

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/edalfon/wdiquickplots/branch/master/graph/badge.svg)](https://app.codecov.io/gh/edalfon/wdiquickplots?branch=master)
<!-- badges: end -->

The goal of `wdiquickplots` is to provide, well, quick plots for World
Development Indicators (WDI, [“the primary World Bank collection of
development indicators, compiled from officially recognized
international sources.”](https://databank.worldbank.org/home.aspx)).
Just jump to the “Get started” page if you want to take a look at all
the quick plots examples. To get WDI data, this package is powered by
[`WDI`](http://vincentarelbundock.github.io/WDI/) package, developed by
Vincent Arel-Bundock.s

## Installation

You can install it from this Github repo with:

``` r
remotes::install_github("edalfon/wdiquickplots")
```

## How to use it

Use case: hey I have to present this study I have been working on in my
home country to an audience where I currently live (studying abroad or
whatever). Thus, some background data on my home country is in order. A
table would certainly do, but it is boring. So let’s put some plots in
there.

``` r
library(wdiquickplots)
plot_dist_wdi("NY.GDP.PCAP.PP.CD", p = 0)
```

<img src="man/figures/README-dist-1.svg" width="47%" style="display: block; margin: auto;" />

There you go. That’s the spirit of this package. One line of code and
bang!, a relatively decent plot that you can put in your slides to
convey a quick message.

Using this package goes as follows:

-   Find the code of the indicator of interest. You can use
    `WDI::WDIsearch` for this, but I actually find it a bit more
    user-friendly to simply go to the [indicators page
    (https://data.worldbank.org/indicator)](https://data.worldbank.org/indicator)
    and get the code from there (it’s in the URL).
-   You pass the indicator code as the first argument of the different
    plotting functions in this package.
-   As second argument, you pass the countries you want to highlight.

And that’s it.

You can read other details and description of features [in the `pkgdown`
site for this little package](https://edalfon.github.io/wdiquickplots/)
(I know, a `pkgdown` site may be overkill, but anyway). There you can
also see examples for all the quick plots in this package, but in
general, they quickly show:

-   Where the highlighted countries stand in terms of the indicator of
    interest.
-   How do they compare among highlighted countries, and against the
    rest of the world, regions or income groups.
-   What have been the changes in time.
