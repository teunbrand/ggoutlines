
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggoutlines

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of ggoutlines is to extend ggplot2 by providing outlines to
popular geometry layers and theme elements.

## Installation

You can install the development version of ggoutlines from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("teunbrand/ggoutlines")
```

## Example

The idea here is that there are variants of classic ggplot2 functions
that draw the same thing, but with outlines.

``` r
library(ggoutlines)
#> Loading required package: ggplot2

ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_path_outline(
    stroke_colour = "white",
    stroke_linewidth = 2
  )
```

<img src="man/figures/README-example-1.png" width="100%" />

Some extra attention is paid to the transparency settings, ensuring that
everything works ‘as expected’ with semi-transparent colours.

``` r
df <- data.frame(
  x = c(0.33, 0.66),
  y = c(0.66, 0.33),
  lab = c("Hello there,", "README reader")
)

ggplot(df, aes(x, y, label = lab, colour = lab)) +
  geom_text_outline(
    stroke_colour = "black",
    stroke_linewidth = 3, alpha = 0.3,
    size = 20, hjust = c(0, 1),
    show.legend = FALSE
  ) +
  ylim(c(-1, 2))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Missing

Here is a list of missing functionality:

-   The `linetype` for lines is not properly implemented for the
    outlines.

## Related packages

Some packages that implement similar functionality as {ggoutlines} are
[shadowtext](https://github.com/GuangchuangYu/shadowtext) and
[ggtrace](https://github.com/rnabioco/ggtrace).
