
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gghdr

Package `gghdr` imports the package
[`hdrcde`](https://pkg.robjhyndman.com/hdrcde/) and provides tools for
plotting highest density regions in the ggplot2 framework.

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# Traditional boxplot

  - central box bounded by Q1 and Q3 representing the interquartile
    range
  - whiskers extending from Q1 - 1.5(Q3 - Q1) to Q3 + to 1.5(Q3 - Q1),
    representing 99% coverage for large samples
  - median represented by a horizontal line

<!-- end list -->

    #> This is hdrcde 3.3

![](README_files/figure-gfm/boxplot-1.png)<!-- -->

# hdr.boxplot (Existing)

There are different ways to summarize a distribution but the highest
density region allows the display of multimodality.

  - Region bounded by the interquartile range is replaced by 50% HDR
  - Region bounded by the whiskers is replaced by the 99% HDR
  - The mode is represented by a horizontal line

<!-- In both HDR and box plots, the interquartile range or 50% HDR will have a coverage probability of 50%. -->

``` r
library(hdrcde)
hdr.boxplot(faithful$eruptions)
```

![](README_files/figure-gfm/hdrcde-boxplot-1.png)<!-- -->

<!-- # ```{r hdrcde-boxplot_more, echo=TRUE, eval = FALSE} -->

<!-- # hdr.boxplot(x, prob = c(99, 50), h = hdrbw(BoxCox(x, lambda), -->

<!-- #   mean(prob)), lambda = 1, boxlabels = "", col = gray((9:1)/10), -->

<!-- #   main = "", xlab = "", ylab = "", pch = 1, border = 1, -->

<!-- #   outline = TRUE, space = 0.25, ...) -->

<!-- # ``` -->

# geom\_boxplot\_hdr (Proposal)

``` r
library(ggplot2)
library(gghdr)
ggplot(faithful, aes(y = eruptions)) +  
  geom_hdr_boxplot(position = "identity")
```

# hdr.den (Existing)

  - a density plot
  - the endpoints of each interval in each HDR on the x-axis
  - a straight line showing value of the density at the boundaries of
    each HDR

<!-- end list -->

``` r
hdr.den(faithful$eruptions)
```

![](README_files/figure-gfm/hdr.den-1.png)<!-- -->

    #> $hdr
    #>         [,1]     [,2]     [,3]     [,4]
    #> 99% 1.323766 2.819344 3.152257 5.282088
    #> 95% 1.500661 2.520845 3.500000 5.091669
    #> 50% 1.923610 2.024692 3.941872 4.772422
    #> 
    #> $mode
    #> [1] 4.378107
    #> 
    #> $falpha
    #>         1%         5%        50% 
    #> 0.06760665 0.15309392 0.36085651
    #ggplot(faithful, aes(x=eruptions)) +  geom_density()

# gg\_hdr (Proposal)

  - geom\_density() - to represent the density
  - geom\_hdr\_rug() - The endpoints of each interval in each HDR on the
    x-axis
  - geom\_hline() - The value of the density at the boundaries of each
    HDR

gghdr() = geom\_density() + geom\_hdr\_rug() + geom\_hline()

``` r
library(ggplot2)
faithful %>% ggplot(aes(y = , x = )) +  geom_hdr()
```
