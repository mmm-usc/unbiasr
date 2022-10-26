
<!-- README.md is generated from README.Rmd. Please edit that file -->

# unbiasr

MMM <Lab@USC>

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of unbiasr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("mmm-usc/unbiasr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(unbiasr)
## Toy example from Millsap & Kwok (2004, doi: 10.1037/1082-989X.9.1.93)
PartInv(
    propsel = .25,
    alpha_r = 0.5,
    alpha_f = 0,
    psi_r = 1,
    lambda_r = c(.3, .5, .9, .7),
    nu_r = c(.225, .025, .010, .240),
    nu_f = c(.225, -.05, .240, -.025),
    Theta_r = diag(.96, 4)
)
#> Proportion selected:  0.25 
#> Cutpoint on the latent scale (xi):  0.946 
#> Cutpoint on the observed scale (Z):  3.182 
#> AI ratio:  0.96 
#> 
#> Classification Accuracy Indices:
#>                     Reference Focal E_R(Focal)
#> True Positive           0.224 0.108      0.219
#> False Positive          0.092 0.076      0.085
#> True Negative           0.580 0.752      0.587
#> False Negative          0.103 0.064      0.109
#> Proportion Selected     0.316 0.184      0.304
#> Success Ratio           0.710 0.587      0.720
#> Sensitivity             0.684 0.627      0.667
#> Specificity             0.863 0.908      0.873
```

## Shiny Application

Browser version: <https://partinvmmm.shinyapps.io/partinvshinyui/>

Or call `unbiasr::launch()` in R.

## References

Millsap, R. E., & Kwok, O.-M. (2004). Evaluating the impact of partial
factorial invariance on selection in two populations. *Psychological
Methods, 9*(1), 93–115. <https://doi.org/10.1037/1082-989X.9.1.93>

Lai, M. H. C., Kwok, O., Yoon, M., & Hsiao, Y.-Y. (2017). Understanding
the impact of partial factorial invariance on selection accuracy: An R
script. *Structural Equation Modeling: A Multidisciplinary Journal,
24*(5), 783–799. <https://doi.org/10.1080/10705511.2017.1318703>

Lai, M. H. C., & Zhang, Y. (2022). Classification accuracy of
multidimensional tests: Quantifying the impact of noninvariance.
*Structural Equation Modeling: A Multidisciplinary Journal.* Advance
online publication. <https://doi.org/10.1080/10705511.2021.1977936>

The development of this package is supported by the U.S. Army Research
Institute for the Behavioral and Social Sciences (ARI) under Grant
W911NF2010282. The views, opinions, and/or findings contained in this
report (paper) are those of the authors and shall not be construed as an
official Department of the Army position, policy, or decision, unless so
designated by other documents.
