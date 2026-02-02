
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
    alpha = list(0.5,0), psi = list(1,1),
    lambda = list(c(.3, .5, .9, .7), c(.3, .5, .9, .7)),
    nu = list(c(.225, .025, .010, .240), c(.225, -.05, .240, -.025)),
    theta = list(diag(.96, 4), diag(.96, 4))
)
#> Mixing proportions not provided (pmix). Assuming equal weights.
#> Partial invariance results:
#> 
#> Proportion selected:  0.25 
#> Cutpoint on the latent scale (xi):  0.946 
#> Cutpoint on the observed scale (Z):  3.18 
#> Adverse impact ratio (reference group: 'Reference'):
#> Focal_1 
#>    0.96 
#> 
#> Classification Accuracy Indices:
#>                     Reference Focal_1 E_R(Focal_1)
#> True Positive            .224    .108         .219
#> False Positive           .092    .076         .085
#> True Negative            .580    .752         .587
#> False Negative           .103    .064         .109
#> Proportion Selected      .316    .184         .304
#> Success Ratio            .710    .587         .720
#> Sensitivity              .684    .627         .667
#> Specificity              .863    .908         .873

## Deprecated:
# PartInv(
#    propsel = .25,
#    alpha_r = 0.5,
#    alpha_f = 0,
#    psi_r = 1,
#    lambda_r = c(.3, .5, .9, .7),
#    nu_r = c(.225, .025, .010, .240),
#    nu_f = c(.225, -.05, .240, -.025),
#    Theta_r = diag(.96, 4)
#)
```

## Shiny Application

Browser version: <https://mmmlabusc.shinyapps.io/partinvshinyui/>

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
*Structural Equation Modeling: A Multidisciplinary Journal.* 29(4),
620–629. <https://doi.org/10.1080/10705511.2021.1977936>

Ozcan, M., Lai, M. H. C. (2025) Exploring the Impact of Deleting (or
Retaining) a Biased Item: A Procedure Based on Classification Accuracy.
*Assessment*. 32(8), 1211-1225.
<https://doi.org/10.1177/10731911241298081>

The development of this package is supported by the U.S. Army Research
Institute for the Behavioral and Social Sciences (ARI) under Grant
W911NF2010282. The views, opinions, and/or findings contained in this
report (paper) are those of the authors and shall not be construed as an
official Department of the Army position, policy, or decision, unless so
designated by other documents.
