# safestats

Build R Step by Step: <https://r-pkgs.org/testing-design.html>

C code and R: https://jcarroll.com.au/2023/08/11/wrapping-c-code-in-an-r-package/

The build status on CRAN: <https://cran.r-project.org/web/checks/check_results_safestats.html>

Test coverage (Probably not important...)

```         
safestats Coverage: 16.46%
R/safe2x2Test.R: 0.00%
R/tTest.R: 6.22%
R/safeS3Methods.R: 10.38%
R/zTest.R: 21.52%
R/helpers.R: 26.15%
R/logRankTest.R: 50.67%
```

## Load packages

Provided the repo is cloned.

``` r
library("devtools")

# load the package
load_all()
```

## How to get to the calculation of e-variable in Turner's paper

Originally inspired by simulation where the posterior concentrates around
1 solution which has nice format.

Later in version 1, the solution is calculated by solving the argmin of KL
divergence. Not just convex hull.

However, the corollary 1 of Theorem 1 of safe testing paper provides a shorter
and simper solution that if we find a likelihood ratio form e value then it must 
be GRO. But this is not necessarily true posterior scenarios.

In Bayesian posterior settings, the parameters are learnt: TODO.

## Slow in contingency table calculation

**Goal (short term)**: To realize a design table without simulation

Now every time it runs, it still required the power of the test theta to be 
input and calculating the minimal blocks to achieve that.

The idea is to separate that part of `simulation` from the main function, 
probably due to the fact running worst case for "nPlan". That is for design 
and planning, Most users would just need to calculate the e-value hassle-free
way.

**Goal (long term)**: To have a meta function similar to "eval()"

``` r
library(profvis)

checkLong <- designSafeTwoProportions(
  na = 1,
  nb = 1,
  alpha = 0.05, #significance level for testing
  beta = 1 - 0.8, #power 80% equals type II error (beta) of 0.2
  delta = 0.3
)
```

## Compare the design safe for t test with or without power (beta)

* why remove safetest in 0.9.0? Ask Alexandar
* 


## calculateSequential2x2E

In the simpliest settings, `pilot = TRUE`. No restriction was placed on Ha.

What is the eVariable??

In the for loop (`seq_along(aSample)`), e value is calculated and updated with
`updateETwoProportions` (Eq. 4.2 in Turner et al. paper)


```math
\theta_a^* = \frac{n_{a1} + prior_{a1}}{n_a + prior_{a}}
```

TODO: speed up the calculation by switch?


