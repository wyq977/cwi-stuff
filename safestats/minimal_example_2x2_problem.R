library("devtools")
load_all("~/projects/safestats")

set.seed(123)

designObj <- safestats::designSafeTwoProportions(
    na = 1, nb = 1, delta = 0.2, alternativeRestriction = "none", alpha = 0.05,
    beta = 0.2, M = 100
)


ya = rbinom(100, 1, 0.5)
yb = rbinom(100, 1, 0.3)

# es 0.2
safestats::safeTwoProportionsTest(ya, yb, designObj)


# es 0.3
designObj
designObj$delta <- 0.3

# TODO: what is the delta with no restriction?
# same e-value = 22.276


# see calculateSequential2x2E in line 1453


library(microbenchmark)

vec <- 1:100000

simpleFunc <- function(vec) {
  sum <- 0
  for (i in seq_along(vec)) {
    sum <- sum + vec[i]
  }
  return(sum)
}

microbenchmark(
  vectorized = cumsum(vec),  # Vectorized operation
  reduce = Reduce(`+`, vec, accumulate = TRUE),  # Reduce
  forloop = simpleFunc(vec),
  times = 100
)

# this is numerically unsafe
calculateETwoProportions <- function(na1, na, nb1, nb, thetaA, thetaB, theta0){
  (thetaA^na1 * (1-thetaA)^(na - na1) * thetaB^nb1 * (1-thetaB)^(nb - nb1)) /
  (theta0^(na1 + nb1) * (1-theta0)^(na+nb-na1-nb1))
}


# Benchmark between the original approach and new one

set.seed(123)
ya = rbinom(10000, 1, 0.5)
yb = rbinom(10000, 1, 0.499)


load_all("~/projects/safestats")


microbenchmark(
  original = safestats::safeTwoProportionsTest(ya, yb, designObj=designObj),
  new = EValTwoProp(ya, yb),
  times = 10
)

EValTwoProp(ya, yb)
