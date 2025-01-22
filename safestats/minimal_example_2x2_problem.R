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
safestats::safeTwoProportionsTest(ya, yb, designObj)

# TODO: what is the delta with no restriction?
# same e-value = 22.276


# see calculateSequential2x2E in line 1453
