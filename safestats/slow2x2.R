# Replicate the slow 2x2 calculation in main branch

library("devtools")
load_all("~/projects/safestats")

# simple e-stat calculation without simulation

alpha <- 0.05
deltaMin <- 0.2

designObj <- designSafeT(
  deltaMin = deltaMin,
  alpha = alpha, beta = NULL, alternative = "greater", testType = "paired", seed = 1, pb = FALSE
)

designObj

set.seed(1)
preData <- rnorm(n = 100, mean = 120, sd = 15)
postData <- rnorm(n = 100, mean = 12, sd = 1)
# Thus, the true delta is 0:
# deltaTrue <- (120-120)/(sqrt(2)*15)

safeTTest(
  x = preData, y = postData, alternative = "greater",
  designObj = designObj, paired = TRUE
)


# Problem: failed to create 2x2 table without specifying beta or nPlan
#   Provide two of nBlocksPlan, delta and power, or only nBlocksPlan for a pilot with default settings.
# TODO:


design <- designSafeTwoProportions(
  na = 1,
  nb = 1,
  alpha = 0.05, #significance level for testing
  beta = 1 - 0.8, #power 80% equals type II error (beta) of 0.2
  delta = 0.3
)
print(design)


set.seed(692021)
ya <- rbinom(n = design[["nPlan"]]["nBlocksPlan"], size = 1, prob = 0.3)
yb <- rbinom(n = design[["nPlan"]]["nBlocksPlan"], size = 1, prob = 0.3)

# 
safeTwoProportionsTest(
  ya = ya, yb = yb,
  designObj = design
)


## Simple case in Z-test

designObj <- designSafeZ(meanDiffMin=0.4)
print(designObj)

#         Safe One Sample Z-Test Design

#          minimal mean difference = 0.4
#                      alternative = twoSided
#                  parameter: phiS = 0.4
#                            alpha = 0.05
# decision rule: e-value > 1/alpha = 20

# Timestamp: 2025-01-13 15:11:10 CET

# simply calculate the e-value for 2x2
# na = nb = 1

set.seed(1)
ya <- rbinom(n = 1000, size = 1, prob = 0.3)
yb <- rbinom(n = 1000, size = 1, prob = 0.1)


safeTwoProportionsTest(
    ya = ya, yb = yb,
    designObj = NULL,
    pilot = TRUE
)
