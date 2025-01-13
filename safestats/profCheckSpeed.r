library("devtools")
load_all("~/projects/safestats")

# check which part slow down 2x2 calculation
library("profvis")

checkLongProf <- function() {
  designSafeTwoProportions(
    na = 1,
    nb = 1,
    alpha = 0.05, #significance level for testing
    beta = 1 - 0.8, #power 80% equals type II error (beta) of 0.2
    delta = 0.3,
    M =100
  )
}


p <- profvis(checkLongProf())


p
print(p)