styler::style_file("./safestats/test.R")
set.seed(123)

ya <- rbinom(100, 1, 0.5)
yb <- rbinom(100, 1, 0.3)

shape1 <- shape2 <- 0.18

esMin <- 0.2

# After
gridSize <- 1e3
K <- 1 / gridSize # precision K
rhoGrid <- seq(K / (1 - esMin), 1 - K, length.out = gridSize)
# rhoGridDensity <- stats::dbeta(x = rhoGrid, shape1. shape2)

# prepare prob. grid = rhoGrid * (1 - esMin)
thetaAgrid <- rhoGrid * (1 - esMin)

# prior density for thetaA at each grid points with normalization
thetaAgridDensity <- stats::dbeta(thetaAgrid, shape1, shape2)
thetaAgridDensity <- thetaAgridDensity / sum(thetaAgridDensity)

sum(thetaAgridDensity)


likelihoodBernoulli <- function(x, theta) {
  # Use log and exp to avoid underflow/overflow
  loglikelihood <- x * log(theta) + (1 - x) * log(1 - theta)
  return(exp(loglikelihood))
}

# prior times likelihood

likelihoodTimesPrior <- rep(0, length(thetaAgrid))

x <- 1

# Vectorized computation
likelihoodTimesPrior <- likelihoodBernoulli(x, thetaAgrid) * thetaAgridDensity

# Normalize to compute posterior density
posteriorDensity <- likelihoodTimesPrior / sum(likelihoodTimesPrior)

# prior for theta
# FIXME: why not 0.5!!!
thetaAPrior <- thetaAgrid %*% thetaAgridDensity
thetaAPrior

# posterior for theta
thetaAPosterior <- thetaAgrid %*% posteriorDensity

plot(rhoGrid,
  thetaAgridDensity,
  type = "l", col = "blue", lwd = 2, xlab = "thetaA", ylab = "Density",
  main = "Density for theta"
)
abline(v = thetaAPrior, col = "blue")

lines(rhoGrid,
  posteriorDensity,
  type = "l", col = "red", lwd = 2, xlab = "thetaA", ylab = "Density",
)
abline(v = thetaAPosterior, col = "red")
legend("topleft",
  legend = c("Posterior", "Prior"),
  col = c("red", "blue"), lty = 1:2, cex = 0.8
)
