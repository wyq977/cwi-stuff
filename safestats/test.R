set.seed(123)

nData <- 1e2
trueThetaA <- 0.5
trueThetaB <- 0.3
na <- nb <- 1
gridSize <- 1e3
K <- 1 / gridSize # precision K
shape1 <- shape2 <- 0.18
esMin <- 0.2 # delta, assmued to be between (0, 1)

ya <- rbinom(nData, na, trueThetaA)
yb <- rbinom(nData, nb, trueThetaB)

rhoGrid <- seq(K / (1 - esMin), 1 - K, length.out = gridSize)
# rhoGridDensity <- stats::dbeta(x = rhoGrid, shape1, shape2)

# prepare prob. grid = rhoGrid * (1 - esMin)
thetaAgrid <- rhoGrid * (1 - esMin)

# prior density for thetaA at each grid points with normalization
# rho = thetaA / (1 - esMin) ~ Beta(shape1, shape2)
thetaADensity <- stats::dbeta(rhoGrid, shape1, shape2)
thetaADensity <- thetaADensity / sum(thetaADensity)

likelihoodBernoulli <- function(x, theta) {
  # Use log and exp to avoid underflow/overflow
  loglikelihood <- x * log(theta) + (1 - x) * log(1 - theta)
  return(exp(loglikelihood))
}

thetaB <- thetaA <- rep(0.5, length(ya))

for (i in 1:length(ya)) {
  # thetaADensity is the prior density, gets updated at each iteration
  # calculate likelihood times prior
  likelihoodTimesPrior <- likelihoodBernoulli(ya[i], thetaAgrid) * thetaADensity

  # update posterior density
  thetaADensity <- likelihoodTimesPrior / sum(likelihoodTimesPrior)

  # prediction for theta
  thetaA[i] <- thetaAgrid %*% thetaADensity
}


thetaB <- thetaA + esMin


plot(seq(length(ya)), thetaA, col = "blue", ylim = c(0, 1))
points(seq(length(ya)), thetaB, col = "red")
abline(h = trueThetaA)

