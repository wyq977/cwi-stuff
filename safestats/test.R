set.seed(123)

likelihoodBernoulli <- function(n, x, theta) {
  # Use log and exp to avoid underflow/overflow
  return(x * log(theta) + (n - x) * log(1 - theta))
}

calculateThetaBFromThetaAAndLOR <- function(thetaA, lOR) {
  c <- exp(lOR) * thetaA / (1 - thetaA)
  return(c / (1 + c))
}

nData <- 150
esMin <- 0.2 # delta, assmued to be between (0, 1)


trueThetaA <- 0.5
trueThetaB <- trueThetaA + esMin
na <- nb <- 1
gridSize <- 1e2
K <- 1 / gridSize # precision K
shape1 <- shape2 <- 0.18

ya <- rbinom(nData, na, trueThetaA)
yb <- rbinom(nData, nb, trueThetaB)

testAbsDiff <- function() {
  rhoGrid <- seq(K / (1 - esMin), 1 - K, length.out = gridSize)

  # prepare prob. grid = rho x (1 - esMin)
  thetaAgrid <- rhoGrid * (1 - esMin)
  thetaBgrid <- thetaAgrid + esMin

  # prior density for thetaA at each grid points with normalization
  # rho = thetaA / (1 - esMin) ~ Beta(shape1, shape2)
  thetaADensity <- stats::dbeta(rhoGrid, shape1, shape2)
  thetaADensity <- thetaADensity / sum(thetaADensity)

  # FIXME: initial theta should be 0.5 or what?
  thetaB <- thetaA <- rep(0.5, length(ya))

  for (i in 1:nData) {
    # Compute log-likelihoods for thetaA and thetaB
    logLikelihoodA <- likelihoodBernoulli(na, ya[i], thetaAgrid)
    logLikelihoodB <- likelihoodBernoulli(nb, yb[i], thetaBgrid)

    # Compute posterior in log-space for stability
    logPosterior <- logLikelihoodA + logLikelihoodB + log(thetaADensity)

    # Convert back to probability space
    posteriorDensity <- exp(logPosterior)

    # Normalize to update thetaADensity
    thetaADensity <- posteriorDensity / sum(posteriorDensity)

    # Compute expectation of thetaA (posterior mean)
    thetaA[i] <- thetaAgrid %*% thetaADensity
  }



  thetaB <- thetaA + esMin

  png(file = "testAbsDiff.png", width = 600, height = 600)
  plot(seq_along(ya), thetaA,
    col = "blue", ylim = c(0, 1),
    main = "Absolute Difference"
  )
  points(seq_along(ya), thetaB, col = "red")
  abline(h = trueThetaA)
  abline(h = trueThetaB)
  text(x = length(ya) * 0.8, y = trueThetaA, labels = "True ThetaA", pos = 3, col = "black")
  text(x = length(ya) * 0.8, y = trueThetaB, labels = "True ThetaB", pos = 3, col = "black")

  legend("topleft",
    legend = c("thetaA", "thetaB"),
    col = c("blue", "red"), lty = 1:2
  )
  dev.off()
}


testLogOddsRatioDiff <- function() {
  rhoGrid <- seq(K / (1 - esMin), 1 - K, length.out = gridSize)
  thetaAgrid <- rhoGrid

  thetaBgrid <- sapply(rhoGrid, FUN = calculateThetaBFromThetaAAndLOR, lOR = esMin)

  thetaADensity <- stats::dbeta(rhoGrid, shape1, shape2)
  thetaADensity <- thetaADensity / sum(thetaADensity)

  thetaA <- rep(0.5, length(ya))

  for (i in 1:nData) {
    # Compute log-likelihoods for thetaA and thetaB
    logLikelihoodA <- likelihoodBernoulli(na, ya[i], thetaAgrid)
    logLikelihoodB <- likelihoodBernoulli(nb, yb[i], thetaBgrid)

    # Compute posterior in log-space for stability
    logPosterior <- logLikelihoodA + logLikelihoodB + log(thetaADensity)

    # Convert back to probability space
    posteriorDensity <- exp(logPosterior)

    # Normalize to update thetaADensity
    thetaADensity <- posteriorDensity / sum(posteriorDensity)

    # Compute expectation of thetaA (posterior mean)
    thetaA[i] <- thetaAgrid %*% thetaADensity
  }


  thetaB <- sapply(thetaA, FUN = calculateThetaBFromThetaAAndLOR, lOR = esMin)


  png(file = "testLogOddsRatioDiff.png", width = 600, height = 600)
  plot(seq_along(ya), thetaA,
    col = "blue", ylim = c(0, 1),
    main = "Log Odds Ratio Difference"
  )
  points(seq_along(ya), thetaB, col = "red")
  abline(h = trueThetaA)
  abline(h = trueThetaB)
  text(x = length(ya) * 0.8, y = trueThetaA, labels = "True ThetaA", pos = 3, col = "black")
  text(x = length(ya) * 0.8, y = trueThetaB, labels = "True ThetaB", pos = 3, col = "black")

  legend("topleft",
    legend = c("thetaA", "thetaB"),
    col = c("blue", "red"), lty = 1:2
  )
  dev.off()
}


calculateThetaBFromThetaAAndLOR(trueThetaA, esMin)


testAbsDiff()

testLogOddsRatioDiff()
