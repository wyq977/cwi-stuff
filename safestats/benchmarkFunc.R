computeTheta <- function(X, Y, thetaA1, thetaA2, thetaB1, thetaB2, na = 1, nb = 1) {
  # Compute cumulative successes and failures
  totalSuccessA <- cumsum(X)
  totalSuccessB <- cumsum(Y)
  totalFailA <- seq_along(X) - totalSuccessA
  totalFailB <- seq_along(Y) - totalSuccessB

  # Calculate thetaA and thetaB (vectorized)
  thetaA <- (totalSuccessA + thetaA1) / (totalSuccessA + totalFailA + thetaA1 + thetaA2)
  thetaB <- (totalSuccessB + thetaB1) / (totalSuccessB + totalFailB + thetaB1 + thetaB2)

  # Calculate theta0 (vectorized)
  theta0 <- (na * thetaA + nb * thetaB) / (na + nb)

  # Return results as a list
  return(list(thetaA = thetaA, thetaB = thetaB, theta0 = theta0))
}

computeThetaForLoopOptimized <- function(X, Y, thetaA1, thetaA2, thetaB1, thetaB2, na = 1, nb = 1) {
  n <- length(X)
  thetaA <- numeric(n)
  thetaB <- numeric(n)
  theta0 <- numeric(n)

  # Precompute cumulative successes and failures
  totalSuccessA <- cumsum(X)
  totalSuccessB <- cumsum(Y)
  totalFailA <- seq_along(X) - totalSuccessA
  totalFailB <- seq_along(Y) - totalSuccessB

  for (i in seq_len(n)) {
    # Compute posterior thetas
    thetaA[i] <- (totalSuccessA[i] + thetaA1) / (totalSuccessA[i] + totalFailA[i] + thetaA1 + thetaA2)
    thetaB[i] <- (totalSuccessB[i] + thetaB1) / (totalSuccessB[i] + totalFailB[i] + thetaB1 + thetaB2)
    theta0[i] <- (na * thetaA[i] + nb * thetaB[i]) / (na + nb)
  }

  return(list(thetaA = thetaA, thetaB = thetaB, theta0 = theta0))
}




library(microbenchmark)

# Example inputs
X <- sample(0:1, 1e5, replace = TRUE)
Y <- sample(0:1, 1e5, replace = TRUE)
thetaA1 <- thetaA2 <- thetaB1 <- thetaB2 <- 0.5

# Benchmark the optimized for-loop vs vectorized approach
microbenchmark(
  vectorized = computeTheta(X, Y, thetaA1, thetaA2, thetaB1, thetaB2),
  for_loop_optimized = computeThetaForLoopOptimized(X, Y, thetaA1, thetaA2, thetaB1, thetaB2),
  times = 10
)


# Example inputs
X <- c(1, 0, 1, 1, 0)
Y <- c(0, 1, 0, 1, 1)
thetaA1 <- 0.5
thetaA2 <- 0.5
thetaB1 <- 0.5
thetaB2 <- 0.5

# Compute using both approaches
result_vec <- computeTheta(X, Y, thetaA1, thetaA2, thetaB1, thetaB2)
result_loop <- computeThetaForLoop(X, Y, thetaA1, thetaA2, thetaB1, thetaB2)

# Compare results
print("Vectorized thetaA:")
print(result_vec$thetaA)

print("For-Loop thetaA:")
print(result_loop$thetaA)

print("Vectorized thetaB:")
print(result_vec$thetaB)

print("For-Loop thetaB:")
print(result_loop$thetaB)

print("Vectorized theta0:")
print(result_vec$theta0)

print("For-Loop theta0:")
print(result_loop$theta0)

