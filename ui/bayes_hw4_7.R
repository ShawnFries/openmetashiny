library(Rmpfr)
library(numDeriv)

y <- mpfr(c(118, 74, 44, 24, 29, 22, 20, 14, 20, 15, 12, 14, 6, 12, 6, 9, 9, 6, 10, 10, 11, 5, 3, 3), 1024)
y_expanded <- integer()
y_length <- length(y)

alpha <- seq(0.1, 10, length=1000)
beta <- seq(0.1, 10, length=1000)
alpha <- sample(alpha, 1000)
beta <- sample(beta, 1000)
multinomial_probabilities <- numeric(1000)
for (i in 1:1000) {
  multinomial_probabilities[i] <-
    dmultinom(y,
              prob=mpfr(sapply(1:y_length, function(x) (1 + 1 / beta[i])^(x + alpha[i]) / (beta[i]^alpha[i] * gamma(x + alpha[i]) * gamma(alpha[i]) * factorial(x))), 1024)
             )
}
multinomial_probabilities <- sapplyMpfr(multinomial_probabilities, function(x) x) # Get values from mpfr list
posterior_mode <- which.max(multinomial_probabilities)
alpha_mode <- alpha[posterior_mode]
beta_mode <- beta[posterior_mode]
print(alpha_mode)
print(beta_mode)

density <- function(params) {
  dmultinom(y,
            prob=sapply(1:y_length, function(x) (1 + 1 / params[2])^(x + params[1]) / (params[2]^params[1] * gamma(x + params[1]) * gamma(params[1]) * factorial(x)))
           )
}
print(hessian(density, c(alpha_mode, beta_mode)))

x <- rep(0, 1000)
probs <- sapply(1:100, function(x) (1 + 1 / beta[i])^(x + alpha_mode) / (beta_mode^alpha_mode * gamma(x + alpha_mode) * gamma(alpha_mode) * factorial(x)))
for (i in 1:1000) {
  sum <- 0
  while (sum < 10000) {
    sum <- sum + sample.int(100, 1, prob=probs)  # Doesn't matter if we replace or not, only drawing one item at a time
    x[i] <- x[i] + 1
  }
}
print(mean(x))
sorted_x <- sort(x)
print(sorted_x[26])  # 95% lower bound
print(sorted_x[975]) # 95% upper bound