data.fn <- function(n = 40, alpha = 3.5576, beta1 = -0.0912, beta2 = 0.0091, beta3 = -0.00014, sd = 1) {
  year <- 1:n
  eps <- rnorm(n = n, mean = 0, sd = sd)
  log.expected.count <- alpha + beta1 * year + beta2 * year^2 + beta3 * year^3 + eps
  expected.count <- exp(log.expected.count)
  C <- rpois(n = n, lambda = expected.count)
  plot(year, C, type = "b", lwd = 2, main = "", las = 1, ylab = "Size", xlab = "Year", ylim = c(0, 1.1*max(C)))
  lines(year, expected.count, type = "l", lwd = 3, col = "red")
  return(list(n = n, alpha = alpha, beta1 = beta1, beta2 = beta2, beta3 = beta3, year = year, sd = sd, expected.count = expected.count, C = C))
}

data <- data.fn()


