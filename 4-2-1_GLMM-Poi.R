Sys.setenv(LANGUAGE="en")

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

library(lme4)
yr <- factor(data$year)
glmm.fit <- glmer(C ~ (1|yr) + year + I(year^2) + I(year^3), family = poisson, data = data)

mny <- mean(data$year)
sdy <- sd(data$year)
cov1 <- (data$year - mny) / sdy
cov2 <- cov1 * cov1
cov3 <- cov1 * cov1 * cov1
glmm.fit <- glmer(C ~ (1|yr) + cov1 + cov2 + cov3, family = poisson, data = data)
summary(glm.fit)

R.predictions <- exp(fixef(glmm.fit)[1] + fixef(glmm.fit)[2]*cov1 + fixef(glmm.fit)[3]*cov2 + fixef(glmm.fit)[4]*cov3 + unlist(ranef(glmm.fit)))
lines(data$year, R.predictions, col = "green", lwd = 2, type = "l")

