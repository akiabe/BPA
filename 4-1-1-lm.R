mass <- c(25, 14, 68, 79, 64, 139, 49, 119, 111)
pop <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3))
length <- c(1, 14, 22, 2, 9, 20, 2, 13, 22)
plot(length, mass)

summary(lm <- lm(mass ~ pop-1 + length))
abline(lm$coef[1], lm$coef[4], col = "red", lwd = 3, lty = 2)
abline(lm$coef[2], lm$coef[4], col = "blue", lwd = 3, lty = 2)
abline(lm$coef[3], lm$coef[4], col = "green", lwd = 3, lty = 2)

library(lme4)

summary(lmm <- lmer(mass ~ length + (1|pop)))
ranef(lmm)
abline(fixef(lmm)[1]+ranef(lmm)$pop[1,], fixef(lmm)[2], col = "red", lwd = 3)
abline(fixef(lmm)[1]+ranef(lmm)$pop[2,], fixef(lmm)[2], col = "blue", lwd = 3)
abline(fixef(lmm)[1]+ranef(lmm)$pop[3,], fixef(lmm)[2], col = "green", lwd = 3)
