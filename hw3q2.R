library(boot)
set.seed(23)
head(aircondit)
# using the diff function to generate the differences
times <- diff(aircondit$hours)
print(times)
# the MLE of of lambda is the sample mean
lambda_hat <-  1 / mean(times)
# bootstrap trials
b <- 2000
lambda_hat_star <- numeric(b)
for (i in 1:b) {
  x_star <- sample(times, length(times), replace = TRUE)
  lambda_hat_star[i] <-  1 / mean(x_star)
}
cat("The bias is given by ", mean(lambda_hat_star) - lambda_hat,
    " the difference between the emprical and bootstrapped mean\n",
    " and the standard error is the sample standard deviation of",
    " the bootstrap estimate", sd(lambda_hat_star), "\n")
hist(lambda_hat_star, probability = TRUE,
     main = "Bootstrap density vs Exp(lambda_hat)")
# density curve
y <- function(x) {
  dexp(x, 1 / lambda_hat)
}
curve(y(x), col = 2, lwd = 2, add = TRUE)
dev.off()
# question 3
# defining out  1 / lambda
times_i <- as.matrix(times)
lambda_boot <- function(dat, ind) {
  x <- times_i[ind, 1]
  1 / mean(x)
}
boot_obj <- boot(times, statistic = lambda_boot, R = 2000)
print(boot_obj)
print(boot.ci(boot_obj,
              type = c("basic", "norm", "perc")))
