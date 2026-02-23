# Question 1
# using the boot package
library(boot)
# making a normal sample
set.seed(23)
n <- 100
x <- rnorm(n)
# function to calculate alpha = 0.05 intervals

intervals <- function(x) {
  alpha <- 0.05
  theta_hat <- mean(x)
  se_theta_hat <- sd(x)
  b <- 2000
  theta_hat_star <- numeric(b)
  # generating the monte carlo sample
  for (i in 1:b) {
    x_star <- sample(x, n, replace = TRUE)
    theta_hat_star[i] <- mean(x_star)
  }
  # calculating normal interval
  zed <- c(-qnorm(1 - alpha / 2), qnorm(1 - alpha / 2))
  test <- mean(theta_hat_star) + sd(theta_hat_star) * zed
  cat("\nNormal Test for the invervals ", test,
      "\nSample mean ", theta_hat,
      "\nStandard Error ", se_theta_hat,
      "\nStandard Error theta* ", sd(theta_hat_star))
  # calculating basic interval
  lims <- c(quantile(theta_hat_star, 1 - alpha / 2),
            quantile(theta_hat_star, alpha / 2))
  test_b <- 2 * theta_hat - lims
  cat("\nBasic Test for the invervals ", test_b,
      "\nSample mean ", theta_hat,
      "\nStandard Error ", se_theta_hat,
      "\nStandard Error theta* ", sd(theta_hat_star))
  # calculating percentile interval
  lims_p <- quantile(theta_hat_star, c(alpha / 2, 1 - alpha / 2), type = 6)
  test_p <- lims_p
  cat("\nPercentile Test for the invervals ", test_p,
      "\nSample mean ", theta_hat,
      "\nStandard Error ", se_theta_hat,
      "\nStandard Error theta* ", sd(theta_hat_star))

  # generating the plot
  limsx <- seq(test[1], test[2], len = 10)
  plot(density(theta_hat_star), main = "Bootstrap Intervals")
  # normal
  lines(limsx, rep(0, length(limsx)),      lty = "dashed", col = 4)
  lines(rep(test[1], length(limsx)), limsx, lty = "dashed", col = 4)
  lines(rep(test[2], length(limsx)), limsx, lty = "dashed", col = 4)
  # basic
  limsx_b <- seq(test_b[1], test_b[2], len = 10)
  lines(limsx_b, rep(0.25, length(limsx_b)),
        lty = "dashed", col = 3)
  lines(rep(test_b[1], length(limsx_b)), 0.25 + limsx,
        lty = "dashed", col = 3)
  lines(rep(test_b[2], length(limsx_b)), 0.25 + limsx_b,
        lty = "dashed", col = 3)
  # percentile
  limsx_p <- seq(lims_p[1], lims_p[2], len = 10)
  lines(limsx_p, rep(0.5, length(limsx_p)),      lty = "dashed", col = 2)
  lines(rep(test_p[1], length(limsx_p)), 0.5 + limsx_p, lty = "dashed", col = 2)
  lines(rep(test_p[2], length(limsx_p)), 0.5 + limsx_p, lty = "dashed", col = 2)
  legend("topleft", c("normal", "basic", "percentile"), col = 4:2,
         lty = "dashed")
  # counting proportions on left an right of missed values for each interval
  n_miss <- cbind(ifelse(theta_hat_star < test[1], 1, 0),
                  ifelse(theta_hat_star > test[2], 1, 0))
  b_miss <- cbind(ifelse(theta_hat_star < test_b[1], 1, 0),
                  ifelse(theta_hat_star > test_b[2], 1, 0))
  p_miss <- cbind(ifelse(theta_hat_star < test_p[1], 1, 0),
                  ifelse(theta_hat_star > test_p[2], 1, 0))
  cat("\nProportion of bootstrap points out on the normal ",
      1 / b * apply(n_miss, 2, sum),
      "\nProportion of bootstrap points out on the basic ",
      1 / b * apply(b_miss, 2, sum),
      "\nProportion of bootstrap points out on the percentile ",
      1 / b * apply(p_miss, 2, sum))
}

example <- function(x) {
  sim <- as.matrix(x, length(x), 1)
  theta_boot <- function(dat, ind) {
    x <- dat[ind, 1]
    mean(x)
  }
  boot_obj <- boot(data = sim, statistic = theta_boot, R = 2000)
  print(boot_obj)
  print(boot.ci(boot_obj,
                type = c("basic", "norm", "perc")))
}

intervals(x)
check <- readline(prompt = "Input 1 to close device ")
if (check == 1) {
  dev.off()
}
cat("\nRunning from boot package")
example(x)

# Question 2

library(boot)
set.seed(23)
cat("\n Loaded air conditioner data")
head(aircondit)
# using the diff function to generate the differences
times <- diff(aircondit$hours)
cat("\n Time differences: ", times)
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
# Question 4
library(boot)
set.seed(23)
problem <- function(x, y, n) {
  x <- matrix(runif(n, x[1], x[2]), n, 1)
  cbind(x, apply(x, 1, y))
}
# bootstrap
boot_func <- function(dat, ind) {
  x <- dat[ind, 1]
  y <- dat[ind, 2]
  cor(x, y)
}

boots_obj <- function(dat) {
  boot(dat, statistic = boot_func, R = 2000)
}
# part b p ~= 0.016, we reject H_0 that the correlation between X and Y is 0
data <- problem(c(-1, 1),
                function(u) {
                  u^2
                }, 100)
print(cor.test(x = data[, 1], y = data[, 2]))
# part c # p ~= 0.0005. Reject H_0 that X and Y are independent
library(energy)
print(dcov.test(x = data[, 1], y = data[, 2], R = 2000))
