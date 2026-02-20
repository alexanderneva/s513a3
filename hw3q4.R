# question 4
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
# part c
