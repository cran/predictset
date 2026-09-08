## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(predictset)

set.seed(42)
n <- 500
x <- matrix(rnorm(n * 5), ncol = 5)
y <- x[, 1] * 2 + x[, 2] + rnorm(n)
x_new <- matrix(rnorm(100 * 5), ncol = 5)

result <- conformal_split(x, y, model = y ~ ., x_new = x_new, alpha = 0.10)
print(result)

## -----------------------------------------------------------------------------
result <- conformal_split(x, y, model = y ~ ., x_new = x_new)

## -----------------------------------------------------------------------------
fit <- lm(y ~ ., data = data.frame(y = y, x))
result <- conformal_split(x, y, model = fit, x_new = x_new)

## ----eval = FALSE-------------------------------------------------------------
# rf <- make_model(
#   train_fun = function(x, y) {
#     ranger::ranger(y ~ ., data = data.frame(y = y, x))
#   },
#   predict_fun = function(object, x_new) {
#     predict(object, data = as.data.frame(x_new))$predictions
#   },
#   type = "regression"
# )
# 
# result <- conformal_split(x, y, model = rf, x_new = x_new)

## ----eval = FALSE-------------------------------------------------------------
# set.seed(42)
# n <- 600
# x <- matrix(rnorm(n * 4), ncol = 4)
# y <- factor(ifelse(x[, 1] > 0.5, "A", ifelse(x[, 2] > 0, "B", "C")))
# x_new <- matrix(rnorm(100 * 4), ncol = 4)
# 
# clf <- make_model(
#   train_fun = function(x, y) {
#     ranger::ranger(y ~ ., data = data.frame(y = y, x), probability = TRUE)
#   },
#   predict_fun = function(object, x_new) {
#     predict(object, data = as.data.frame(x_new))$predictions
#   },
#   type = "classification"
# )
# 
# # APS is randomised by default, which is the method as published. Pass `seed`
# # for reproducible sets; `randomize = FALSE` gives deterministic but materially
# # more conservative ones.
# result <- conformal_aps(x, y, model = clf, x_new = x_new, alpha = 0.10, seed = 1)
# print(result)
# 
# # Most predictions are a single class; ambiguous ones include 2-3
# table(set_size(result))

## -----------------------------------------------------------------------------
set.seed(42)
n <- 600
x <- matrix(rnorm(n * 3), ncol = 3)
groups <- factor(ifelse(x[, 1] > 0, "high", "low"))
y <- x[, 1] * 2 + ifelse(groups == "high", 3, 0.5) * rnorm(n)
x_new <- matrix(rnorm(200 * 3), ncol = 3)
groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

result <- conformal_mondrian(x, y, model = y ~ ., x_new = x_new,
                              groups = groups, groups_new = groups_new)

# Check per-group coverage
y_new <- x_new[, 1] * 2 + ifelse(groups_new == "high", 3, 0.5) * rnorm(200)
coverage_by_group(result, y_new, groups_new)

## ----eval = FALSE-------------------------------------------------------------
# # Empirical coverage (should be close to 1 - alpha)
# coverage(result, y_test)
# 
# # Average interval width (regression) - narrower is better
# mean(interval_width(result))
# 
# # Prediction set sizes (classification) - smaller is better
# table(set_size(result))
# 
# # Coverage within subgroups (fairness check)
# coverage_by_group(result, y_test, groups = groups_test)
# 
# # Coverage by prediction quantile bin
# coverage_by_bin(result, y_test, bins = 5)
# 
# # Conformal p-values for outlier detection
# pvals <- conformal_pvalue(result$scores, new_scores)

## -----------------------------------------------------------------------------
set.seed(42)
n <- 500
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(200 * 3), ncol = 3)
y_new <- x_new[, 1] * 2 + rnorm(200)

comp <- conformal_compare(x, y, model = y ~ ., x_new = x_new, y_new = y_new,
                           methods = c("split", "cv", "jackknife"))
print(comp)

