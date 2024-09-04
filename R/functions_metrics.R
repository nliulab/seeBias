#' Compute performance metrics with Clopper–Pearson 95\% CI
#' @param numerator Numerator for performance metrics formula.
#' @param denominator Denominator for performance metrics formula.
#' @param alpha Target error rate. Default is 0.05 for 95\% CI.
#' @return Returns a data.frame of the performance metrics (\code{est}) and the
#'   95\% CI (\code{lower}, \code{upper}). Consistent with the 95\% CI of
#'   accuracy returned from \code{carret::confusionMatrix} and
#'   \code{epiR::epi.tests} (default setting).
#' @references
#' \itemize{
#'  \item{Clopper CJ, Pearson ES. The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika. 1934 Dec 1;26(4):404-13.}
#' }
#' @importFrom stats qf
compute_ci_cp <- function(numerator, denominator, alpha = 0.05) {
  # Need to slightly modify the original formula for the 95% CI because we are
  # calculating for "failures", not "success" as in original paper
  x <- denominator - numerator
  n <- denominator
  f_ub <- suppressWarnings(
    qf(p = alpha / 2, df1 = 2 * x, df2 = 2 * (n - x + 1))
  )
  ub <- 1 - (1 + (n - x + 1) / (x * f_ub)) ^ -1
  f_lb <- suppressWarnings(
    qf(p = 1 - alpha / 2, df1 = 2 * (x + 1), df2 = 2 * (n - x))
  )
  lb <- 1 - (1 + (n - x) / ((x + 1) * f_lb)) ^ -1
  if (numerator == denominator & is.na(ub)) ub <- 1
  if (numerator == 0 & is.na(lb)) lb <- 0
  data.frame(est = numerator / denominator, lower = lb, upper = ub)
}
#' Evaluates fairness metrics within a subject group
#' @param y_pred_bin Predicted class label.
#' @param y_obs Observed class label.
#' @param y_pos Character representing positive class. Default is "1" for 0/1
#'   encoding.
#' @return Returns a data.frame of metrics with Clopper–Pearson 95\% CI
eval_pred <- function(y_pred_bin, y_obs, y_pos = "1") {
  y_pred_bin <- factor(y_pred_bin)
  y_obs <- factor(y_obs)
  n <- length(y_obs)
  y_neg <- setdiff(levels(y_obs), y_pos)
  TP <- sum(y_pred_bin == y_pos & y_obs == y_pos)
  TN <- sum(y_pred_bin == y_neg & y_obs == y_neg)
  FP <- sum(y_pred_bin == y_pos & y_obs == y_neg)
  FN <- sum(y_pred_bin == y_neg & y_obs == y_pos)
  df <- cbind(
    metric = c("Accuracy", "PPV", "NPV", "TPR", "FPR"),
    rbind(compute_ci_cp(numerator = TP + TN, denominator = n),
          compute_ci_cp(numerator = TP, denominator = TP + FP),
          compute_ci_cp(numerator = TN, denominator = TN + FN),
          compute_ci_cp(numerator = TP, denominator = TP + FN),
          compute_ci_cp(numerator = FP, denominator = FP + TN))
  )
  df
}
#' Compute 95\% CI for a proportion
#' @param p The proportion.
#' @param n Number of observations.
#' @param alpha Target error rate. Default is 0.05 for 95\% CI.
#' @return Returns a vector of the 95\% CI.
#' @importFrom stats qt
compute_ci_prop <- function(p, n, alpha = 0.05) {
  se <- sqrt(p * (1 - p) / (n - 1))
  z <- qt(p = 1 - alpha / 2, df = n - 1)
  c(max(c(p - z * se, 0)), min(c(p + z * se, 1)))
}
#' Find best threshold for predicted score from ROC analysis
#' @inheritParams eval_pred
#' @param y_pred Predicted probabilities or scores.
#' @return Returns the best threshold.
#' @importFrom pROC roc coords
find_y_pred_threshold <- function(y_pred, y_obs, y_pos) {
  y_neg <- setdiff(unique(y_obs), y_pos)
  m_roc <- pROC::roc(response = y_obs, predictor = y_pred,
                     levels = c(y_neg, y_pos), quiet = TRUE)
  as.numeric(pROC::coords(m_roc, "best", ret = c("threshold"), transpose = TRUE))
}
