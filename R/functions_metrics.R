#' Compute selected performance metrics
#' @inheritParams eval_pred
#' @returns Returns a data.frame of the names (\code{metric}) and estimated
#'   values (\code{est}) of the performance metrics.
compute_metrics <- function(y_pred_bin, y_obs, y_pos = "1") {
  y_pred_bin <- factor(y_pred_bin)
  y_obs <- factor(y_obs)
  n <- length(y_obs)
  y_neg <- setdiff(levels(y_obs), y_pos)
  TP <- sum(y_pred_bin == y_pos & y_obs == y_pos)
  TN <- sum(y_pred_bin == y_neg & y_obs == y_neg)
  FP <- sum(y_pred_bin == y_pos & y_obs == y_neg)
  FN <- sum(y_pred_bin == y_neg & y_obs == y_pos)
  df <- data.frame(
    metric = c("Accuracy", "Balanced\nAccuracy", "PPV", "NPV", "TPR", "FPR", "TNR"),
    est = c((TP + TN) / n, (TP / (TP + FN) + TN / (TN + FP)) / 2,
            TP / (TP + FP), TN / (TN + FN),
            TP / (TP + FN), FP / (FP + TN), TN / (FP + TN))
  )
  df$metric <- factor(df$metric, levels = df$metric)
  df[sort.list(df$metric), ]
}
#' Computes selected performance metrics and bootstrap confidence interval (CI)
#' @inheritParams eval_pred
#' @returns Returns a data.frame generated from \code{compute_metrics()}, with
#'   two additional columns \code{lower} and \code{upper} of the bootstrap CI
#'   derived based on the quartiles of the bootstrap samples.
#' @importFrom stats quantile
compute_ci_boot <- function(y_pred_bin, y_obs, y_pos, B, conf_level) {
  set.seed(1234)
  df <- compute_metrics(y_pred_bin = y_pred_bin, y_obs = y_obs, y_pos = y_pos)
  n <- length(y_pred_bin)
  df_boot <- do.call("rbind", lapply(1:B, function(b) {
    idx <- sample(1:n, size = n, replace = TRUE)
    y_obs_b <- y_obs[idx]
    y_pred_bin_b <- y_pred_bin[idx]
    df <- compute_metrics(y_pred_bin = y_pred_bin_b, y_obs = y_obs_b, y_pos = y_pos)
    cbind(b = b, df)
  }))
  alpha <- 1 - conf_level
  ci_boot <- do.call("rbind", tapply(df_boot$est, df_boot$metric, function(x) {
    quantile(x, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  }))
  df_ci <- data.frame(metric = rownames(ci_boot),
                      lower = ci_boot[, 1], upper = ci_boot[, 2])
  merge(x = df, y = df_ci, by = "metric")
}
#' Evaluates fairness metrics within a subject group
#' @param y_pred_bin Predicted class label.
#' @param y_obs Observed class label.
#' @param y_pos Character representing positive class. Default is "1" for 0/1
#'   encoding.
#' @param B Number of bootstrap samples generated with replacement. Default is 1000.
#' @param conf_level Confidence level of the CI. Default is 0.95.
#' @returns Returns a data.frame of metrics with bootstrap 95\% CI
eval_pred <- function(y_pred_bin, y_obs, y_pos = "1", B = 1000, conf_level = 0.95) {
  compute_ci_boot(y_pred_bin = y_pred_bin, y_obs = y_obs, y_pos = y_pos,
                  B = B, conf_level = conf_level)
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
