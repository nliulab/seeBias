#' Evaluate algorithm fairness across patient subgroups based on predicted probabilities
#' @param y_pred A numeric vector of predicted probabilities. Should not contain
#'   missing value.
#' @param y_pred_threshold Threshold for \code{y_pred}. Predict positive label
#'   if \code{y_pred > y_pred_threshold}. Default is \code{NULL}, in which case
#'   the threshold will be selected based on the ROC curve for \code{y_pred}.
#' @param y_obs A vector of observed binary outcome. Can be numeric, character
#'   or factor. Should not contain missing value.
#' @param y_pos A character representing the positive class in \code{y_obs}. If
#'   \code{y_obs} is a factor, by default \code{y_pos = levels(y_obs)[2]}. If
#'   \code{y_obs} is numeric or character, by default \code{y_pos = "1"} for 0/1
#'   encoding.
#' @param sens_var Sensitive variable(s). Must be categorical.
#' @param sens_var_ref Reference class(es) of sensitive variable(s). Default is the
#'   default reference category in \code{sens_var}.
#' @return Returns a data.frame of performance metrics evaluated within each
#'   sensitive group.
#' @export
evaluate_prediction_prob <- function(y_pred, y_pred_threshold = NULL,
                                     y_obs, y_pos = "1",
                                     sens_var, sens_var_ref = NULL) {
  # Process observation and prediction
  y_obs <- check_obs(y_obs = y_obs, y_pos = y_pos)
  input_pred <- check_pred_prob(
    y_pred = y_pred, y_pred_threshold = y_pred_threshold, y_obs = y_obs
  )
  # Process sensitive variable
  sens_var <- check_sens_var(sens_var = sens_var, sens_var_ref = sens_var_ref)
  if (length(sens_var) != length(y_obs)) {
    stop(simpleError("The length (or number of rows) of sensitive variables ('sens_var') must be the same as the length of observed label ('y_obs')."))
  }
  # Evaluate fairness
  # Metrics:
  res_metrics <- eval_metrics_based(
    y_pred_bin = input_pred$y_pred_bin, y_obs = y_obs, sens_var = sens_var
  )
  # ROC and AUC:
  ls_roc <- eval_roc(
    y_pred = input_pred$y_pred, y_obs = y_obs, sens_var = sens_var
  )
  df_roc <- ls_roc$df_roc
  df_auc <- ls_roc$df_auc
  # Calibration:
  df_calib <- eval_calib(
    y_pred = input_pred$y_pred, y_obs = y_obs, sens_var = sens_var
  )
  # Compile results
  obj <- list(
    input = list(
      data = data.frame(
        y_obs = y_obs,
        y_pred = input_pred$y_pred,
        y_pred_prob = input_pred$y_pred,
        y_pred_bin = input_pred$y_pred_bin,
        sens_var = sens_var
      ),
      y_pred_threshold = input_pred$y_pred_threshold,
      n_sens = length(levels(sens_var))
    ),
    performance_evaluation = list(
      df_metrics = res_metrics$df_metrics,
      df_prob = res_metrics$df_prob,
      df_roc = df_roc, df_auc = df_auc,
      df_calib = df_calib
    )
  )
  class(obj) <- "seeBias"
  obj
}
#' Evaluate algorithm fairness across patient subgroups based on predicted scores
#' @inheritParams evaluate_prediction_prob
#' @param y_pred A numeric vector of predicted scores. Should not contain
#'   missing value.
#' @return Returns a data.frame of performance metrics evaluated within each
#'   sensitive group.
#' @export
evaluate_prediction_score <- function(y_pred, y_pred_threshold = NULL,
                                      y_obs, y_pos = "1",
                                      sens_var, sens_var_ref = NULL) {
  # Process observation and prediction
  y_obs <- check_obs(y_obs = y_obs, y_pos = y_pos)
  input_pred <- check_pred_score(
    y_pred = y_pred, y_pred_threshold = y_pred_threshold, y_obs = y_obs
  )
  # Process sensitive variable
  sens_var <- check_sens_var(sens_var = sens_var, sens_var_ref = sens_var_ref)
  if (length(sens_var) != length(y_obs)) {
    stop(simpleError("The length (or number of rows) of sensitive variables ('sens_var') must be the same as the length of observed label ('y_obs')."))
  }
  # Evaluate fairness
  # Metrics:
  res_metrics <- eval_metrics_based(
    y_pred_bin = input_pred$y_pred_bin, y_obs = y_obs, sens_var = sens_var
  )
  # ROC and AUC:
  ls_roc <- eval_roc(
    y_pred = input_pred$y_pred, y_obs = y_obs, sens_var = sens_var
  )
  df_roc <- ls_roc$df_roc
  df_auc <- ls_roc$df_auc
  # Calibration:
  df_calib <- eval_calib(
    y_pred = input_pred$y_pred_prob, y_obs = y_obs, sens_var = sens_var
  )
  # Compile results
  obj <- list(
    input = list(
      data = data.frame(
        y_obs = y_obs,
        y_pred = input_pred$y_pred,
        y_pred_prob = input_pred$y_pred_prob,
        y_pred_bin = input_pred$y_pred_bin,
        sens_var = sens_var
      ),
      y_pred_threshold = input_pred$y_pred_threshold,
      n_sens = length(levels(sens_var))
    ),
    performance_evaluation = list(
      df_metrics = res_metrics$df_metrics,
      df_prob = res_metrics$df_prob,
      df_roc = df_roc, df_auc = df_auc,
      df_calib = df_calib
    )
  )
  class(obj) <- "seeBias"
  obj
}
#' Evaluate algorithm fairness across patient subgroups based on predicted labels
#' @inheritParams evaluate_prediction_prob
#' @param y_pred A vector of predicted outcome labels. Should not contain
#'   missing value.
#' @return Returns a data.frame of performance metrics evaluated within each
#'   sensitive group.
#' @export
evaluate_prediction_bin <- function(y_pred, y_obs, y_pos = "1",
                                    sens_var, sens_var_ref = NULL) {
  # Process observation and prediction
  y_obs <- check_obs(y_obs = y_obs, y_pos = y_pos)
  y_pred <- check_pred_bin(
    y_pred = y_pred, y_pos = y_pos, y_obs = y_obs
  )
  # Process sensitive variable
  sens_var <- check_sens_var(sens_var = sens_var, sens_var_ref = sens_var_ref)
  if (length(sens_var) != length(y_obs)) {
    stop(simpleError("The length (or number of rows) of sensitive variables ('sens_var') must be the same as the length of observed label ('y_obs')."))
  }
  # Evaluate fairness: only simple fairness metrics can be computed
  res_metrics <- eval_metrics_based(
    y_pred_bin = y_pred, y_obs = y_obs,
    sens_var = sens_var
  )
  # Compile results
  obj <- list(
    input = list(
      data = data.frame(
        y_obs = y_obs,
        y_pred = y_pred,
        y_pred_prob = NA,
        y_pred_bin = y_pred,
        sens_var = sens_var
      ),
      y_pred_threshold = NULL,
      n_sens = length(levels(sens_var))
    ),
    performance_evaluation = list(
      df_metrics = res_metrics$df_metrics,
      df_prob = res_metrics$df_prob,
      df_roc = NULL, df_auc = NULL,
      df_calib = NULL
    )
  )
  class(obj) <- "seeBias"
  obj
}
#' @describeIn evaluate_prediction
#' Plot fairness metrics
#' @param x \code{FairLite} object to plot
#' @param y Not supported.
#' @param ... Not supported.
#' @export
plot.seeBias <- function(x, print_statistics = TRUE, y = NULL, ...) {
  p_metrics <- plot_metrics(x = x)
  p_roc <- plot_roc(x = x, print_statistics = print_statistics)
  p_calib <- plot_calibration(x = x, print_statistics = print_statistics)
  p_calib_large <- plot_calib_large(x = x)
  p_score <- plot_score(x = x)
  plot(p_metrics)
  invisible(list(metrics = p_metrics, roc = p_roc,
                 calibration_in_large = p_calib_large, calibration = p_calib,
                 score = p_score))
}
