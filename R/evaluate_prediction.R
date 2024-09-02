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
#' @describeIn evaluate_prediction_prob
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
#' @describeIn evaluate_prediction_prob
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
#' @describeIn evaluate_prediction_prob
#' Plot fairness metrics
#' @param x \code{seeBias} object
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
#' @describeIn evaluate_prediction_prob
#' Plot model performance metrics for fairness evaluation
#' @param object \code{seeBias} object
#' @param ... Not supported.
#' @importFrom dplyr mutate filter select group_by summarise
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
summary.seeBias <- function(object, ...) {
  df_metrics <- object$performance_evaluation$df_metrics %>%
    mutate(difference = .data$est -
             .data$est[which(.data$group == levels(.data$group)[1])]) %>%
    select(group, metric, ratio, difference)
  # Equal opportunity
  df_equal_opp <- df_metrics %>%
    filter(.data$metric == "TPR", .data$group != levels(.data$group)[1]) %>%
    select(-metric)
  names(df_equal_opp) <- c("Subgroup", "Equal opportunity ratio",
                           "Equal opportunity difference")
  # Equalised odds
  df_equalised_odds <- df_metrics %>%
    filter(.data$metric %in% c("FPR", "TPR"),
           .data$group != levels(.data$group)[1]) %>%
    group_by(.data$group) %>%
    summarise(ratio = .data$ratio[which.max(abs(.data$difference))],
              difference = .data$difference[which.max(abs(.data$difference))])
  names(df_equalised_odds) <- c("Subgroup", "Equalised odds ratio",
                                "Equalised odds difference")
  # Balanced error rate
  df_ber <- object$performance_evaluation$df_metrics %>%
    filter(.data$metric %in% c("FPR", "TPR")) %>%
    select(group, metric, est) %>%
    tidyr::pivot_wider(names_from = metric, values_from = est) %>%
    mutate(BER = (.data$FPR + 1 - .data$TPR) / 2,
           ratio = .data$BER /
             .data$BER[which(.data$group == levels(.data$group)[1])],
           difference = .data$BER -
             .data$BER[which(.data$group == levels(.data$group)[1])]) %>%
    filter(.data$group != levels(.data$group)[1]) %>%
    select(group, ratio, difference)
  names(df_ber) <- c("Subgroup", "BER equality ratio", "BER equality difference")
  df_fairness <- merge(df_equal_opp, df_equalised_odds, by = "Subgroup")
  df_fairness <- merge(df_fairness, df_ber, by = "Subgroup")
  ls_concept <- list(
    `Equal opportunity` = "Equal opportunity ensures that different subgroups have the same True Positive Rate (TPR). This means that the model is equally good at correctly identifying positive cases across all groups. It is measured by comparing the TPR of each subgroup to that of a reference group, either through a ratio or a difference.",
    `Equalised odds` = "Equalised odds ensures that different subgroups have the same True Positive Rate (TPR) and False Positive Rate (FPR). This means the model is equally accurate and equally prone to errors across all groups. It is assessed by separately comparing the TPR and FPR of each subgroup to those of a reference group, and then taking the larger disparity—whether it's in the TPR or FPR—based on the ratio or difference.",
    `BER equality` = "Balanced error rate (BER) equality ensures that the Balanced Error Rate (BER) is consistent across different subgroups. BER is calculated as the average of the False Positive Rate (FPR) and False Negative Rate (FNR, which is 1 minus the True Positive Rate [TPR]). This means the model's overall error rate, considering both false positives and false negatives, is uniform across all groups. It is assessed by comparing the BER of each subgroup to that of a reference group, with disparities measured using either ratios or differences."
  )
  obj_summ <- list(fairness_metrics = df_fairness,
                   fairness_metrics_concept = ls_concept)
  class(obj_summ) <- "summary.seeBias"
  print.summary.seeBias(obj_summ)
  invisible(obj_summ)
}
#' @describeIn evaluate_prediction_prob
#' Summarise fairness metrics
#' @param object \code{summary.seeBias} object
#' @param ... Not supported.
#' @param digits Number of digits to print for fairness metrics. Default is 3.
#' @param metric_type Whether to quantify fairness as \code{"difference"} or
#'   \code{"ratio"}. To print both, specify \code{"all"}.
#' @importFrom knitr kable
#' @export
print.summary.seeBias <- function(object, ..., digits = 3,
                                  metric_type = "difference") {
  metric_type <- match.arg(arg = tolower(metric_type),
                           choices = c("difference", "ratio", "all"))
  metric_names <- c("Equal opportunity", "Equalised odds", "BER equality")
  df_fairness <- object$fairness_metrics
  if (metric_type == "difference") {
    df_fairness <- df_fairness[, c("Subgroup", paste(metric_names, "difference"))]
  }
  if (metric_type == "ratio") {
    df_fairness <- df_fairness[, c("Subgroup", paste(metric_names, "ratio"))]
  }
  print(knitr::kable(df_fairness, digits = digits, row.names = FALSE))
  cat("\n")
  for (l in object$fairness_metrics_concept) cat(l, "\n\n")
}
