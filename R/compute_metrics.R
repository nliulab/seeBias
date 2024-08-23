#' Evaluate performance-based metrics
#' @param y_pred_bin Processed predicted label.
#' @param y_obs Processed observed label.
#' @param sens_var Sensitive variable.
#' @return Returns a list of \code{df_prob} (predicted vs observed probability)
#'   and \code{df_metrics} (predictive performance).
eval_metrics_based <- function(y_pred_bin, y_obs, sens_var) {
  y_pos <- "1"
  sens_var_ref <- levels(sens_var)[1]
  # Estimated and observed probabilities
  df_prob <- do.call("rbind", lapply(levels(sens_var), function(var) {
    i <- which(sens_var == var)
    p_obs <- mean(y_obs[i] == y_pos)
    p_obs_ci <- compute_ci_prop(p = p_obs, n = length(i))
    data.frame(group = var, p_obs = p_obs,
               p_obs_lower = p_obs_ci[1], p_obs_upper = p_obs_ci[2],
               p_pred = sum(y_pred_bin[i] == y_pos) / length(i))
  }))
  df_prob$group <- configure_group(group = df_prob$group, ref = sens_var_ref)
  df_prob$ratio <- df_prob$p_pred /
    df_prob$p_pred[which(df_prob$group == sens_var_ref)]
  # Performance metrics
  df_metrics <- do.call("rbind", lapply(levels(sens_var), function(var) {
    i <- which(sens_var == var)
    p_obs <- mean(y_obs[i] == y_pos)
    p_obs_ci <- compute_ci_prop(p = p_obs, n = length(i))
    cbind(
      group = var,
      eval_pred(y_pred_bin = y_pred_bin[i], y_obs = y_obs[i], y_pos = y_pos)
    )
  }))
  df_metrics$group <- configure_group(group = df_metrics$group,
                                      ref = sens_var_ref)
  df_metrics$ratio <- df_metrics$est /
    df_metrics$est[which(df_metrics$group == sens_var_ref)]
  df_metrics$metric <- factor(df_metrics$metric,
                              levels = rev(unique(df_metrics$metric)))
  list(df_prob = df_prob, df_metrics = df_metrics)
}
#' Compute ROC curves
#' @param y_pred Processed predicted probability.
#' @param y_obs Processed observed label.
#' @param sens_var Sensitive variable.
#' @importFrom pROC roc ci.auc
#' @return Returns a list containing a data.frame of data for ROC curves and a
#'   data.frame of AUC and 95\% DeLong CI.
eval_roc <- function(y_pred, y_obs, sens_var) {
  sens_var_ref <- levels(sens_var)[1]
  df_score <- data.frame(y_obs = y_obs, y_pred = y_pred, group = sens_var)
  ls_roc <- lapply(unique(sens_var), function(s) {
    df_score_s <- df_score[which(df_score$group == s), ]
    pROC::roc(
      response = df_score_s$y_obs, predictor = df_score_s$y_pred,
      levels = c("0", "1"), quiet = TRUE
    )
  })
  names(ls_roc) <- unique(sens_var)
  df_roc <- do.call("rbind", lapply(seq_along(ls_roc), function(i) {
    m_roc <- ls_roc[[i]]
    data.frame(
      fpr = as.vector(coords(m_roc, "local maximas", ret = "1-specificity",
                             transpose = TRUE)),
      tpr = as.vector(coords(m_roc, "local maximas", ret = "sensitivity",
                             transpose = TRUE)),
      group = names(ls_roc)[i]
    )
    # data.frame(fpr = 1 - m_roc$specificities, tpr = m_roc$sensitivities,
    #            group = names(ls_roc)[i])
  }))
  df_roc$group <- configure_group(group = df_roc$group, ref = sens_var_ref)
  df_auc <- do.call("rbind", lapply(seq_along(ls_roc), function(i) {
    m_roc <- ls_roc[[i]]
    ci_auc <- sort(pROC::ci.auc(roc = m_roc, method = "delong"))
    data.frame(auc = ci_auc[2], lower = ci_auc[1], upper = ci_auc[3],
               group = names(ls_roc)[i])
  }))
  df_auc$group <- configure_group(group = df_auc$group, ref = sens_var_ref)
  list(df_roc = df_roc, df_auc = df_auc)
}
#' Compute calibration curves
#' @inheritParams eval_roc
#' @importFrom probably cal_plot_breaks
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate bind_rows
#' @importFrom rlang .data
#' @return Returns a data.frame of data for calibration curves.
eval_calib <- function(y_pred, y_obs, sens_var) {
  sens_var_ref <- levels(sens_var)[1]
  df_calib <- do.call("rbind", lapply(levels(sens_var), function(sens_var_cat) {
    df_calib <- data.frame(
      y_obs = as.numeric(y_obs[which(sens_var == sens_var_cat)] == "1"),
      y_pred = y_pred[which(sens_var == sens_var_cat)]
    ) %>% probably::cal_plot_breaks(truth = .data$y_obs, estimate = .data$y_pred)
    df_calib$data$group <- sens_var_cat
    df_calib$data
  }))
  df_calib$group <- configure_group(group = df_calib$group, ref = sens_var_ref)
  df_calib
}
#' Evaluates fairness metrics based on sensitive variable(s)
#' @inheritParams eval_pred
#' @param y_pred Predicted risk (or score), if available. Use \code{y_pred_bin}
#'   if only predicted class label is available.
#' @param y_pred_threshold Threshold for \code{y_pred}.
#'   \code{y_pred > y_pred_threshold} will be considered positive.
#' @param y_pred_bin Predicted class label. Ignored if \code{y_pred} is provided.
#' @param sens_var Sensitive variable(s). Must be categorical.
#' @param sens_var_ref Reference class(es) of sensitive variable(s). Default is the
#'   default reference category in \code{sens_var}.
#' @return Returns a data.frame of performance metrics evaluated within each
#'   sensitive group.
#' @importFrom stats relevel
#' @export
evaluate_prediction <- function(y_pred = NULL, y_pred_threshold = NULL,
                                y_pred_bin = NULL, y_obs, y_pos = "1",
                                sens_var, sens_var_ref = NULL,
                                evaluate_calibration = TRUE) {
  input <- check_input(y_pred = y_pred, y_pred_threshold = y_pred_threshold,
                       y_pred_bin = y_pred_bin, y_obs = y_obs, y_pos = y_pos,
                       evaluate_calibration = evaluate_calibration)
  sens_var <- check_sens_var(sens_var = sens_var, sens_var_ref = sens_var_ref)
  if (length(sens_var) != length(y_obs)) {
    stop(simpleError("The length (or number of rows) of sensitive variables ('sens_var') must be the same as the length of observed label ('y_obs')."))
  }
  # Metrics:
  res_metrics <- eval_metrics_based(
    y_pred_bin = input$y_pred_bin, y_obs = input$y_obs, sens_var = sens_var
  )
  # ROC and AUC:
  if (is.null(y_pred)) {
    df_roc <- NULL
    df_auc <- NULL
  } else {
    ls_roc <- eval_roc(y_pred = y_pred, y_obs = input$y_obs, sens_var = sens_var)
    df_roc <- ls_roc$df_roc
    df_auc <- ls_roc$df_auc
  }
  # Calibration:
  if (is.null(y_pred) | !evaluate_calibration) {
    df_calib <- NULL
  } else {
    df_calib <- eval_calib(y_pred = y_pred, y_obs = input$y_obs,
                           sens_var = sens_var)
  }
  # Compile results
  obj <- list(df_metrics = res_metrics$df_metrics,
              df_prob = res_metrics$df_prob, df_roc = df_roc, df_auc = df_auc,
              df_calib = df_calib,
              y_pred = input$y_pred, y_obs = input$y_obs,
              sens_var = sens_var,
              y_pred_threshold = input$y_pred_threshold,
              n_sens = length(levels(sens_var)))
  class(obj) <- "seeBias"
  obj
}
#' @describeIn evaluate_prediction
#' Plot fairness metrics
#' @param x \code{FairLite} object to plot
#' @param y Not supported.
#' @param labels Labels for the 3 sub-figures in the combine figure. Default is
#'   A, B and C.
#' @param heights Relative height of metrics figure (top) and the two figures
#'   (bottom). Default is 2:1.
#' @param ... Not supported.
#' @importFrom ggpubr ggarrange annotate_figure text_grob
#' @export
plot.seeBias <- function(x, print_statistics = TRUE,
                         labels = c("A", "B", "C", "D", "E"),
                         heights = c(1.8, 1, 1), y = NULL, ...) {
  p_metrics <- plot_metrics(x = x)
  if (!is.null(x$df_roc)) {
    p_roc <- plot_roc(x = x, print_statistics = print_statistics)
  } else {
    message(simpleMessage(
      "ROC analysis not available when only predicted labels are provided."
    ))
    p_roc <- NULL
  }
  if (!is.null(x$df_calib)) {
    p_calib <- plot_calibration(x = x, print_statistics = print_statistics)
  } else {
    message(simpleMessage(
      "Calibration not available when only predicted labels are provided."
    ))
    p_calib <- NULL
  }
  p_calib_large <- plot_calib_large(x = x)
  if (!is.null(x$y_pred)) {
    p_score <- plot_score(x = x)
  } else {
    message(simpleMessage(
      "Prediction distribution not available when only predicted labels are provided."
    ))
    p_score <- NULL
  }
  if (x$n_sens > 4 & heights[1] < 2) heights <- c(2, 1, 1)
  # Arrange figures
  p_metrics <- ggpubr::ggarrange(p_metrics, nrow = 1, ncol = 1,
                                 labels = labels[1], font.label = "bold")
  p_roc <- ggpubr::ggarrange(p_roc, nrow = 1, ncol = 1,
                             labels = labels[2], font.label = "bold")
  if (!is.null(p_calib)) {
    p_calib <- ggpubr::ggarrange(p_calib, nrow = 1, ncol = 1,
                                 labels = labels[3], font.label = "bold")
    p_calib_large <- ggpubr::ggarrange(p_calib_large, nrow = 1, ncol = 1,
                                       labels = labels[4], font.label = "bold")
    p_score <- ggpubr::ggarrange(p_score, nrow = 1, ncol = 1,
                                 labels = labels[5], font.label = "bold")
    p_all <- ggpubr::ggarrange(
      p_metrics,
      ggpubr::ggarrange(p_roc, p_calib, nrow = 1, ncol = 2),
      ggpubr::ggarrange(p_calib_large, p_score, nrow = 1, ncol = 2),
      nrow = 3, ncol = 1, heights = heights
    )
  } else {
    p_calib_large <- ggpubr::ggarrange(p_calib_large, nrow = 1, ncol = 1,
                                       labels = labels[3], font.label = "bold")
    p_score <- ggpubr::ggarrange(p_score, nrow = 1, ncol = 1,
                                 labels = labels[4], font.label = "bold")
    p_all <- ggpubr::ggarrange(
      p_metrics,
      ggpubr::ggarrange(p_roc, p_calib_large, nrow = 1, ncol = 2),
      ggpubr::ggarrange(p_score, nrow = 1, ncol = 2),
      nrow = 3, ncol = 1, heights = heights
    )
  }
  plot(p_all)
  invisible(list(metrics = p_metrics, roc = p_roc,
                 calibration_in_large = p_calib_large, calibration = p_calib,
                 score = p_score,
                 overall = p_all))
}
