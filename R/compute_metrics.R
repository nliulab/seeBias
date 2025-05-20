#' Evaluate performance-based metrics
#' @param y_pred_bin Processed predicted label.
#' @param y_obs Processed observed label.
#' @param sens_var Sensitive variable.
#' @return Returns a list of \code{df_prob} (predicted vs observed probability)
#'   and \code{df_metrics} (predictive performance).
eval_metrics_based <- function(y_pred_bin, y_obs, sens_var, B, conf_level) {
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
      eval_pred(y_pred_bin = y_pred_bin[i], y_obs = y_obs[i], y_pos = y_pos,
                B = B, conf_level = conf_level)
    )
  }))
  df_metrics$group <- configure_group(group = df_metrics$group,
                                      ref = sens_var_ref)
  df_metrics$ratio <- df_metrics$est /
    df_metrics$est[which(df_metrics$group == sens_var_ref)]
  df_metrics$metric <- factor(df_metrics$metric,
                              levels = rev(levels(df_metrics$metric)))
  list(df_prob = df_prob, df_metrics = df_metrics)
}
#' Compute ROC curves
#' @param y_pred Processed predicted probability or score.
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
#' Evaluate model performance at different thresholds by group
#' @param y_pred Processed predicted probabilities or scores.
#' @param y_obs Processed observed label.
#' @param sens_var Sensitive variable.
#' @importFrom tidyr pivot_wider
eval_metrics_group <- function(y_pred, y_obs, sens_var) {
  # Some quantiles may have the same value. Take unique values only
  thresholds <- unique(quantile(
    x = y_pred,
    probs = setdiff(seq(from = 0, to = 1, by = 0.1), c(0, 1))
  ))
  y_pos <- "1"
  sens_var_ref <- levels(sens_var)[1]
  # Performance metrics
  df_metrics <- do.call("rbind", lapply(thresholds, function(th) {
    y_pred_bin <- factor(as.numeric(y_pred >= th))
    do.call("rbind", lapply(levels(sens_var), function(var) {
      i <- which(sens_var == var)
      p_obs <- mean(y_obs[i] == y_pos)
      p_obs_ci <- compute_ci_prop(p = p_obs, n = length(i))
      cbind(
        group = var,
        threshold = th,
        eval_pred(y_pred_bin = y_pred_bin[i], y_obs = y_obs[i], y_pos = y_pos)
        # The above includes accuracy, which is not needed. Will remove later
      )
    }))
  }))
  df_metrics$group <- configure_group(
    group = df_metrics$group, ref = sens_var_ref
  )
  df_metrics <- df_metrics[which(df_metrics$metric != "Accuracy"), ]
  rownames(df_metrics) <- NULL
  tidyr::pivot_wider(
    data = df_metrics[, setdiff(names(df_metrics), c("lower", "upper"))],
    names_from = metric, values_from = est
  )
}
