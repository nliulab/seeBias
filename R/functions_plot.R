#' Common theme for small figures
#' @importFrom ggplot2 theme element_text element_blank element_rect margin
common_theme_small <- function() {
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
        plot.margin = margin(t = 0, r = 15, b = 2, l = 2, unit = "pt"))
}

#' Private function to plot fairness metrics
#' @inheritParams plot.seeBias
#' @import ggplot2
#' @importFrom rlang .data
plot_metrics <- function(x) {
  df_metrics <- x$performance_evaluation$df_metrics
  # Includes TNR, which is not needed now, so remove. Also need to reset levels
  df_metrics$metric <- factor(as.character(df_metrics$metric),
                              levels = setdiff(levels(df_metrics$metric), "TNR"))
  df_metrics <- df_metrics[which(df_metrics$metric != "TNR"), ]
  f_scale_fill <- select_scale(x = x, type = "fill")
  n_sens <- x$input$n_sens
  w_sens <- 0.8 / n_sens

  df_metrics$y <- as.numeric(df_metrics$metric) -
    (as.numeric(df_metrics$group) - (1 + n_sens) / 2) * w_sens
  m <- length(levels(df_metrics$metric))
  sens_var_ref <- levels(df_metrics$group)[1]
  df_ref <- df_metrics[df_metrics$group == sens_var_ref, ]
  mark_ratio <- 0.8
  df_ref$x_lower <- df_ref$est * mark_ratio
  df_ref$x_upper <- df_ref$est / mark_ratio
  df_ref$y <- as.numeric(df_ref$metric)
  error_bar_w <- (4 * 5) * 0.1 / nrow(df_metrics)

  ggplot(df_metrics, aes(y = .data$y)) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0.5, m + 0.5), expand = FALSE) +
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_y_continuous(breaks = df_metrics$y, labels = df_metrics$group,
                       position = "left",
                       sec.axis = sec_axis(transform = ~ ., breaks = 1:m,
                                           labels = levels(df_metrics$metric))) +
    labs(y = NULL, x = "Performance metrics",
         # title = sprintf("Performance-based fairness (threshold=%.2f)",
         #                 x$y_pred_threshold),
         subtitle = sprintf("Performance evaluated at threshold=%.2f\nGreen shading: 0.8-1.25 times the reference level", x$input$y_pred_threshold)) +
         # subtitle = "Expect metrics within green range (80% rule)") +
    theme_bw() +
    geom_tile(data = df_ref,
              mapping = aes(x = (.data$x_lower + .data$x_upper) / 2,
                            width = .data$x_upper - .data$x_lower,
                            y = .data$y),
              fill = "#BCE6D8", alpha = 0.5, height = 1, color = NA) +
    geom_hline(yintercept = 1:m - 0.5) +
    geom_tile(aes(x = .data$est / 2, width = .data$est, fill = .data$group),
              height = w_sens - 0.01, color = NA) +
    geom_errorbar(aes(x = .data$est, xmin = .data$lower, xmax = .data$upper),
                  width = error_bar_w) +
    f_scale_fill() +
    theme(axis.ticks.y = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position = "none",
          plot.subtitle = element_text(size = 12),
          panel.border = element_rect(fill = NA, colour = "black", size = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(t = 0, r = 2, b = 0, l = 0, unit = "pt"))
}

#' Private function to plot ROC curves
#' @inheritParams plot.seeBias
#' @param print_statistics Whether to print statistics in the plot legend.
#' @import ggplot2
#' @importFrom rlang .data
plot_roc <- function(x, print_statistics) {
  df_roc <- x$performance_evaluation$df_roc
  df_auc <- x$performance_evaluation$df_auc
  if (is.null(df_roc)) return(NULL)

  if (print_statistics) {
    auc_text <- unlist(lapply(1:nrow(df_auc), function(i) {
      sprintf("%s: %.3f (%.3f-%.3f)", df_auc$group[i], df_auc$auc[i],
              df_auc$lower[i], df_auc$upper[i])
    }))
    auc_text_labels <- auc_text[match(levels(df_auc$group), df_auc$group)]
    legend_title <- "AUC (95% CI)"
  } else {
    auc_text_labels <- levels(df_auc$group)
    legend_title <- "Group"
  }

  df_roc$`AUC (95% CI)` <- factor(df_roc$group, levels = levels(df_roc$group),
                                  labels = auc_text_labels)
  f_scale_color <- select_scale(x = x, type = "color")
  ggplot(df_roc,
         aes(x = .data$fpr, y = .data$tpr, color = .data$`AUC (95% CI)`)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, lty = 2, lwd = 0.3) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    labs(x = "1-Specificity", y = "Sensitivity",
         title = "Receiver operating characteristic curves") +
    f_scale_color(name = legend_title) +
    theme_bw() +
    common_theme_small() +
    theme(legend.background = element_rect(fill = NA),
          legend.position = "right",
          legend.box = "vertical",
          legend.key.width = unit(1, "lines"),
          legend.key.spacing.y = unit(0.1, "lines")) +
    guides(color = guide_legend(ncol = 1))
}

#' Private function to plot calibration in the large
#' @inheritParams plot.seeBias
#' @import ggplot2
#' @importFrom rlang .data
plot_calib_large <- function(x) {
  df_prob <- x$performance_evaluation$df_prob
  if (is.null(df_prob)) return(NULL)

  f_scale_fill <- select_scale(x = x, type = "fill")
  n_sens <- x$input$n_sens
  w_sens <- 0.95
  df_prob$y <- n_sens + 1 - as.numeric(df_prob$group)
  error_bar_w <- 0.2 / 4 * nrow(df_prob)

  ggplot(df_prob, aes(y = .data$y)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_y_continuous(breaks = NULL, labels = NULL) +
    labs(y = "", x = "Proportion of positive label",
         title = "Calibration in the large",
         subtitle = "Black boxes: observed proportion (with 95% CI)") +
         # subtitle = "Expect prediction close to observation (black boxes)") +
    theme_bw() +
    geom_tile(aes(x = .data$p_pred / 2, width = .data$p_pred,
                  fill = .data$group),
              height = w_sens - 0.01, color = NA) +
    geom_tile(aes(x = .data$p_obs / 2, width = .data$p_obs),
              height = w_sens - 0.01, color = "black", fill = NA,
              linewidth = 0.5) +
    geom_errorbar(aes(x = .data$p_obs, xmin = .data$p_obs_lower,
                      xmax = .data$p_obs_upper),
                  width = error_bar_w) +
    f_scale_fill(name = "Group") +
    common_theme_small() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "right",
          legend.box = "vertical",
          legend.key.width = unit(0.5, "lines"),
          legend.key.height = unit(0.5, "lines"),
          legend.key.spacing.y = unit(0.5, "lines")) +
    guides(fill = guide_legend(ncol = 1))
}
#' Private function to plot calibration curves
#' @inheritParams plot_roc
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom probably cal_plot_logistic
plot_calibration <- function(x) {
  if (x$input$type == "bin") return(NULL)
  df_dat <- x$input$data
  df_dat$y_obs <- as.numeric(df_dat$y_obs == "1") # needed by cal_plot_logistic
  f_scale_color <- select_scale(x = x, type = "color")
  f_fill_color <- select_scale(x = x, type = "fill")

  subtitle <- NULL # "Calibration via the logistic recalibration framework"
  if (x$input$type == "score") {
    subtitle <- "Probabilities converted from scores using\nlogistic regression on observed labels (Platt scaling)"
    # subtitle <- paste0(
    #   subtitle,
    #   "\nProbabilities converted from scores using\nlogistic regression on observed labels (Platt scaling)"
    # )
  }
  x$input$data %>%
    mutate(y_obs = as.numeric(y_obs == 1)) %>%
    probably::cal_plot_logistic(
      truth = y_obs, estimate = y_pred_prob, include_ribbon = TRUE, smooth = FALSE,
      .by = sens_var
    ) +
    # Plot all in one panel and remove unnecessary panel strip:
    facet_wrap(facets = ~ NULL, drop = TRUE) +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    coord_cartesian(expand = FALSE) +
    labs(x = "Predicted probability",
         y = "Event rate\n(estimated by logistic recalibration)",
    title = "Calibration curves", subtitle = subtitle) +
    f_scale_color() +
    f_fill_color() +
    theme_bw() +
    common_theme_small() +
    theme(legend.background = element_rect(fill = NA),
          legend.position = "right",
          legend.box = "vertical",
          legend.key.width = unit(1, "lines"),
          legend.key.spacing.y = unit(0.1, "lines"),
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    guides(color = guide_legend(ncol = 1, title = "Group"),
           fill = guide_legend(ncol = 1, title = "Group"))
}
#' Private function to plot score distributions by label and group
#' @inheritParams plot.seeBias
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom stats quantile
plot_score <- function(x) {
  if (x$input$type == "prob") {
    labs_x <- "Predicted probability"
    labs_title <- "Distribution of predicted probability"
  } else if (x$input$type == "score") {
    labs_x <- "Predicted score"
    labs_title <- "Distribution of predicted score"
  } else {
    return(NULL)
  }
  f_scale_color <- select_scale(x = x, type = "color")
  df <- data.frame(y_pred = x$input$data$y_pred,
                   Label = factor(x$input$data$y_obs, levels = c(1, 0)),
                   Group = x$input$data$sens_var)
  n_group <- length(unique(df$Group))
  df$y_group <- n_group - as.numeric(df$Group) + 1

  ggplot(data = df,
         aes(y = .data$y_group, x = .data$y_pred, color = .data$Group,
             lty = .data$Label)) +
    geom_boxplot(position = position_dodge(width = 0.8), width = 0.6) +
    labs(x = labs_x, y = "", title = labs_title) +
    f_scale_color() +
    theme_bw() +
    common_theme_small() +
    theme(legend.position = "right",
          legend.box = "vertical",
          legend.key.width = unit(0.8, "lines"),
          legend.key.height = unit(0.5, "lines"),
          legend.key.spacing.y = unit(0.5, "lines"),
          legend.spacing.y = unit(0, "lines"), # Space between Group and Label
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.spacing = unit(0, "cm"),
          panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5)) +
    guides(
      color = guide_legend(ncol = 1, title = "Group"),
      linetype = guide_legend(ncol = 1, title = "Label")
    )
}

#' Private function to plot numbers needed by group
#' @inheritParams plot.seeBias
#' @import ggplot2
#' @importFrom rlang .data
plot_metrics_group <- function(x) {
  df_metrics_group <- x$performance_evaluation$df_metrics_group
  if (is.null(df_metrics_group)) return(NULL)
  f_scale_color <- select_scale(x = x, type = "color")
  common_theme <- theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    plot.margin = margin(t = 0, r = 15, b = 2, l = 2, unit = "pt"),
    legend.position = "right",
    legend.box = "vertical",
    legend.key.spacing.y = unit(0.1, "lines")
  )

  df_metrics_group$y_ppv <- 1 / df_metrics_group$PPV
  df_metrics_group$y_npv <- 1 / df_metrics_group$NPV
  # PPV Plot
  p_ppv <- ggplot(df_metrics_group,
                  aes(x = .data$threshold, y = .data$y_ppv, color = .data$group)) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_y_continuous(limits = c(floor(min(df_metrics_group$y_ppv)),
                                  ceiling(max(df_metrics_group$y_ppv)))) +
    labs(title = "Number needed for true positive",
         subtitle = "Average number of positive predictions required\nper true positive outcome",
         x = "Prediction threshold",
         y = "Positive predictions per true positive") +
    f_scale_color() +
    theme_bw() +
    common_theme +
    guides(color = guide_legend(ncol = 1, title = "Group"))

  # NPV Plot
  p_npv <- ggplot(df_metrics_group,
                  aes(x = .data$threshold, y = .data$y_npv, color = .data$group)) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_y_continuous(limits = c(floor(min(df_metrics_group$y_npv)),
                                  ceiling(max(df_metrics_group$y_npv)))) +
    labs(title = "Number needed for true negative",
         subtitle = "Average number of negative predictions required\nper true negative outcome",
         x = "Prediction threshold",
         y = "Negative predictions per true negative") +
    f_scale_color() +
    theme_bw() +
    common_theme +
    guides(color = guide_legend(ncol = 1, title = "Group"))

  list(p_ppv = p_ppv, p_npv = p_npv)
}
