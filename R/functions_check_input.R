#' Configure sensitive group
#' @param group Original group variable.
#' @param ref Reference class of sensitive variable.
#' @param ref_label New label for the reference class, if not \code{NULL}.
#' @importFrom stats relevel
configure_group <- function(group, ref, ref_label = NULL) {
  if (!is.null(ref_label)) {
    group <- as.character(group)
    group[which(group == ref)] <- ref_label
    ref <- ref_label
  }
  if (!is.factor(group)) group <- factor(group)
  relevel(x = group, ref = ref)
}
#' Check observed binary outcome
#' @inheritParams evaluate_prediction_prob
check_obs <- function(y_obs, y_pos) {
  if (is.null(y_obs)) {
    stop(simpleError("Please input observed binary outcome ('y_obs')."))
  }
  if (anyNA(y_obs)) {
    stop(simpleError("Observed outcome ('y_obs') must not have missing values."))
  }
  if (any(is.infinite(y_obs))) {
    stop(simpleError("Observed outcome ('y_obs') must not have infinite values."))
  }
  if (length(unique(y_obs)) != 2) {
    stop(simpleError("Observed outcome ('y_obs') should be binary."))
  }
  if (is.factor(y_obs)) {
    if (is.null(y_pos)) y_pos <- levels(y_obs)[2]
  } else {
    y_obs <- as.factor(y_obs)
  }
  if (is.null(y_pos) || is.na(y_pos)) {
    message("Positive label ('y_pos') not specified, assumed to be '1'.")
    y_pos <- "1"
  }
  if (!y_pos %in% levels(y_obs)) {
    stop(simpleError(sprintf(
      "Positive label ('y_pos'='%s') not found in observed labels ('y_obs'='%s').",
      y_pos, toString(levels(y_obs))
    )))
  }
  factor(as.numeric(as.character(y_obs) == as.character(y_pos)))
}
#' Check prediction (general)
#' @inheritParams evaluate_prediction_prob
check_pred_general <- function(y_pred, y_obs) {
  if (is.null(y_pred)) {
    stop(simpleError("Please input predicted outcome ('y_pred')."))
  }
  if (anyNA(y_pred)) {
    stop(simpleError("Predicted outcome ('y_pred') must not have missing values."))
  }
  if (any(is.infinite(y_pred))) {
    stop(simpleError("Predicted outcome ('y_pred') must not have infinite values."))
  }
  if (length(y_pred) != length(y_obs)) {
    stop(simpleError(
      "Predicted outcome ('y_pred') must have the same length as observed outcome ('y_obs')."
    ))
  }
}
#' Check observed binary outcome
#' @inheritParams evaluate_prediction_bin
check_pred_bin <- function(y_pred, y_pos, y_obs) {
  check_pred_general(y_pred = y_pred, y_obs = y_obs)
  if (length(unique(y_pred)) != 2) {
    stop(simpleError("Predicted outcome ('y_pred') should be binary."))
  }
  y_pred <- as.character(y_pred)
  if (!y_pos %in% levels(y_pred)) {
    stop(simpleError(sprintf(
      "Positive label ('y_pos'='%s') not found in observed labels ('y_pred'='%s').",
      y_pos, toString(levels(y_pred))
    )))
  }
  factor(as.numeric(as.character(y_pred) == as.character(y_pos)))
}
#' Check predicted probability
#' @inheritParams evaluate_prediction_prob
check_pred_prob <- function(y_pred, y_pred_threshold, y_obs) {
  if (!is.numeric(y_pred)) {
    y_pred <- as.numeric(y_pred)
    if (anyNA(y_pred)) {
      stop(simpleError(
        "Predicted probability ('y_pred') must be numeric and without missing values."
      ))
    }
  }
  check_pred_general(y_pred = y_pred, y_obs = y_obs)
  if (any(y_pred > 1 | y_pred < 0)) {
    stop(simpleError("Predicted probability ('y_pred') should be between 0 and 1."))
  }
  if (!is.null(y_pred_threshold)) {
    y_pred_range <- range(y_pred)
    if (y_pred_threshold >= max(y_pred_range) | y_pred_threshold <= min(y_pred_range)) {
      stop(simpleError(sprintf(
        "Threshold ('y_pred_threshold'=%.3f) is beyond the range of predicted probability (%.3f, %.3f).",
        y_pred_threshold, min(y_pred_range), max(y_pred_range)
      )))
    } else {
      message(sprintf(
        "Threshold=%.3f specified by user.", y_pred_threshold
      ))
    }
  } else {
    y_pred_threshold <- find_y_pred_threshold(
      y_pred = y_pred, y_obs = y_obs, y_pos = "1"
    )
    message(sprintf(
      "Threshold=%.3f set by ROC analysis.", y_pred_threshold
    ))
  }
  list(y_pred = y_pred, y_pred_threshold = y_pred_threshold,
       y_pred_bin = factor(as.numeric(y_pred > y_pred_threshold)))
}
#' Check predicted score
#' @inheritParams evaluate_prediction_score
#' @importFrom stats glm predict as.formula
check_pred_score <- function(y_pred, y_pred_threshold, y_obs) {
  if (!is.numeric(y_pred)) {
    y_pred <- as.numeric(y_pred)
    if (anyNA(y_pred)) {
      stop(simpleError(
        "Predicted score ('y_pred') must be numeric and without missing values."
      ))
    }
  }
  check_pred_general(y_pred = y_pred, y_obs = y_obs)
  if (all(y_pred <= 1 & y_pred >= 0)) {
    warning(simpleWarning("All values of predicted scores ('y_pred') are between 0 and 1. If 'y_pred' is predicted probability, please use function 'evaluate_prediction_prob' instead."))
  }
  if (!is.null(y_pred_threshold)) {
    y_pred_range <- range(y_pred)
    if (y_pred_threshold >= max(y_pred_range) | y_pred_threshold <= min(y_pred_range)) {
      stop(simpleError(sprintf(
        "Threshold ('y_pred_threshold'=%.3f) is beyond the range of predicted probability (%.3f, %.3f).",
        y_pred_threshold, min(y_pred_range), max(y_pred_range)
      )))
    } else {
      message(sprintf(
        "Threshold=%.3f specified by user.", y_pred_threshold
      ))
    }
  } else {
    y_pred_threshold <- find_y_pred_threshold(
      y_pred = y_pred, y_obs = y_obs, y_pos = "1"
    )
    message(sprintf(
      "Threshold=%.3f set by ROC analysis.", y_pred_threshold
    ))
  }
  # Convert score to probability by fitting a logistic regression of y_obs and
  # y_pred
  m <- glm(as.formula("y_obs ~ y_pred"), family = "binomial",
           data = data.frame(y_obs = y_obs, y_pred = y_pred))
  list(y_pred = y_pred, y_pred_threshold = y_pred_threshold,
       y_pred_prob = predict(object = m, type = "response"),
       y_pred_bin = factor(as.numeric(y_pred > y_pred_threshold)))
}
#' <Private function> Convert sensitive variable(s) to a data.frame of vector(s)
#' @inheritParams evaluate_prediction_prob
sens_var_to_df <- function(sens_var) {
  if (is.null(dim(sens_var))) {
    # A single sensitive variable, input should be a vector
    sens_var <- unlist(sens_var)
    if (!is.factor(sens_var)) {
      sens_var <- factor(as.character(sens_var))
    }
    sens_var_df <- data.frame(sens_var = sens_var, stringsAsFactors = TRUE)
  } else {
    # Multiple sensitive variables, input should be a matrix or data.frame
    sens_var_df <- as.data.frame(sens_var, stringsAsFactors = TRUE)
    for (i in 1:ncol(sens_var_df)) {
      if (!is.factor(sens_var_df[, i])) {
        sens_var_df[, i] <- factor(as.character(sens_var_df[, i]))
      }
    }
  }
  sens_var_df
}
#' <Private function> Check reference levels for sensitive variables
#' @param sens_var_df A data.frame of sensitive variable(s).
#' @inheritParams evaluate_prediction_prob
check_sens_ref <- function(sens_var_df, sens_var_ref) {
  if (ncol(sens_var_df) != length(sens_var_ref)) {
    stop(simpleError("Please specify exactly one reference level for each sensitive variable."))
  }
  any_error <- FALSE
  # message(sprintf("Checking references for sensitive variables ..."))
  for (i in seq_along(sens_var_ref)) {
    ref_i <- sens_var_ref[i]
    levels_i <- levels(sens_var_df[, i])
    if (!ref_i %in% levels_i) {
      message(sprintf(
        "Error in sensitive variable %d: reference level '%s' not found in levels '{%s}'.",
        i, ref_i, toString(levels_i)
      ))
      any_error <- TRUE
    }
  }
  if (any_error) {
    stop(simpleError("Please correct errors above."))
  } # else {
    # message(sprintf("Check passed.\n"))
  #}
}
#' Check and process sensitive variable
#' @inheritParams evaluate_prediction_prob
#' @return Returns a vector of processed sensitive variable as a factor with
#'   specified reference class.
check_sens_var <- function(sens_var, sens_var_ref) {
  if (anyNA(sens_var)) {
    stop(simpleError("Sensitive variable ('sens_var') must not have missing values."))
  }
  sens_var_df <- sens_var_to_df(sens_var = sens_var)
  if (is.null(sens_var_ref)) {
    sens_var_ref <- unlist(lapply(sens_var_df, function(sens_var) {
      levels(sens_var)[1]
    }))
  } else {
    sens_var_ref <- as.character(as.vector(unlist(sens_var_ref)))
    if (anyNA(sens_var_ref)) {
      stop(simpleError("Reference for sensitive variable ('sens_var_ref') must not have missing values."))
    }
    check_sens_ref(sens_var_df = sens_var_df, sens_var_ref = sens_var_ref)
  }
  message("Configuring sensitive variables ...")
  if (ncol(sens_var_df) > 1) {
    # Combine multiple sensitive variable into a single variable
    sens_var_vec <- NULL
    sens_var_vec <- interaction(sens_var_df[, 1], sens_var_df[, 2], sep = " & ")
    if (ncol(sens_var_df) > 2) {
      for (i in 3:ncol(sens_var_df)) {
        sens_var_vec <- interaction(sens_var_vec, sens_var_df[, i], sep = " & ")
      }
    }
    sens_var_ref <- paste(sens_var_ref, collapse = " & ")
  } else {
    sens_var_vec <- as.character(sens_var_df[, 1])
  }
  n_sens_var <- table(sens_var_vec)
  sens_var_levels <- names(which(n_sens_var > 0))
  message(sprintf(
    "    %d subgroups based on sensitive variables ('sens_var'): %s.\n    Reference group: %s.",
    length(sens_var_levels), toString(sens_var_levels), sens_var_ref
  ))
  message("Configuration completed.")
  list(
    sens_var_ref = sens_var_ref,
    sens_var = configure_group(group = sens_var_vec, ref = sens_var_ref,
                               ref_label = paste("[Ref]", sens_var_ref))
  )
}
