#' Configure sensitive group
#' @param group Original group variable.
#' @param ref Reference class of sensitive variable.
configure_group <- function(group, ref, ref_label = NULL) {
  if (!is.null(ref_label)) {
    group <- as.character(group)
    group[which(group == ref)] <- ref_label
    ref <- ref_label
  }
  if (!is.factor(group)) group <- factor(group)
  relevel(x = group, ref = ref)
}
#' Check input prediction and observation for possible errors
#' @inheritParams evaluate_prediction
#' @return Returns a list of cleaned \code{y_pred_threshold} and \code{y_obs}
#'   (both using 0/1 encoding).
check_input <- function(y_pred, y_pred_threshold, y_pred_bin, y_obs, y_pos,
                        evaluate_calibration) {
  if (is.null(y_obs)) {
    stop(simpleError("Please input observed label ('y_obs')."))
  }
  if (anyNA(y_obs)) {
    stop(simpleError("Observed label ('y_obs') must not have missing values."))
  }
  if (length(unique(y_obs)) != 2) {
    stop(simpleError("Observed label ('y_obs') should be binary."))
  }
  if (is.null(y_pos) | is.na(y_pos)) {
    warning(simpleWarning(
      "Positive label ('y_pos') not specified, assumed to be '1'."
    ))
    y_pos <- "1"
  }
  if (!y_pos %in% levels(y_obs)) {
    stop(simpleError(sprintf(
      "Positive label ('y_pos'='%s') not found in observed labels ('y_obs'='%s').",
      y_pos, toString(levels(y_obs))
    )))
  }
  if (is.null(y_pred_bin) & is.null(y_pred)) {
    stop(simpleError("Please input predicted risk/label."))
  }
  # Recode y_obs to 0/1 for consistency
  y_obs_labels <- sort(unique(y_obs))
  y_obs <- factor(as.numeric(as.character(y_obs) == as.character(y_pos)))
  # Ignore y_pred_bin unless y_pred=NULL
  if (!is.null(y_pred)) {
    if (length(y_pred) != length(y_obs)) {
      stop(simpleError(
        "Predicted risk/score ('y_pred') must have the same length as observed label ('y_obs')."
      ))
    }
    y_pred <- as.numeric(y_pred)
    if (anyNA(y_pred)) {
      stop(simpleError(
        "Predicted risk/score ('y_pred') must be numeric and without missing values."
      ))
    }
    if (is.null(y_pred_threshold)) {
      y_pred_threshold <- find_y_pred_threshold(
        y_pred = y_pred, y_obs = y_obs, y_pos = "1"
      )
      message(simpleMessage(sprintf(
        "Threshold=%.2f set by ROC analysis.\n", y_pred_threshold
      )))
    } else {
      message(simpleMessage(sprintf(
        "Threshold=%.2f specified by user.\n", y_pred_threshold
      )))
    }
    y_pred_bin <- factor(as.numeric(y_pred > y_pred_threshold)) # 0/1 encoding
  } else {
    if (length(unique(y_pred_bin)) != 2) {
      stop(simpleError("Predicted label ('y_pred_bin') should be binary."))
    }
    if (length(y_pred_bin) != length(y_obs)) {
      stop(simpleError(
        "Predicted label ('y_pred_bin') must have the same length as 'y_obs'."
      ))
    }
    if (anyNA(y_pred_bin)) {
      stop(simpleError("Predicted label ('y_pred_bin') must not have missing values."))
    }
    if (!y_pos %in% levels(y_pred_bin)) {
      stop(simpleError(sprintf(
        "Predicted labels ('y_pred_bin'='%s') inconsistent with observed labels ('y_obs'='%s').",
        toString(levels(y_pred_bin)), toString(y_obs_labels)
      )))
    }
    # Recode y_pred_bin to 0/1 for consistency
    y_pred_bin <- factor(as.numeric(as.character(y_pred_bin) == y_pos))
  }
  # If predicted score is provided, cannot evaluate calibration even when requested
  if ((any(y_pred < 0) | any(y_pred > 1)) & evaluate_calibration) {
    warning(simpleWarning(
      "'y_pred' is not predicted risk because the range is beyond 0-1. Calibration cannot be evaluated."
    ))
    evaluate_calibration <- FALSE
  }
  list(y_pred_bin = y_pred_bin, y_pred = y_pred, y_obs = y_obs,
       y_pred_threshold = y_pred_threshold,
       evaluate_calibration = evaluate_calibration)
}
#' Check and process sensitive variable
#' @inheritParams evaluate_prediction
#' @return Returns a vector of processed sensitive variable as a factor with
#'   specified reference class.
check_sens_var <- function(sens_var, sens_var_ref) {
  if (anyNA(sens_var)) {
    stop(simpleError("Sensitive variable ('sens_var') must not have missing values."))
  }
  sens_var_ref <- as.vector(sens_var_ref)
  if (is.null(dim(sens_var))) {
    sens_var_vec <- unlist(sens_var)
    # If specified, reference for sensitive variable should be a single value
    if (!is.null(sens_var_ref)) {
      if (length(sens_var_ref) != 1) {
        warning(simpleWarning(
          "Multiple values specified for the reference class of sensitive variable ('sens_var_ref'). Only using the first class."
        ))
        sens_var_ref <- sens_var_ref[1]
        if (!sens_var_ref %in% sens_var) {
          stop(simpleError(sprintf(
            "Reference class of sensitive variable ('sens_var_ref'='%s') not found in sensitive variable ('sens_var'='%s').",
            sens_var_ref, toString(sort(unique(sens_var)))
          )))
        }
      }
    } else {
      if (!is.factor(sens_var_vec)) sens_var_vec <- factor(sens_var_vec)
      sens_var_ref <- levels(sens_var_vec)[1]
    }
  } else {
    # If specified, reference for sensitive variable should have the same length
    # as the number of columns in sensitive variable
    if (!is.null(sens_var_ref)) {
      if (length(sens_var_ref) != ncol(sens_var)) {
        stop(simpleError(
          "One reference class should be specified for each column of sensitive variable, therefore the length of 'sens_var_ref' should be the same as the number of columns in 'sens_var'."
        ))
      }
      if (!all(sens_var_ref %in% sens_var)) {
        stop(simpleError(sprintf(
          "Some reference classes of sensitive variable ('sens_var_ref'='%s') not found in sensitive variable ('sens_var'='%s').",
          toString(sens_var_ref),
          paste(apply(sens_var, 2, function(x) toString(sort(unique(x)))),
                collapse = "; ")
        )))
      }
    } else {
      sens_var_ref <- rep(NA, ncol(sens_var))
      for (i in 1:ncol(sens_var)) {
        if (!is.factor(sens_var[, i])) {
          sens_var[, i] <- factor(sens_var[, i])
        }
        sens_var_ref[i] <- levels(sens_var[, i])[1]
      }
    }
    # Multiple sensitive variable as a matrix or data.frame
    sens_var_vec <- NULL
    sens_var_vec <- interaction(sens_var[, 1], sens_var[, 2], sep = " & ")
    if (ncol(sens_var) > 2) {
      for (i in 3:ncol(sens_var)) {
        sens_var_vec <- interaction(sens_var_vec, sens_var[, i], sep = " & ")
      }
    }
    n_sens_var <- table(sens_var_vec)
    sens_var_levels <- names(which(n_sens_var > 0))
    sens_var_ref <- paste(sens_var_ref, collapse = " & ")
    message(simpleMessage(sprintf(
      "Among all %d combinations of the %d sensitive variables ('sens_var'), %d combinations are observed. The reference class is %s.",
      length(n_sens_var), ncol(sens_var), length(sens_var_levels), sens_var_ref
    )))
  }
  configure_group(group = sens_var_vec, ref = sens_var_ref,
                  ref_label = paste("[Ref]", sens_var_ref))
}
