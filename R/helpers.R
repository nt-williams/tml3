make_folds <- function(data, folds, id) {
  folded <- origami::make_folds(data, V = folds, cluster_ids = {
    if (is.null(id))
      1:nrow(data)
    else
      data[[id]]
  })

  if (folds == 1) {
    folded[[1]]$training_set <- folded[[1]]$validation_set
  }

  folded
}

scale <- function(data, outcome, outcome_type) {
  if (outcome_type == "continuous") {
    bounds <- c(min(data[[outcome]], na.rm = T), max(data[[outcome]], na.rm = T))
    data[[outcome]] <- (data[[outcome]] - bounds[1]) / (bounds[2] - bounds[1])
  }
  data
}

rescale <- function(outcome, scaled, outcome_type) {
  if (outcome_type == "continuous") {
    bounds <- c(min(outcome, na.rm = T), max(outcome, na.rm = T))
    return((scaled*(bounds[2] - bounds[1])) + bounds[1])
  }
  scaled
}

format_trt <- function(data, trt) {
  trt_factor <- data.table::as.data.table(model.matrix(~ factor(data[[trt]]) - 1))
  lvls <- levels(factor(data[[trt]]))
  names(trt_factor) <- paste0(trt, ".", lvls)
  trt_factor
}

get_train <- function(data, folds, fold) {
  data.table::as.data.table(data[folds[[fold]]$training_set, ])
}

get_valid <- function(data, folds, fold) {
  data.table::as.data.table(data[folds[[fold]]$validation_set, ])
}
