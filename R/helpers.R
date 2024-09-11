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
  if (match.arg(outcome_type) == "continuous") {
    bounds <- c(min(data[[outcome]], na.rm = T), max(data[[outcome]], na.rm = T))
    data[[outcome]] <- (data[[outcome]] - bounds[1]) / (bounds[2] - bounds[1])
  }
  data
}

rescale <- function() {

}
