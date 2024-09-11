#' Title
#'
#' @param .trt_folds
#' @param .outcome_folds
#' @param .cens_folds
#' @param .trim
#' @param .discrete
#'
#' @return
#' @export
#'
#' @examples
tml3_control <- function(.trt_folds = NULL,
                         .outcome_folds = NULL,
                         .cens_folds = NULL,
                         .trim = 0.999,
                         .discrete = TRUE) {
  list(.discrete = .discrete,
       .trim = .trim,
       .outcome_folds = .outcome_folds,
       .trt_folds = .trt_folds,
       .cens_folds = .cens_folds)
}
