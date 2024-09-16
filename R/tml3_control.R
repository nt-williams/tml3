#' Title
#'
#' @param .trt_folds
#' @param .outcome_folds
#' @param .cens_folds
#' @param .trim
#' @param .discrete
#' @param .info
#'
#' @return
#' @export
#'
#' @examples
tml3_control <- function(.learners_trt_folds = NULL,
                         .learners_outcome_folds = NULL,
                         .learners_cens_folds = NULL,
                         .trim = 0.999,
                         .discrete = TRUE,
                         .info = FALSE) {
  list(.trim = .trim,
       .learners_outcome_folds = .learners_outcome_folds,
       .learners_trt_folds = .learners_trt_folds,
       .learners_cens_folds = .learners_cens_folds,
       .discrete = .discrete,
       .info = .info)
}
