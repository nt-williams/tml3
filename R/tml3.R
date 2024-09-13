#' Cross-validated Targeted Minimum Loss-based Estimation
#'
#' @param data
#' @param trt
#' @param outcome
#' @param covar
#' @param id
#' @param outcome_type
#' @param folds
#' @param learners_trt
#' @param learners_outcome
#' @param learners_cens
#' @param control
#'
#' @return
#' @export
#'
#' @examples
tml3 <- function(data, trt, outcome, covar, id = NULL,
                 outcome_type = c("binomial", "continuous"),
                 folds = 10,
                 learners_trt = "glm",
                 learners_outcome = "glm",
                 learners_cens = learners_trt,
                 control = tml3_control()) {

    outcome_type <- match.arg(outcome_type)
    tmp <- scale(data, outcome, outcome_type)
    folds <- make_folds(tmp, folds, id)
    trt_dummy <- format_trt(tmp, trt)
    obs <- as.numeric(!is.na(tmp[[outcome]]))
    lvls <- levels(factor(tmp[[trt]]))

    trim <- \(x, trim = control$.trim) pmin(x, quantile(x, trim))

    # outcome regressions
    m <- matrix(nrow = nrow(data), ncol = length(lvls) + 1)
    m_eps <- m
    colnames(m) <- c("A", lvls); colnames(m_eps) <- colnames(m)
    outcome_fits <- vector("list", length(folds))

    if (!all(as.logical(obs))) {
      # probability of being observed
      po <- matrix(nrow = nrow(data), ncol = 1, data = 1)
      cens_fits <- outcome_fits
    }
    # inverse probability of censoring weights
    icw <- matrix(nrow = nrow(data), ncol = 1, data = 1)

    # propensity scores
    g <- matrix(nrow = nrow(data), ncol = length(lvls))
    colnames(g) <- c(lvls)
    # inverse probability weights
    ipw <- matrix(nrow = nrow(data), ncol = length(lvls) + 1)
    colnames(ipw) <- c("A", lvls)
    trt_fits <- outcome_fits

    for (fold in seq_along(folds)) {
      train           <- get_train(tmp, folds, fold)
      train_trt_dummy <- get_train(trt_dummy, folds, fold)
      valid           <- get_valid(tmp, folds, fold)
      valid_trt_dummy <- get_valid(trt_dummy, folds, fold)

      # create versions of data for prediction
      valids <- vector("list", length(lvls) + 1)
      names(valids) <- c("A", lvls)
      valids[["A"]] <- valid_trt_dummy

      for (lvl in lvls) {
        current <- paste0(trt, ".", lvl)
        other <- setdiff(paste0(trt, ".", lvls), current)

        valids[[lvl]] <- data.table::copy(valid_trt_dummy)
        valids[[lvl]][[current]] <- 1
        data.table::set(valids[[lvl]], j = other, value = 0)
      }

      # Fit the outcome regression
      fit_Q <- mlr3superlearner::mlr3superlearner(
        data = cbind(
          train[ c(covar, outcome, id)],
          train_trt_dummy)[as.logical(obs)[folds[[fold]]$training_set], ],
        target = outcome,
        library = learners_outcome,
        outcome_type = match.arg(outcome_type),
        folds = control$.outcome_folds,
        newdata = lapply(valids, function(x) cbind(valid[, c(covar, id)], x)),
        group = id
      )

      for (lvl in c("A", lvls)) {
        m[folds[[fold]]$validation_set, lvl] <- fit_Q$preds[[lvl]]
      }

      # Fit the censoring model
      if (!all(as.logical(obs))) {
        fit_obs <- mlr3superlearner::mlr3superlearner(
          data = cbind(train[, c(covar, id)],
                       get_train(data.frame(obs = obs), folds, fold)),
          target = "obs",
          library = learners_cens,
          folds = control$.cens_folds,
          outcome_type = "binomial",
          newdata = list(valid[, c(covar, id)]),
        )

        po[folds[[fold]]$validation_set, 1] <- fit_obs$preds[[1]]
        icw[folds[[fold]]$validation_set, 1] <-
          obs[folds[[fold]]$validation_set] / po[folds[[fold]]$validation_set, 1]
      }

      # Fit the propensity scores
      for (lvl in lvls[1:(length(lvls) - 1)]) {
        target <- paste0(trt, ".", lvl)

        fit_g <- mlr3superlearner::mlr3superlearner(
          data = cbind(train[, c(covar, id)],
                       train_trt_dummy[, target, drop = FALSE]),
          target = target,
          library = learners_trt,
          outcome_type = "binomial",
          folds = control$.trt_folds,
          newdata = list(valid[, c(covar, id)]),
          group = id
        )

        g[folds[[fold]]$validation_set, lvl] <- fit_g$preds[[1]]
        ipw[folds[[fold]]$validation_set, lvl] <-
          trim(valid_trt_dummy[[target]] / g[folds[[fold]]$validation_set, lvl])
      }
    }

    g[, lvls[length(lvls)]] <- 1 - rowSums(g, na.rm = T)
    ipw[, lvls[length(lvls)]] <-
      trim(trt_dummy[[paste0(trt, ".", lvls[length(lvls)])]] /
             g[, lvls[length(lvls)]])


    ipw[, "A"] <- purrr::imap_dbl(tmp[[trt]], \(x, i) ipw[i, as.character(x)])

    # calculate TMLE
    tmle_data <- data.frame(y = tmp[[outcome]],
                            Q_A = m[, "A"],
                            H_A = ipw[, "A"]*icw[, 1])

    fluc <- suppressWarnings(
      glm(y ~ -1 + offset(qlogis(Q_A)) + H_A,
          data = tmle_data[obs, ],
          family = binomial)
    )
    eps <- coef(fluc)

    for (lvl in c("A", lvls)) {
      m_eps[, lvl] <- plogis(qlogis(m[, lvl]) + eps*ipw[, lvl]*icw[, 1])
    }

    y <- ifelse(is.na(tmp[[outcome]]), -999, tmp[[outcome]])
    psis <- apply(m_eps, 2, mean)
    eics <- lapply(c("A", lvls), function(lvl) {
      ipw[, lvl] * icw[, 1] * (y - m_eps[, lvl]) + m_eps[, lvl] - psis[lvl]
    })
    names(eics) <- c("A", lvls)

    eics <- lapply(eics, \(x) rescale(data[[outcome]], x, outcome_type))
    psis <- purrr::map_dbl(psis, \(x) rescale(data[[outcome]], x, outcome_type))

    if (is.null(id)) {
      ids <- 1:nrow(data)
    } else {
      ids <- data[[id]]
    }

    ses <- purrr::map_dbl(c("A", lvls), function(a) {
      clusters <- split(eics[[a]], ids)
      j <- length(clusters)
      sqrt(var(vapply(clusters, function(x) mean(x), 1)) / j)
    })

    names(ses) <- c("A", lvls)

    out <- list(
      lvls = lvls,
      psis = psis,
      ses = ses,
      eics = eics,
      ipw = ipw,
      icw = icw,
      m_eps = m_eps,
      call = match.call()
    )
    class(out) <- "tml3"
    print(out)
}
