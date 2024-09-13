#' @export
print.tml3 <- function(x, ...) {
  d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
  cli::cli_rule(left = "Results {.fn tml3}")
  cli::cli_end(d)
  for (i in seq_along(names(x$psis))) {
    ci <- x$psis[i] + c(-1, 1)*qnorm(0.975)*x$ses[i]
    print_marginal_mean(names(x$psis)[i], x$psis[i], x$ses[i], ci)
  }
}

print_marginal_mean <- function(level, psi, se, ci) {
  cat("\n")
  cli::cli_text(cat("  "), "{.strong Estimand}: E[Y(A={level})]")
  cli::cli_text(cat("  "), "{.strong Estimate}: {round(psi, 4)}")
  cli::cli_text("{.strong Std. error}: {round(se, 4)}")
  cli::cli_text(cat("    "), "{.strong 95% CI}: ({round(ci[1], 4)}, {round(ci[2], 4)})")
}
