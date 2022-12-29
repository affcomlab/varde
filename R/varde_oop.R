# S3 Generic
#' @export
varde <- function(model, ...) {
  UseMethod("varde")
}

# S3 Constructor
#' @exportClass verde_res
new_varde_res <- function(vars_summary = tibble(),
                          ints_summary = tibble(),
                          vars_posterior = matrix(),
                          ints_posterior = matrix(),
                          config = list(),
                          model = list()) {

  stopifnot(tibble::is_tibble(vars_summary))
  stopifnot(tibble::is_tibble(ints_summary))
  stopifnot(is.matrix(vars_posterior))
  stopifnot(is.matrix(ints_posterior))
  stopifnot(is.list(config))
  stopifnot(is.list(model), inherits(model, "brmsfit"))

  structure(
    list(
      vars_summary = vars_summary,
      ints_summary = ints_summary,
      vars_posterior = vars_posterior,
      ints_posterior = ints_posterior,
      config = config,
      model = model
    ),
    class = "varde_res"
  )
}

# S3 Helper
varde_res <- function(vars_summary = tibble(),
                      ints_summary = tibble(),
                      vars_posterior = matrix(),
                      ints_posterior = matrix(),
                      config = list(),
                      model = list()) {
  new_varde_res(
    vars_summary,
    ints_summary,
    vars_posterior,
    ints_posterior,
    config,
    model
  )
}

#' @export print.varde_res
#' @export
print.varde_res <- function(x, intercepts = TRUE, ...) {
  cat(crayon::blue("# Variance Estimates\n"))
  print(x$vars_summary, ...)
  if (intercepts) {
    cat(crayon::blue("\n# Intercept Estimates\n"))
    print(x$ints_summary, ...)
  }
}

# TODO: Add functions to extract posteriors from varde_res and varde_icc objects?


#' @export summary.varde_res
#' @export
summary.varde_res <- function(x,
                              which = "variances",
                              ...) {

  match.arg(which, choices = c("variances", "intercepts", "model"))
  if (which == "variances") {
    out <- x$vars_summary
  } else if (which == "intercepts") {
    out <- x$ints_summary
  } else if (which == "model") {
    out <- summary(x$model, ...)
  }
  out
}
