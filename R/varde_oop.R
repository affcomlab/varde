# S3 Generic
#' @export
varde <- function(model, ci = 0.95) {
  UseMethod("varde")
}

# S3 Constructor
#' @exportClass verde_res
new_varde_res <- function(vars_summary = tibble(),
                          vars_posterior = matrix(),
                          ints_summary = tibble(),
                          ints_posterior = matrix()) {
  stopifnot(tibble::is_tibble(vars_summary))
  stopifnot(tibble::is_tibble(ints_summary))
  stopifnot(is.matrix(vars_posterior))
  stopifnot(is.matrix(ints_posterior))
  structure(
    list(
      vars_summary = vars_summary,
      vars_posterior = vars_posterior,
      ints_summary = ints_summary,
      ints_posterior = ints_posterior
    ),
    class = "varde_res"
  )
}

# S3 Helper
varde_res <- function(vars_summary = tibble(),
                      vars_posterior = matrix(),
                      ints_summary = tibble(),
                      ints_posterior = matrix()) {
  new_varde_res(vars_summary, vars_posterior, ints_summary, ints_posterior)
}

#' @export print.varde_res
#' @export
print.varde_res <- function(x, ...) {
  cat(crayon::blue("# Variance Estimates\n"))
  print(x$vars_summary, ...)
  cat(crayon::blue("\n# Intercept Estimates\n"))
  print(x$ints_summary)
}
