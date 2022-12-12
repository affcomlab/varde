# S3 Generic
#' @export
varde <- function(model, ci = 0.95) {
  UseMethod("varde")
}

# S3 Constructor
#' @exportClass verde_res
new_varde_res <- function(summary = tibble(), posterior = matrix()) {
  stopifnot(tibble::is_tibble(summary))
  stopifnot(is.matrix(posterior))
  structure(
    list(summary = summary, posterior = posterior),
    class = "varde_res"
  )
}

# S3 Helper
varde_res <- function(summary = tibble(), posterior = matrix()) {
  new_varde_res(summary, posterior)
}

#' @export print.varde_res
#' @export
print.varde_res <- function(x, ...) {
  print(x$summary, ...)
}
