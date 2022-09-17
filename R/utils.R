check_convergence <- function(model) {
  if(any(abs(brms::rhat(model) - 1) > .1)) {
    return(FALSE)
  }

  TRUE
}

post_mode <- function(x) {
  d <- stats::density(x)
  d$x[which.max(d$y)]
}
