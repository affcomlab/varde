#' @export plot.varde_icc
#' @export
#' @importFrom graphics plot
plot.varde_icc <- function(x,
                           components = "all",
                           font_size = 10,
                           ...) {

  match.arg(components, choices = c("all", "var", "icc"))

  if (components == "var") {
    post <- x$posterior[, 1:3]
    terms <- x$summary$term[1:3]
  } else if (components == "icc") {
    post <- x$posterior[, 4:9]
    terms<- x$summary$term[4:9]
  } else {
    post <- x$posterior
    terms <- x$summary$term
  }
  colnames(post) <- terms
  out <-
    tibble::as_tibble(post) |>
    tidyr::pivot_longer(
      cols = tidyr::everything(),
      names_to = "Term",
      values_to = "Estimate"
    ) |>
    dplyr::mutate(Term = factor(Term, levels = terms)) |>
    ggplot2::ggplot(ggplot2::aes(x = Estimate)) +
    ggplot2::facet_wrap(~Term, scales = "free") +
    ggplot2::geom_density(fill = "lightblue", alpha = 1/2) +
    ggplot2::scale_x_continuous() +
    ggplot2::labs(y = "Posterior Density") +
    ggplot2::theme_grey(base_size = font_size)

  out

}
