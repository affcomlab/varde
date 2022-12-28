#' @export plot.varde_icc
#' @export
#' @importFrom graphics plot
plot.varde_icc <- function(x,
                           parameters = NULL,
                           font_size = 10,
                           ...) {

  post <- cbind(x$vars_posterior, x$iccs_posterior)

  assertthat::assert_that(
    rlang::is_null(parameters) || all(parameters %in% colnames(post)),
    msg = "parameter value not found, check spelling"
  )
  assertthat::assert_that(rlang::is_double(font_size, n = 1, finite = TRUE))

  colnames(post)[1:3] <- paste0(colnames(post)[1:3], " Variance")

  if (!rlang::is_null(parameters)) {
    post <- post[, parameters]
  }

  out <-
    tibble::as_tibble(post) |>
    tidyr::pivot_longer(
      cols = colnames(post),
      names_to = "Term",
      values_to = "Estimate"
    ) |>
    dplyr::mutate(Term = factor(Term, levels = colnames(post))) |>
    ggplot2::ggplot(ggplot2::aes(x = Estimate)) +
    ggplot2::facet_wrap(~Term, scales = "free") +
    ggplot2::geom_density(fill = "lightblue", alpha = 1/2) +
    ggplot2::scale_x_continuous() +
    ggplot2::labs(y = "Posterior Density") +
    ggplot2::theme_grey(base_size = font_size)

  out

}
