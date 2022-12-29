#' @export plot.varde_icc
#' @export
#' @importFrom graphics plot
plot.varde_icc <- function(x,
                           parameters = NULL,
                           font_size = 10,
                           fill_color = "lightblue",
                           panel_spacing = 2,
                           ...) {

  post <- cbind(x$vars_posterior, x$iccs_posterior)

  assertthat::assert_that(rlang::is_double(font_size, n = 1, finite = TRUE))

  colnames(post)[1:3] <- paste0(colnames(post)[1:3], " Variance")

  assertthat::assert_that(
    rlang::is_null(parameters) || all(parameters %in% colnames(post)),
    msg = "parameter value not found, check spelling"
  )

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
    ggdist::stat_slabinterval(
      fill = fill_color,
      normalize = "panels",
      point_interval = x$config$method,
      .width = x$config$ci,
      interval_size = 1.5,
      ...
    ) +
    ggplot2::scale_x_continuous() +
    ggplot2::labs(y = "Posterior Density") +
    ggplot2::theme_grey(base_size = font_size) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(panel_spacing, "mm")
    )

  out

}
