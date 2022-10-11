#' @export plot.varde_res
#' @export
#' @importFrom graphics plot
plot.varde_res <- function(x, ...) {
  df <-
    tibble::tibble(
      id = 1:nrow(x),
      component = x$component,
      variance = x$variance,
      percent = x$percent,
      total = rep("Total\nVariance", nrow(x)),
      label = ifelse(
        x$percent > 0.10,
        paste0(x$component, "\n", round(x$percent * 100, digits = 1), "%"),
        paste0(x$component, " (", round(x$percent * 100, digits = 1), "%)")
      )
    ) |>
    ggforce::gather_set_data(x = 5:6, id_name = "id")

  df$label <- forcats::fct_reorder(df$label, df$percent, .desc = TRUE)
  df$y <- factor(df$y, levels = c("Total\nVariance", levels(df$label)))

  ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(x, id = id, split = y, value = percent)
  ) +
    ggforce::geom_parallel_sets(
      mapping = ggplot2::aes(fill = label),
      alpha = 0.3,
      axis.width = 0.1,
      show.legend = FALSE
    ) +
    ggforce::geom_parallel_sets_axes(
      axis.width = 0.1
    ) +
    ggforce::geom_parallel_sets_labels(
      size = 4,
      hjust = c(1, rep(0, times = nrow(x))),
      nudge_x = c(-0.1, rep(0.1, times = nrow(x))),
      angle = 0
    ) +
    ggplot2::scale_fill_discrete() +
    ggplot2::theme_void()
}
