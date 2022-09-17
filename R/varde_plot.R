#' @export plot.varde_res
#' @export
#' @importFrom graphics plot
plot.varde_res <- function(x, ...) {
  df <-
    tibble(
      id = 1:nrow(x),
      component = x$component,
      variance = x$variance,
      percent = x$percent,
      total = rep("Total\nVariance", nrow(x)),
      label = paste0(
        x$component,
        "\n",
        round(x$percent * 100, digits = 1),
        "%"
      )
    ) |>
    ggforce::gather_set_data(c("total", "label"))

  df$x <- factor(df$x, levels = c("total", "label"))
  df$label <- forcats::fct_relevel(
    factor(df$label),
    df[[nrow(df), "y"]],
    after = Inf
  )
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
      hjust = 1,
      nudge_x = -0.1,
      angle = 0
    ) +
    ggplot2::scale_fill_discrete() +
    ggplot2::theme_void()
}
