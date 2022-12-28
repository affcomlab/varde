#' @export plot.varde_res
#' @export
#' @importFrom graphics plot
plot.varde_res <- function(x,
                           type = "river",
                           font_size = 10,
                           xlim = c(4.75, 6.5),
                           interactive = FALSE,
                           ...) {

  match.arg(type, choices = c("river", "variances", "intercepts"))

  if (type == "river") {
    summary_df <- x$vars_summary
    df <-
      tibble::tibble(
        id = 1:nrow(summary_df),
        component = summary_df$component,
        variance = summary_df$estimate,
        percent = summary_df$percent,
        total = rep("Total\nVariance", nrow(summary_df)),
        label =
          paste0(
            summary_df$component,
            " (",
            round(summary_df$percent * 100, digits = 1), "%)")
      ) |>
      ggforce::gather_set_data(x = 5:6, id_name = "id")

    df$label <- forcats::fct_reorder(df$label, df$percent, .desc = TRUE)
    df$y <- factor(df$y, levels = c("Total\nVariance", levels(df$label)))

    out <-
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
        size = font_size / ggplot2::.pt,
        hjust = c(1, rep(0, times = nrow(summary_df))),
        nudge_x = c(-0.1, rep(0.1, times = nrow(summary_df))),
        angle = 0
      ) +
      ggplot2::coord_cartesian(xlim = xlim) +
      ggplot2::scale_fill_discrete() +
      ggplot2::theme_void()
  } else if (type == "variances") {
    append_var <- function(string) {
      paste0(string, " Variance")
    }
    posterior_df <- x$vars_posterior
    out <-
      tibble::as_tibble(posterior_df) |>
      tidyr::pivot_longer(
        cols = c(Rater, Target, Residual),
        names_to = "Component",
        values_to = "Variance"
      ) |>
      dplyr::mutate(
        Component = forcats::fct_relevel(Component, "Residual", after = Inf)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = Variance)) +
      ggplot2::facet_wrap(
        ~Component,
        scales = "free",
        labeller = ggplot2::labeller(.default = append_var)
      ) +
      ggplot2::geom_density(fill = "lightblue", alpha = 1/2) +
      ggplot2::scale_x_continuous() +
      ggplot2::labs(y = "Posterior Density", x = "Estimate") +
      ggplot2::theme_grey(base_size = font_size)
  } else if (type == "intercepts") {
    summary_df <- x$ints_summary
    out <-
      summary_df |>
      dplyr::mutate(strip_label = paste0(component, " ", term)) |>
      ggplot2::ggplot(ggplot2::aes(x = estimate, y = 1)) +
      ggplot2::facet_wrap(~strip_label, scales = "free") +
      ggbeeswarm::geom_quasirandom() +
      ggplot2::scale_x_continuous() +
      ggplot2::scale_y_discrete() +
      ggplot2::labs(x = "Estimate", y = NULL) +
      ggplot2::theme_grey(base_size = font_size)
    if (interactive) {
      require("plotly")
      out <-
        summary_df |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = estimate,
            y = 1,
            #text = paste0(component, ": ", id, "\nEstimate: ", round(estimate, 3))
          )
        ) +
        ggplot2::facet_wrap(~component, scales = "free") +
        ggplot2::geom_jitter(width = 0) +
        ggplot2::scale_x_continuous() +
        ggplot2::scale_y_discrete() +
        ggplot2::labs(x = "Estimated Intercept", y = NULL) +
        ggplot2::theme_grey(base_size = font_size)
      out <-
        plotly::ggplotly(out, tooltip = "text") |>
        plotly::config(displayModeBar = FALSE)
    }
  }

  out
}
