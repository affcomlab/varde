#' @export plot.varde_icc
#' @export
#' @importFrom graphics plot
plot.varde_icc<- function(x, parameters = NULL,
                          font_size = 10,
                          fill_color = "lightblue",
                          panel_spacing = 2,
                          order = NULL,
                          type = "point",
                          ...) {

  if (class(x$model) == "brmsfit"){
    post <- cbind(x$vars_samples, x$iccs_samples)

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
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::labs(y = "Posterior Density") +
      ggplot2::theme_grey(base_size = font_size) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.spacing.x = ggplot2::unit(panel_spacing, "mm"),
        plot.margin = ggplot2::margin(
          t = font_size,
          r = font_size,
          b = font_size,
          l = font_size
        )
      )

    out



  } else {

    #frequentist
    if (type == "point") {
      #order: default by summary order
      if (is.null(order)) {
        ord_var <- (x$vars_summary$component)
        ord_icc <- (x$iccs_summary$term)
      } else if (order == "reverse") {
        ord_var <- rev(x$vars_summary$component)
        ord_icc <- rev(x$iccs_summary$term)
      }

      #variances
      dat_var <- dplyr::mutate(x$vars_summary,name = forcats::fct_relevel(component, ord_var))

      plot_var <- ggplot2::ggplot(dat_var, ggplot2::aes(x=name, y=estimate, ymin = lower, ymax= upper)) +
        ggplot2::geom_point() + ggplot2::geom_errorbar(ymax =x$vars_summary$upper, ymin =  x$vars_summary$lower) +
        ggplot2::theme_bw()  +  ggplot2::xlab("Component") +
        ggplot2::ylab("Estimate") + ggplot2::coord_flip() +
        ggplot2::ggtitle("Random Effects")

      #iccs
      dat_icc <- dplyr::mutate(x$iccs_summary, name = forcats::fct_relevel(term, ord_icc))

      plot_iccs <- ggplot2::ggplot(dat_icc, ggplot2::aes(x=name, y=estimate, ymin = lower, ymax = upper)) +
        ggplot2::geom_point() +  ggplot2::geom_errorbar(ymax =x$iccs_summary$upper, ymin =  x$iccs_summary$lower) +
        ggplot2::theme_bw() +  ggplot2::xlab("Term") +
        ggplot2::ylab("Estimate") + ggplot2::coord_flip() +
        ggplot2::ggtitle("ICCs")

      out <- patchwork::wrap_plots(plot_var / plot_iccs)

      out
    } else if (type == "slab" ){

      post <- cbind(x$vars_samples, x$iccs_samples)

      assertthat::assert_that(rlang::is_double(font_size, n = 1, finite = TRUE))

      colnames(post)[1:3] <- paste0(colnames(post)[1:3], " Variance")

      # assertthat::assert_that(
      #   rlang::is_null(parameters) || all(parameters %in% colnames(post)),
      #   msg = "parameter value not found, check spelling"
      # )
      #
      # if (!rlang::is_null(parameters)) {
      #   post <- post[, parameters]
      # }
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
          interval_size = 1.5
        ) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
        ggplot2::labs(y = "Posterior Density") +
        ggplot2::theme_grey(base_size = font_size) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.spacing.x = ggplot2::unit(panel_spacing, "mm"),
          plot.margin = ggplot2::margin(
            t = font_size,
            r = font_size,
            b = font_size,
            l = font_size
          )
        )

      out


  }



  }

}







