#' @export
varde <- function(model) {
  UseMethod("varde")
}

#' @method varde lmerMod
#' @export
varde.lmerMod <- function(model) {
  tibble(
    component = c(names(summary(model)$varcor), "Residual"),
    variance = c(as.double(summary(model)$varcor), summary(model)$sigma^2),
    percent = variance / sum(variance),
    method = "lmer"
  )
}

