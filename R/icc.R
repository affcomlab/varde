# One-way ICCs ------------------------------------------------------------

## nested design, single rating
icc_1 <- function(vs, vrins) {
  vs / (vs + vrins)
}

## nested design, average ratings, balanced number of raters
icc_k <- function(vs, vrins, k) {
  vs / (vs + vrins / k)
}

## nested design, average ratings, unbalanced number of raters
icc_khat <- function(vs, vrins, khat) {
  vs / (vs + vrins / khat)
}

# Helper functions --------------------------------------------------------

create_srm <- function(.data,
                       subject = "subject",
                       rater = "rater",
                       score = "score") {
  # TODO: Allow subject, rater, and score to be specified using NSE
  assertthat::assert_that(is.data.frame(.data) || is.matrix(.data))
  cn <- colnames(.data)
  assertthat::assert_that(rlang::is_character(subject, n = 1), subject %in% cn)
  assertthat::assert_that(rlang::is_character(rater, n = 1), rater %in% cn)
  assertthat::assert_that(rlang::is_character(score, n = 1), score %in% cn)

  # Remove missing and non-finite scores
  na_index <- is.na(.data[[score]]) | !is.finite(.data[[score]])
  .data <- .data[!na_index, ]

  # Check if each subject was scored by each rater
  srm <- table(.data[[subject]], .data[[rater]], useNA = "no") > 0

  new_srm(srm)
}

create_parse <- function(.data, subject, rater, scores, v){
  # TODO: Check that is_twoway() works correctly for mv models
  # TODO: Check that everything works when scores contain weird characters
  twoway <- is_twoway(.data, subject, rater)
  if (twoway && v == 1) {
    formula <- brms::bf(paste0(
      scores, " ~ 1 + (1 | ", subject, ") + (1 | ", rater, ")"
    ))
  } else if (!twoway && v == 1) {
    formula <- brms::bf(paste0(
      scores, " ~ 1 + (1 | ", subject, ")"
    ))
  } else if (twoway && v > 1) {
    formula <- brms::bf(paste0(
      "mvbind(", paste(scores, collapse = ", "), ") | mi() ~ ",
      "1 + (1 | ", subject, ") + (1 | ", rater, ")"
    )) + brms::set_rescor(FALSE)
  } else if (!twoway && v > 1) {
    formula <- brms::bf(paste0(
      "mvbind(", paste(scores, collapse = ", "), ") | mi() ~ ",
      "1 + (1 | ", subject, ")"
    )) + brms::set_rescor(FALSE)
  } else {
    stop("Error determining model type")
  }
  return(formula)

}

create_parseaov <- function(.data, subject, rater, scores, v){
  # TODO: Check that is_twoway() works correctly for mv models
  # TODO: Check that everything works when scores contain weird characters
  twoway <- is_twoway(.data, subject, rater)
  if (twoway && v == 1) {
    formula <- paste0("(",scores," ~ ",subject, " + ", rater, ")")

  } else if (!twoway && v == 1) {
    formula <- paste0("(",scores," ~ ",subject, ")")

  } else if (twoway && v > 1) {
    stop("Multiple scores in AOV? Need to do...")
  } else if (!twoway && v > 1) {
    stop("Multiple scores in AOV? Need to do...")
  } else {
    stop("Error determining model type")
  }
  return(formula)

}


# calc_icc() --------------------------------------------------------------

#' Calculate Inter-Rater ICC
#'
#' Calculate variance component and inter-rater intraclass correlation estimates
#' using a Bayesian generalizability study.
#'
#' @param .data Either a data frame containing at least the variables identified
#'   in `subject`, `rater`, and `score` or a brmsfit object.
#' @param subject A string indicating the column name in `.data` that contains
#'   an identifier for the subject or thing being scored in each row (e.g.,
#'   person, image, or document). (default = `"subject"`)
#' @param rater A string indicating the column name in `.data` that contains an
#'   identifier for the rater or thing providing the score in each row (e.g.,
#'   rater, judge, or instrument). (default = `"rater"`)
#' @param scores A character vector indicating the column names in `.data` that
#'   contain the numerical scores representing the rating of each row's subject
#'   from that same row's rater (e.g., score, rating, judgment, measurement).
#'   (default = `c("score1", "score2")`)
#' @param k Either `NULL` to set the number of raters you would like to estimate
#'   the reliability of to the total number of unique raters observed in `.data`
#'   or an integer specifying the number of raters you would like to estimate
#'   the reliability of (see details below). (default = `NULL`)
#' @param method A function (ideally from [ggdist::point_interval()]) that
#'   returns a data frame containing a point estimate (`y`) and the lower
#'   (`ymin`) and upper (`ymax`) bounds of an interval estimate. (default =
#'   [ggdist::mode_qi()])
#' @param engine A character vector indicating the choice of estimation
#' framework (`"LME"`, or `"BRMS"`). (default = "`"BRMS"`)
#' @param ci A finite number between 0 and 1 that represents the width of the
#'   credible intervals to estimate (e.g., 0.95 = 95% CI). (default = `0.95`)
#' @param chains An integer representing the number of Markov chains to use in
#'   estimation. Forwarded on to [brms::brm()]. (default = `4`)
#' @param iter An integer representing the total number of interations per chain
#'   (including warmup). Forwarded on to [brms::brm()]. (default = `5000`)
#' @param file Either `NULL` to ignore or a string representing the filename to
#'   save the results to. If a file with that name already exists, the results
#'   will instead be read from that file. (default = `NULL`)
#' @param ... Further arguments passed to [brms::brm()].
#' @return A list object of class "varde_icc" that includes three main elements:
#' * `$iccs_summary`: A [tibble::tibble()] containing summary information about
#'   each ICC estimate.
#' * `$vars_summary`: A [tibble::tibble()] containing summary information about
#'   each variance estimate.
#' * `$ints_summary`: A [tibble::tibble()] containing summary information about
#'   each random intercept estimate.
#' * `$iccs_posterior`: A matrix where each row is a single posterior sample and
#'   each column is an ICC estimate.
#' * `$vars_posterior`: A matrix where each row is a single posterior sample and
#'   each column is a variance estimate.
#' * `$ints_posterior`: A matrix where each row is a single posterior sample and
#'   each column is a random intercept estimate.
#' * `$config`: A list containing the specified `method`, `ci`, `k` values.
#' * `$model`: The brmsfit object created by [brms::brm()] containing the full
#'   results of the Bayesian generalizability study.
#' @export
calc_icc <- function(.data,
                     subject = "subject",
                     rater = "rater",
                     scores = c("score1", "score2"),
                     k = NULL,
                     method = ggdist::mode_qi,
                     engine = "BRMS",
                     ci = 0.95,
                     chains = 4,
                     iter = 5000,
                     file = NULL,
                     ...) {

  assertthat::assert_that(
    rlang::is_null(k) || rlang::is_integerish(k, n = 1)
  )
  assertthat::assert_that(
    rlang::is_double(ci, n = 1, finite = TRUE),
    ci > 0, ci < 1
  )
  assertthat::assert_that(
    rlang::is_integerish(chains, n = 1, finite = TRUE),
    chains >= 1
  )

  if (!is.null(file)) {
    # Add rds extension if needed
    if (tools::file_ext(file) != "rds") {
      file <- paste0(file, ".rds")
    }
    # If already exists, read it in
    if (file.exists(file)) {
      message("Reading results from file")
      out <- readRDS(file)
      return(out)
    }
  }

  # How many score variables were provided?
  v <- length(scores)

  # Create logical subject-rater matrices
  srm <- lapply(
    X = scores,
    FUN = create_srm,
    .data = .data,
    subject = subject,
    rater = rater
  )
  names(srm) <- scores

  # Count the number of raters who scored each subject
  ks <- lapply(X = srm, FUN = rowSums)

  # Count the number of subjects scored by each rater
  nk <- lapply(X = srm, FUN = colSums)

  # TODO
  ## ADD WARNINGS WHEN DATA IS TRIMMED? ####
  ## fix the output plots for LME objects
  ## Fix the CI of estimates in LME objects?

  # # Remove all subjects that had no raters
  keep <- lapply(ks, function(x) names(x[x > 0])) |>
    unlist() |>
    unique()
  .data <- .data[.data[[subject]] %in% keep, ]

  # # Remove all raters that had no subjects
  keep <- lapply(nk, function(x) names(x[x > 0])) |>
    unlist() |>
    unique()
  .data <- .data[.data[[rater]] %in% keep, ]

  ## Check type of design
  # Balanced or unbalanced
  ##TODO Recall:
  #two-way complete/incomplete and/or balanced/unbalanced designs
  #one-way is special case of incomplete two-way design (balanced no overlap)
  #tell user which engine


  balanced <- is_balanced(.data, subject,rater)
  complete <- is_complete(.data, subject,rater)
  twoway <- is_twoway(.data, subject, rater)


  # If not specified, set k as the number of unique raters
  if (is.null(k)) {
    k <- length(unique(.data[[rater]]))
  }

  # Calculate the harmonic mean of the number of raters per subject
  khat <- lapply(srm, calc_khat)

  # Calculate the proportion of non-overlap for raters and subjects
  q <- lapply(srm, calc_q)

  if(twoway == FALSE){
      q <- 1/k #since sigmaR cannot be distinguished
    }


  #Branch engine cases

  switch(engine,


         "LME" = {
           # Construct mixed-effects formula
           formula <- create_parse(.data, subject, rater, scores, v)

           fit <- lme4::lmer(formula = formula,
                             data = .data)

           #res <- varde(fit, subject, rater, ci, k, khat, q) #obtain model summaries

           res <- varde(fit, ci=ci) #obtain model summaries


           if (check_convergence(fit)){
           #  icc_est <- computeICC_random(fit, subject, k, khat, q, v)


             #obtain confidence intervals of ICCs
             ## TODO: What if one way model fit?
             iccs_CIs <- ICC_CIs_LME(fit, subject, rater, ci, k, khat, q)

             #need to take simulated CIs but estimated ICCs
             # build iccs_estimates object equivalent to brms branch
             icc_names <- c(
               "ICC(A,1)__Score", "ICC(A,khat)__Score",
               "ICC(A,k)__Score", "ICC(C,1)__Score",
               "ICC(Q,khat)__Score", "ICC(C,k)__Score"
             )

             #TODO
             #change this somehow ?
             #note, you used the simulated data and took point estimates and CIs!
             method <- ggdist::mean_qi

             iccs_estimates <- get_estimates(iccs_CIs$Samples, method = method, ci = ci)
             iccs_estimates$term <- icc_names

             # for when u use t(icc_est) instead
             # iccs_estimates <- data.frame(
             #   term = icc_names,
             #   y = t(icc_est),
             #   ymin = iccs_CIs$ci.lower,
             #   ymax = iccs_CIs$ci.upper,
             #   .wdith = rep(ci,length(icc_est)),
             #   .point = rep("mean",length(icc_est)),
             #   .interval = rep("CI",length(icc_est))

             #)
             #rownames(iccs_estimates) <- NULL
           #  nam <- c("ICCa1_ci","ICCakhat_ci","ICCak_ci","ICCc1_ci","ICCqkhat_ci","ICCck_ci")
             iccs <-data.matrix(iccs_CIs$Samples)



           } else {
             stop("Model did not converge")
           }
         },

         "BRMS" = {
           # Construct mixed-effects formula
           formula <- create_parse(.data, subject, rater, scores, v)
           twoway <- is_twoway(.data, subject, rater)
           # Fit Bayesian mixed-effects model
           fit <- brms::brm(
             formula = formula,
             data = .data,
             chains = chains,
             iter = iter,
             init = "random",
             ...
           )

           # Extract posterior draws from model
           res <- varde(fit, ci = ci)

           # Extract posterior draws as matrices
           if (v > 1) {
             vs <- res$vars_posterior[, paste(subject, bname(scores), sep = "__")]
             if (twoway) {
               vr <- res$vars_posterior[, paste(rater, bname(scores), sep = "__")]
             } else {
               vr <- rep(NA_real_, length(vs))
             }
             vsr <- res$vars_posterior[, paste("Residual", bname(scores), sep = "__")]
           } else {
             vs <- res$vars_posterior[, subject]
             if (twoway) {
               vr <- res$vars_posterior[, rater]
             } else {
               vr <- rep(NA_real_, length(vs))
             }
             vsr <- res$vars_posterior[, "Residual"]
           }

           colnames(vs) <- scores
           colnames(vr) <- scores
           colnames(vsr) <- scores

           # # Calculate the harmonic mean of the number of raters per subject
           # khat <- lapply(srm, calc_khat)
           #
           # # Calculate the proportion of non-overlap for raters and subjects
           # q <- lapply(srm, calc_q)

           # Make matrices for k, khat, and q
           kmat <- matrix(rep(k, times = v * nrow(vs)), ncol = v, byrow = TRUE)
           khatmat <- matrix(
             rep(unlist(khat), times = nrow(vs)),
             ncol = v,
             byrow = TRUE
           )

           qmat <- matrix(
             rep(unlist(q), times = nrow(vs)),
             ncol = v,
             byrow = TRUE
           )

           # Calculate posterior for each intraclass correlation coefficient
           iccs <- cbind(
             vs / (vs + vr + vsr),
             vs / (vs + (vr + vsr) / khatmat),
             vs / (vs + (vr + vsr) / kmat),
             vs / (vs + vsr),
             vs / (vs + qmat * vr + vsr / khatmat),
             vs / (vs + vsr / kmat)
           )
           icc_names <- c(
             "ICC(A,1)", "ICC(A,khat)", "ICC(A,k)",
             "ICC(C,1)", "ICC(Q,khat)", "ICC(C,k)"
           )
           colnames(iccs) <- paste(
             rep(icc_names, each = v),
             colnames(iccs),
             sep = "__"
           )

           # Construct ICC output tibble
           iccs_estimates <- get_estimates(iccs, method = method, ci = ci)


         }

         )




  iccs_summary <-
    data.frame(
     # term = icc_names,
      term = iccs_estimates$term,
      estimate = iccs_estimates$y,
      lower = iccs_estimates$ymin,
      upper = iccs_estimates$ymax,
      raters = rep(c(rep(1, v), unlist(khat), rep(k, v)), times = 2),
      error = rep(c("Absolute", "Relative"), each = v * 3)
    ) |>
    tidyr::separate(col = term, into = c("term", "score"), sep = "__") |>
    dplyr::relocate(score, .before = 1) |>
    dplyr::arrange(score, error, raters)

  # if (v == 1) {
  #   colnames(iccs) <- icc_names
  # }


  out <-
    varde_icc(
      iccs_summary = iccs_summary,
      vars_summary = res$vars_summary,
      ints_summary = res$ints_summary,
      iccs_posterior = iccs,
      vars_posterior = res$vars_posterior,
      ints_posterior = res$ints_posterior,
      config = list(method = method, ci = ci, k = k),
      model = fit
    )



  if (!is.null(file)) {
    try(saveRDS(out, file = file), silent = FALSE)
  }

  out
}


# computeICC_random() --------------------------------------------------------------

#' Calculate random effects LME variances into ICC - BETA VERSION
#'
#' Given a random LME model from LME4, first identify the model, then calculate ICC from variances
#'
#' @param model_fit The model object from a lme4.
#' @param subjects The specific variable designated as the raters for ICC calculation.
#' @return A vector of ICCs from the specified lme4 model
#' @export
computeICC_random <- function(model_fit, subjects = "subjects", k, khat, q, v){


  ran_eff <- attr(model_fit@flist,"names")


  if (length(ran_eff) > 1) {
    #two way random effects
    lme_vars <- lme4::VarCorr(model_fit)
    vs <- lme_vars[[subjects]][1] #obtain object name

    #get not specified random effects variances
    ran_eff <- ran_eff[ran_eff != subjects]
    vr <- lme_vars[[ran_eff]][1]

    #residual (residual + interactions)
    vsr <- (attr(lme4::VarCorr(model_fit), "sc"))^2

    #create ICCs with multiple scores
    kmat <- matrix(rep(k, times = v * nrow(table(vs))), ncol = v, byrow = TRUE)
    khatmat <- matrix(
      rep(unlist(khat), times = nrow(table(vs))),
      ncol = v,
      byrow = TRUE
    )

    qmat <- matrix(
      rep(unlist(q), times = nrow(table (vs))),
      ncol = v,
      byrow = TRUE
    )

    ICC <- cbind(
      vs / (vs + vr + vsr), #ICC_A1 absolute (single score)
      vs / (vs + (vr + vsr) / khatmat), #ICC_Akhat absolute (average, using full K)
      vs / (vs + (vr + vsr) / kmat), #ICC_AK (average, using desired k)
      vs / (vs + vsr), #ICC_C1 consistency (single score)
      vs / (vs + qmat * vr + vsr / khatmat), #ICC_Qkhat (average, using full k)
      vs / (vs + vsr / kmat) # ICC_CK (average, using desired k)
    )

    icc_names <- c(
      "ICC(A,1)", "ICC(A,khat)", "ICC(A,k)",
      "ICC(C,1)", "ICC(Q,khat)", "ICC(C,k)"
    )

    colnames(ICC) <- paste(
      rep(icc_names, each = v),
      colnames(ICC),
      sep = "__"
    )

    return (ICC)


  }  else {
    #one way random effects models
    obj_eff <- lme4::VarCorr(model_fit)[[subjects]][1] #obtain ICC for object
    res_err <- (attr(VarCorr(model_fit), "sc"))^2

    ICC_C1 <-  obj_eff / (res_err + obj_eff)

    ICC <- tibble(ICC_C1 = c(ICC_C1))
  }

  return(ICC)


}







## TODO
# add simulation of CI random effects ----DONE
# plot rivers for lme varde object ----- DONE

# you can get CI of sigmas by using calc_icc(which specified raters, objects, q ,etc)
# but you can't then call varde(res_2) to get a varde object which then calls CIs.?
# solution: create a branch of CI_LME that extracts model components and does MCMC
# on the random effects, which doesn't require a specific rater/object distiniction.
# so basically generalize your inputs that aren't required as null, pull the
# needed components from the LME model object, so varde() can do it's thing ..
# then test to see how this would affect the downstream calc_icc() run of CI_LME.


# if you do varde(LME), then calc_icc(LME), the results of the CIs sigmas are slightlys different due to
# simulation variance


## TODO
# what stated above is before meeting Jeff. Now here's the direction:
# 3/29
# -- WHY IS ICCs plots show Target first, then Residual, then Rater? see screenshot
#
# - use the monte-carlo to obtain distributions of the variances.
# --if only using varde(), obtain the estimates, CI, and plots using these distributions
# --if using calc_iccs(), then obtain the distributions from varde,
# do math to obtain both the ICC estimates, CIs, and plots accordingly.
# -- validate both between the simulations and the analysitic estimates (without the simulations?)
# -- give user the option to reorder plot rows
# -Either forcats::fct_reorder(x,wt? = w?), or strings
# -- give user the option for ICCs to be ploted as bars or distributions.
# -- change the varde object to have field for "simultions" instead of posterier.
# -- run small tests()
# -- check()
