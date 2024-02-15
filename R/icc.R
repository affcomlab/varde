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
#' framework (`"ANOVA"`, `"LME"`, or `"BRMS"`). (default = "`"BRMS"`)
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
#' * `$config`: A list containing the specified `method`, `ci`, and `k` values.
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
  #ks <- lapply(X = srm, FUN = rowSums)

  # Count the number of subjects scored by each rater
  #nk <- lapply(X = srm, FUN = colSums)

  # # Remove all subjects that had no raters
  # keep <- lapply(ks, function(x) names(x[x > 0])) |>
  #   unlist() |>
  #   unique()
  # .data <- .data[.data[[subject]] %in% keep, ]
  #
  # # Remove all raters that had no subjects
  # keep <- lapply(nk, function(x) names(x[x > 0])) |>
  #   unlist() |>
  #   unique()
  # .data <- .data[.data[[rater]] %in% keep, ]

  ## Check type of design
  # Balanced or unbalanced

  balanced <- is_balanced(.data, subject,rater)
  complete <- is_complete(.data, subject,rater)
  twoway <- is_twoway(.data, subject, rater)


  khat <- NULL #harmonic mean of raters (usually in incomplete designs)
  Q <- NULL #proportion of nonoverlap across raters

  if(is.null(khat) | is.null(Q)){
    ## Decide on values for khat and q
    if(balanced == TRUE & complete == TRUE){
      khat <- k
      Q <- 0
    } else {
      if(balanced == TRUE & complete == FALSE){
        khat <- unique(rowSums(table(.data[[subject]], by = .data[[rater]])))
        Q <- computeQkhat(.data, subjects = subject, raters = rater)$Q
      } else {
        if(balanced == FALSE & complete == FALSE){
          Qkhat <- computeQkhat(.data, subjects = subject, raters = rater)
          khat <- Qkhat$khat
          Q <- Qkhat$Q
        } else {
          if(twoWay == FALSE){
            khat <- computeKhat(data, subjects = subjects, raters = raters)
            Q <- 1/k # But not needed, since sigmaR cannot be distinguished
          }
        }
      }
    }
  }



  # If not specified, set k as the number of unique raters
  if (is.null(k)) {
    k <- length(unique(.data[[rater]]))
  }

  #Branch engine cases

  switch(engine,

         "ANOVA" = {
           formula <- create_parseaov(.data, subject, rater, scores, v)

           fit <- aov(formula = formula,
                      data = .data)

           iccs_estimats <- computICC_aov(fit,subject)
         },

         "LME" = {
           # Construct mixed-effects formula
           formula <- create_parse(.data, subject, rater, scores, v)

           fit <- lme4::lmer(formula = formula,
                             data = .data)

           # Check convergence (ten Hove et. al, 2022)
           checkConv <- function(mod){
             warn <- mod@optinfo$conv$lme4$messages
             !is.null(warn) && grepl('failed to converge', warn)
           }

           if (checkConv(fit)){
             stop("Model did not converge")
           } else {

             iccs_estimates <- computeICC_random(fit, subject, k, khat, Q)

             #obtain confidence intervals of ICCs
             iccs_CIs <- CIs_LME(fit, subject, rater, ci, k, khat, Q)

             #need to take simulated CIs but estimated ICCs

           }
         },

         "BRMS" = formula <- {
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

           # Calculate the harmonic mean of the number of raters per subject
           khat <- lapply(srm, calc_khat)

           # Calculate the proportion of non-overlap for raters and subjects
           q <- lapply(srm, calc_q)

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
      term = colnames(iccs),
      estimate = iccs_estimates$y,
      lower = iccs_estimates$ymin,
      upper = iccs_estimates$ymax,
      raters = rep(c(rep(1, v), unlist(khat), rep(k, v)), times = 2),
      error = rep(c("Absolute", "Relative"), each = v * 3)
    ) |>
    tidyr::separate(col = term, into = c("term", "score"), sep = "__") |>
    dplyr::relocate(score, .before = 1) |>
    dplyr::arrange(score, error, raters)

  if (v == 1) {
    colnames(iccs) <- icc_names
  }

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

# computeICC_aov() --------------------------------------------------------------

computeICC_aov <- function(model_fit, subjects = "subjects"){



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
computeICC_random <- function(model_fit, subjects = "subjects", k, khat, q){


  ran_eff <- attr(model_fit@flist,"names")


  if (length(ran_eff) > 1) {
    #two way random effects
    lme_vars <- lme4::VarCorr(model_fit)
    obj_eff_var <- lme_vars[[subjects]][1] #obtain object name

    #get not specified random effects variances
    ran_eff <- ran_eff[ran_eff != subjects]
    other_vars <- lme_vars[[ran_eff]][1]

    #residual
    res_err <- (attr(lme4::VarCorr(model_fit), "sc"))^2

    # If more than 2 random effects?
    # for (i in 1:length(ran_eff)) {
    #   other_vars[i]<- lme_vars[[ran_eff[i]]]
    # }
    #

    # number of levels
    n_subs <- nlevels(model_fit@flist[subjects][[subjects]])
    n_other <- nlevels(model_fit@flist[ran_eff][[ran_eff]])

    # single score
    ICC_A1 <- obj_eff_var / (obj_eff_var + other_vars + res_err) #absolute
    ICC_Akhat <- obj_eff_var / (obj_eff_var + (other_vars + res_err)/khat) #absolutekhat
    ICC_C1 <-  obj_eff_var / (obj_eff_var + res_err) #consistency


    #average score
    Q <- 0 #for balanced and complete designs
    ICC_AK <- obj_eff_var / (obj_eff_var + (other_vars+res_err)/n_other) #absolute
    ICC_CKhat <- obj_eff_var / (obj_eff_var + Q*other_vars + res_err/khat) #consistency avgkhat
    ICC_CK <- obj_eff_var / (obj_eff_var + (res_err)/n_other) #consistency

    #ICC <- tibble(ICC_A1 = c(ICC_A1), ICC_AK = c(ICC_AK), ICC_C1 = c(ICC_C1),ICC_CK = c(ICC_CK))
    ICC <- cbind(ICC_A1,ICC_Akhat,ICC_AK,ICC_C1,ICC_CKhat,ICC_CK)


  }  else {
    #one way random effects models
    obj_eff <- lme4::VarCorr(model_fit)[[subjects]][1] #obtain ICC for object
    res_err <- (attr(VarCorr(model_fit), "sc"))^2

    ICC_C1 <-  obj_eff / (res_err + obj_eff)

    ICC <- tibble(ICC_C1 = c(ICC_C1))
  }

  return(ICC)


}




############################################################################
## FUNCTIONS TO COMPUTE KHAT AND Q (ten Hove et al, 2022)
############################################################################
## Function to compute khat and Q from obs. design
computeQkhat <- function(data, subjects = "subjects", raters = "raters"){
  k <- length(unique(data[[raters]]))
  share <- 0 # Proportion shared raters

  RR <- data[[raters]]
  SS <- data[[subjects]]
  tabRxS <- table(RR, SS)
  uSub <- ncol(tabRxS) # Number of unique subjects
  khat <- uSub/ sum(1/colSums(tabRxS)) # harmonic mean number of raters per subject
  for(i in 1:uSub){
    k_s <- colSums(tabRxS)[i]

    for(j in (1:uSub)[-i]){
      k_sprime <- colSums(tabRxS)[j]
      k_s.sprime <- sum(RR[SS == i] %in% RR[SS == j])
      share <- share + (k_s.sprime / (k_s*k_sprime))/(uSub * (uSub-1))
    }
  }
  Q <- round(1/khat - share, 3)
  names(Q) <- "Q"

  return(list(Q = Q, khat = khat))
}

## Function to compute only khat (saves time when Q is not needed)
computeKhat <- function(data, subjects = "subjects", raters = "raters"){
  k <- length(unique(data[[raters]]))
  share <- 0 # Proportion shared raters

  RR <- data[[raters]]
  SS <- data[[subjects]]
  tabRxS <- table(RR, SS)
  uSub <- ncol(tabRxS) # Number of unique subjects
  khat <- uSub/ sum(1/colSums(tabRxS)) # harmonic mean number of raters per subject

  return(khat)
}

############################################################################
## FUNCTIONS TO COMPUTE SIMULATE LME CIs (ten Hove et al, 2022)
############################################################################

CIs_LME <-  function(model_fit, subjects = "subjects", raters = "raters", ci, k, khat, Q){
  level <- ci
  #two way random effects
  lme_vars <- lme4::VarCorr(model_fit)
  S_s <- lme_vars[[subjects]][1] #obtain object name

  #get not specified random effects variances
  ran_eff <- attr(model_fit@flist,"names")
  ran_eff <- ran_eff[ran_eff != subjects]
  S_r <- lme_vars[[ran_eff]][1]

  #residual/interaction variances
  S_sr <- (attr(lme4::VarCorr(model_fit), "sc"))^2



  ## List all (and create SDs)
  sigmas <- c(S_s = S_s, S_r = S_r, S_sr = S_sr)
  # ICCs <- c(ICCa1 = ICCa1, ICCak = ICCak, ICCakhat = ICCakhat,
  #           ICCc1 = ICCc1, ICCck = ICCck, ICCqkhat = ICCqkhat)
  #
  ## Asymptotic vcov matrix of sigmas
  suppressWarnings(ACOV <- merDeriv::vcov.lmerMod(model_fit, full = TRUE))
  Sidx <- grep(pattern = subjects, colnames(ACOV), fixed = TRUE)
  Ridx <- grep(pattern = raters, colnames(ACOV), fixed = TRUE)
  SRidx <- which(colnames(ACOV) == "residual")
  idx      <- c(   Sidx  ,  Ridx  ,  SRidx  )
  newNames <- c("subject", "rater", "interaction")
  VCOV <- ACOV[idx, idx]
  dimnames(VCOV) <- list(newNames, newNames)
  vars <- c(subject = S_s, rater = S_r, interaction = S_sr)

  ## CIs and SEs of ICCs using asymptotic vcov matrix
  ## All info of all ICCs in one list
  ICCnames <- c("ICCa1", "ICCakhat", "ICCak",
                "ICCc1", "ICCqkhat", "ICCck")

  ICCdefs <- c("subject / (subject + rater + interaction)",
               "subject / (subject + (rater + interaction)/khat)",
               "subject / (subject + (rater + interaction)/k)",
               "subject / (subject + interaction)",
               "subject / (subject + Q*rater + interaction/khat)",
               "subject / (subject + interaction/k)"

  )
  names(ICCdefs) <- ICCnames
  ICCs_dm <- do.call("rbind", lapply(ICCdefs, FUN = function(x){
    car::deltaMethod(vars, vcov. = VCOV, level = level,g. = x)
  }))

  ICC_ses <- ICCs_dm[,"SE"]
  names(ICC_ses) <- paste0(ICCnames, "_se")

  sigma_ses <- do.call("rbind", lapply(newNames, FUN = function(x){
    car::deltaMethod(vars, vcov. = VCOV, level = level,g. = x)
  }))$SE
  names(sigma_ses) <- paste0(names(sigmas), "_se")

  ## Monte-Carlo CIs of variances and ICCs
  dimnames(VCOV) <- list(names(sigmas), names(sigmas))
  sigma_mcCIs <- semTools::monteCarloCI(expr = c(S_s = 'S_s', S_r = "S_r", S_sr = "S_sr"),
                                        coefs = sigmas, ACM = VCOV)
  ICC_mcCIs <- semTools::monteCarloCI(expr = c(ICCa1_ci = "S_s / (S_s + S_r + S_sr)",
                                               ICCak_ci = paste0("S_s / (S_s + (S_r + S_sr)/", k, ")"),
                                               ICCakhat_ci = paste0("S_s / (S_s + (S_r + S_sr)/", khat, ")"),
                                               ICCc1_ci = "S_s / (S_s + S_sr)",
                                               ICCck_ci = paste0("S_s / (S_s + S_sr/", k, ")"),
                                               ICCqkhat_ci = paste0("S_s / (S_s + ", Q, "*S_r + S_sr/", khat, ")")),
                                      coefs = sigmas, ACM = VCOV)

  ICCs <- cbind(ICC_mcCIs, ICC_ses)

}
