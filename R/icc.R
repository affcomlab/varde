icc_a_1 <- function(vs, vr, vsr) {
  vs / (vs + vr + vsr)
}

icc_a_k <- function(vs, vr, vsr, k) {
  vs / (vs + (vr + vsr) / k)
}

icc_c_1 <- function(vs, vsr) {
  vs / (vs + vsr)
}

icc_c_k <- function(vs, vsr, k) {
  vs / (vs + vsr / k)
}

icc_a_khat <- function(vs, vr, vsr, khat) {
  vs / (vs + (vr + vsr) / khat)
}

icc_q_khat <- function(vs, vr, vsr, q, khat) {
  vs / (vs + (q * vr) + (vsr / khat))
}

khat <- function(ks) {
  (sum(ks^-1) / length(ks))^-1
}

# srm <- matrix(sample(0:1, size = 30, replace = TRUE), nrow = 10)

qkhat <- function(srm) {
  n <- nrow(srm)
  ks <- rowSums(srm)
  spairs <- combn(n, 2)

  numer <- function(spair, srm) {
    s <- spair[[1]]
    sp <- spair[[2]]
    ks <- rowSums(srm)
    ks_s <- ks[[s]]
    ks_sp <- ks[[sp]]
    ks_ssp <- sum(colSums(srm[c(s, sp), ]) > 1)
    ks_ssp / (ks_s * ks_sp)
  }

  numerator <- sum(apply(X = spairs, MARGIN = 2, FUN = numer, srm))
  (1 / khat(ks)) - (numerator / (n * (n - 1)))
}
