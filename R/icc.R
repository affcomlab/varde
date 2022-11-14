icc_a_1 <- function(vs, vr, vsr) {
  vs / (vs + vr + vsr)
}

icc_a_k <- function(vs, vr, vsr, k) {
  vs / (vs + (vr + vsr) / k)
}

icc_c_1 <- function(vs, vr, vsr) {
  vs / (vs + vsr)
}

icc_c_k <- function(vs, vr, vsr, k) {
  vs / (vs + vsr / k)
}

icc_1 <- function(vs, vrins) {
  vs / (vs + vrins)
}

icc_k <- function(vs, vrins, k) {
  vs / (vs + vrins / k)
}

khat <- function(k) {
  (sum(k^-1) / length(k))^-1
}

# function to calculate q

# k
