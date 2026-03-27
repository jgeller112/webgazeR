# ---- test data ----
set.seed(42)
n_time <- 100
n_subj <- 5

# Shared signal + noise -> positive ISC expected
signal <- sin(seq(0, 4 * pi, length.out = n_time))
mat <- matrix(NA_real_, nrow = n_time, ncol = n_subj)
colnames(mat) <- paste0("P", 1:n_subj)
for (i in seq_len(n_subj)) {
  mat[, i] <- signal + rnorm(n_time, sd = 0.3)
}

# Pure noise -> ISC near zero
noise_mat <- matrix(rnorm(n_time * n_subj), nrow = n_time, ncol = n_subj)
colnames(noise_mat) <- paste0("P", 1:n_subj)

# ---- basic pairwise ----
test_that("pairwise returns named vector of correct length", {
  res <- calculate_isc(mat, method = "pairwise")
  expect_length(res, n_subj)
  expect_named(res, colnames(mat))
  expect_true(all(is.finite(res)))
})

test_that("pairwise ISC is positive for correlated data", {
  res <- calculate_isc(mat, method = "pairwise")
  expect_true(all(res > 0.5))
})

test_that("pairwise ISC is near zero for noise", {
  res <- calculate_isc(noise_mat, method = "pairwise")
  expect_true(all(abs(res) < 0.4))
})

# ---- return_matrix ----
test_that("return_matrix gives full correlation matrix", {
  res <- calculate_isc(mat, method = "pairwise", return_matrix = TRUE)
  expect_true(is.matrix(res))
  expect_equal(dim(res), c(n_subj, n_subj))
  expect_true(all(is.na(diag(res))))
  off_diag <- res[row(res) != col(res)]
  expect_true(all(off_diag > 0.5, na.rm = TRUE))
})

test_that("return_matrix warns for leave-one-out", {
  expect_warning(
    calculate_isc(mat, method = "leave-one-out", return_matrix = TRUE),
    "ignored"
  )
})

# ---- leave-one-out ----
test_that("leave-one-out returns named vector", {
  res <- calculate_isc(mat, method = "leave-one-out")
  expect_length(res, n_subj)
  expect_named(res, colnames(mat))
  expect_true(all(res > 0.5))
})

# ---- cor_method ----
test_that("spearman method works", {
  res <- calculate_isc(mat, method = "pairwise", cor_method = "spearman")
  expect_length(res, n_subj)
  expect_true(all(res > 0.4))
})

# ---- data.frame input ----
test_that("data.frame input is accepted", {
  df <- as.data.frame(mat)
  res <- calculate_isc(df, method = "pairwise")
  expect_length(res, n_subj)
})

# ---- input validation ----
test_that("non-numeric input errors", {
  expect_error(calculate_isc("hello"), "numeric matrix")
})

test_that("single participant errors", {
  expect_error(calculate_isc(mat[, 1, drop = FALSE]), "more than one participant")
})

test_that("invalid method caught by match.arg", {
  expect_error(calculate_isc(mat, method = "bogus"), "should be one of")
})

# ---- consistency: pairwise vs LOO should be in same direction ----
test_that("pairwise and LOO agree in direction", {
  pw <- calculate_isc(mat, method = "pairwise")
  loo <- calculate_isc(mat, method = "leave-one-out")
  expect_true(cor(pw, loo) > 0.8)
})

# ---- edge case: two participants ----
test_that("two participants works", {
  mat2 <- mat[, 1:2]
  res_pw <- calculate_isc(mat2, method = "pairwise")
  res_loo <- calculate_isc(mat2, method = "leave-one-out")
  expect_length(res_pw, 2)
  expect_length(res_loo, 2)
})

# ---- edge case: NAs in data ----
test_that("handles NAs in data", {
  mat_na <- mat
  mat_na[1:10, 1] <- NA
  res <- calculate_isc(mat_na, method = "pairwise")
  expect_length(res, n_subj)
  expect_true(all(is.finite(res)))
})

# ---- edge case: unnamed matrix ----
test_that("unnamed matrix gets default names", {
  mat_noname <- mat
  colnames(mat_noname) <- NULL
  res <- calculate_isc(mat_noname, method = "pairwise")
  expect_named(res, paste0("S", 1:n_subj))
})
