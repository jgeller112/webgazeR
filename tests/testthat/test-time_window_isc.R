# ---- test data ----
set.seed(123)
n_time <- 100
n_subj <- 4

signal <- sin(seq(0, 4 * pi, length.out = n_time))
mat <- matrix(NA_real_, nrow = n_time, ncol = n_subj)
colnames(mat) <- paste0("P", 1:n_subj)
for (i in seq_len(n_subj)) {
  mat[, i] <- signal + rnorm(n_time, sd = 0.3)
}

noise_mat <- matrix(rnorm(n_time * n_subj), nrow = n_time, ncol = n_subj)

# ---- basic functionality ----
test_that("returns named vector of correct length", {
  res <- time_window_isc(mat, window_size = 20, step = 10)
  expect_length(res, n_subj)
  expect_named(res, colnames(mat))
  expect_true(all(is.finite(res)))
})

test_that("ISC is positive for correlated data", {
  res <- time_window_isc(mat, window_size = 20, step = 10)
  expect_true(all(res > 0.3))
})

test_that("ISC is near zero for noise", {
  res <- time_window_isc(noise_mat, window_size = 20, step = 10)
  expect_true(all(abs(res) < 0.5))
})

# ---- return_per_window ----
test_that("return_per_window returns list with correct structure", {
  res <- time_window_isc(mat, window_size = 20, step = 10, return_per_window = TRUE)
  expect_type(res, "list")
  expect_named(res, c("isc", "per_window"))
  expect_true(is.data.frame(res$per_window))
  expect_true(all(c("window_start", "window_end", "subject", "z") %in% names(res$per_window)))
  expect_true(nrow(res$per_window) > 0)
  # ISC vector should match non-per_window call
  res2 <- time_window_isc(mat, window_size = 20, step = 10, return_per_window = FALSE)
  expect_equal(res$isc, res2)
})

# ---- spearman ----
test_that("spearman method works", {
  res <- time_window_isc(mat, window_size = 20, step = 10, method = "spearman")
  expect_length(res, n_subj)
  expect_true(all(is.finite(res)))
})

# ---- data.frame input ----
test_that("data.frame input is accepted", {
  df <- as.data.frame(mat)
  res <- time_window_isc(df, window_size = 20, step = 10)
  expect_length(res, n_subj)
})

# ---- input validation ----
test_that("non-numeric input errors", {
  expect_error(time_window_isc("hello"), "numeric matrix")
})

test_that("single participant errors", {
  expect_error(time_window_isc(mat[, 1, drop = FALSE]), "more than one participant")
})

test_that("window_size too large errors", {
  expect_error(time_window_isc(mat, window_size = 200), "window_size")
})

test_that("window_size < 2 errors", {
  expect_error(time_window_isc(mat, window_size = 1), "window_size")
})

test_that("step < 1 errors", {
  expect_error(time_window_isc(mat, step = 0), "step")
})

test_that("min_overlap < 2 errors", {
  expect_error(time_window_isc(mat, min_overlap = 1), "min_overlap")
})

# ---- edge case: two participants ----
test_that("two participants works", {
  mat2 <- mat[, 1:2]
  res <- time_window_isc(mat2, window_size = 20, step = 10)
  expect_length(res, 2)
  expect_true(all(is.finite(res)))
})

# ---- edge case: NAs in data ----
test_that("handles NAs in data", {
  mat_na <- mat
  mat_na[1:10, 1] <- NA
  res <- time_window_isc(mat_na, window_size = 20, step = 10)
  expect_length(res, n_subj)
  expect_true(all(is.finite(res)))
})

# ---- step = 1 gives more windows than step = 50 ----
test_that("smaller step yields more per-window rows", {
  res1 <- time_window_isc(mat, window_size = 20, step = 1, return_per_window = TRUE)
  res10 <- time_window_isc(mat, window_size = 20, step = 10, return_per_window = TRUE)
  expect_true(nrow(res1$per_window) > nrow(res10$per_window))
})

# ---- unnamed matrix gets default names ----
test_that("unnamed matrix gets S1, S2, ... names", {
  mat_noname <- mat
  colnames(mat_noname) <- NULL
  res <- time_window_isc(mat_noname, window_size = 20, step = 10)
  expect_named(res, paste0("S", 1:n_subj))
})
