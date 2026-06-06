# tests/testthat/test-cosine-match-approx.R
# cosine_match() approx=TRUE path: k-d tree nearest-neighbour via RANN.
# Fully hermetic: synthetic matrices, no embedder, no Python.

# small reproducible matrices ------------------------------------------------
make_mats <- function(seed = 42L) {
  set.seed(seed)
  tgt <- matrix(rnorm(6 * 8), nrow = 6, ncol = 8)
  ref <- matrix(rnorm(10 * 8), nrow = 10, ncol = 8)
  rownames(tgt) <- paste0("t", seq_len(nrow(tgt)))
  rownames(ref) <- paste0("r", seq_len(nrow(ref)))
  list(tgt = tgt, ref = ref)
}

test_that("approx=FALSE (exact) result has correct structure", {
  m   <- make_mats()
  res <- cosine_match(m$tgt, m$ref)
  expect_s3_class(res, "data.table")
  expect_setequal(names(res), c("target", "reference", "similarity", "rank"))
  # all 6 targets × 10 references = 60 rows
  expect_equal(nrow(res), 60L)
  expect_true(all(res$similarity >= -1 - 1e-9 & res$similarity <= 1 + 1e-9))
})

test_that("approx=TRUE returns only rank-1 rows with correct shape", {
  skip_if_not_installed("RANN")
  m   <- make_mats()
  res <- cosine_match(m$tgt, m$ref, approx = TRUE)
  expect_s3_class(res, "data.table")
  expect_setequal(names(res), c("target", "reference", "similarity", "rank"))
  expect_equal(nrow(res), nrow(m$tgt))          # one row per target
  expect_true(all(res$rank == 1L))
  expect_true(all(res$similarity >= -1 - 1e-9 & res$similarity <= 1 + 1e-9))
})

test_that("approx rank-1 matches exact rank-1 for same inputs", {
  skip_if_not_installed("RANN")
  m      <- make_mats()
  exact  <- cosine_match(m$tgt, m$ref)[rank == 1]
  approx <- cosine_match(m$tgt, m$ref, approx = TRUE)

  # same set of (target, reference) pairs at rank 1
  # exact$reference is factor (from melt); coerce to character for comparison
  setorderv(exact,  "target")
  setorderv(approx, "target")
  expect_equal(exact$target,                  approx$target)
  expect_equal(as.character(exact$reference), approx$reference)
  expect_equal(exact$similarity, approx$similarity, tolerance = 1e-4)
})

test_that("approx=TRUE with keep_target_order includes target_order column", {
  skip_if_not_installed("RANN")
  m   <- make_mats()
  res <- cosine_match(m$tgt, m$ref, keep_target_order = TRUE, approx = TRUE)
  expect_true("target_order" %in% names(res))
  expect_equal(res$target_order, seq_len(nrow(m$tgt)))
})

test_that("approx falls back to exact when RANN unavailable", {
  m <- make_mats()
  # mock requireNamespace so RANN appears absent
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if(pkg == "RANN") FALSE else base::requireNamespace(pkg, ...),
    .package = "base"
  )
  # should message and return exact result (60 rows)
  expect_message(
    res <- cosine_match(m$tgt, m$ref, approx = TRUE),
    "falling back to exact"
  )
  expect_equal(nrow(res), 60L)
})
