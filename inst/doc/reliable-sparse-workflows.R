## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(thisutils)
library(Matrix)

## ----sparse-semantics---------------------------------------------------------
x <- Matrix(
  c(-3, 0, 2,
    -1, 4, 0),
  nrow = 3,
  sparse = TRUE,
  dimnames = list(paste0("r", 1:3), paste0("c", 1:2))
)

run_sparse_topk(x, k = 2, by = "col")

## ----graph-semantics----------------------------------------------------------
run_sparse_topk_stored(x, k = 2, by = "col")

## ----sparse-table-------------------------------------------------------------
compact <- matrix_to_table(x, keep_zero = FALSE)
expanded <- matrix_to_table(x)
c(compact_rows = nrow(compact), expanded_rows = nrow(expanded))

## ----stable-variance----------------------------------------------------------
offset <- matrix(1e12 + c(0, 1, 2, 3, 0, 1, 2, 3), nrow = 2, byrow = TRUE)
fast_row_vars(offset)
apply(offset, 1, stats::var)

## ----bounded-correlation------------------------------------------------------
set.seed(1)
a <- rsparsematrix(100, 20, density = 0.1)
b <- rsparsematrix(100, 8, density = 0.1)

cors <- sparse_cor(
  a,
  b,
  threshold = 0.15,
  block_size = 4,
  max_dense_bytes = 1024^2,
  max_output_entries = 200
)
cors

## ----controlled-execution-----------------------------------------------------
datasets <- list(first = a, second = b)
correlate <- function(mat) {
  thisutils::sparse_cor(
    mat,
    threshold = 0.15,
    block_size = 4,
    max_dense_bytes = 1024^2,
    max_output_entries = 400
  )
}

serial <- parallelize_fun(
  datasets,
  correlate,
  cores = 1,
  seed = 2026,
  verbose = FALSE
)
psock <- parallelize_fun(
  datasets,
  correlate,
  cores = 2,
  backend = "psock",
  seed = 2026,
  verbose = TRUE,
  progress = FALSE,
  timestamp_format = ""
)

vapply(
  Map(function(lhs, rhs) isTRUE(all.equal(lhs, rhs)), serial, psock),
  identity,
  logical(1)
)

## ----reproducible-execution---------------------------------------------------
draw <- function(cores) {
  parallelize_fun(
    1:4,
    function(i) stats::rnorm(2),
    cores = cores,
    backend = "psock",
    seed = 42,
    verbose = FALSE
  )
}

identical(draw(1), draw(2))

