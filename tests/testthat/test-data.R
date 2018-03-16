context("data")

x <- list(basecost, cost, costSummary, fmoba, fmobaSummary, cdratio, cdba, fbxfire, firesize)

test_that("data sets are available", {
  purrr::walk(x, ~expect_is(.x, "tbl_df"))
})
