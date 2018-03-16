context("plot")

yrs <- 1950:2099
p <- purrr::map2(c(TRUE, TRUE, FALSE, FALSE), c(TRUE, FALSE, TRUE, FALSE), ~list(
    jfsp_plot("ba_box", yrs, log = TRUE, fmo = c("Limited", "Full"), by_rcp = .x, by_tx = .y),
    jfsp_plot("ba_sd", yrs, continuous = TRUE, n = 10, by_rcp = .x, by_tx = .y),
    jfsp_plot("cba", yrs, breaks = seq(1050, 2090, by = 10), by_rcp = .x, by_tx = .y),
    jfsp_plot("cost", yrs, by_rcp = .x, by_tx = .y),
    jfsp_plot("cost_dec", 2040:2089, obs = TRUE, by_rcp = .x, by_tx = .y),
    jfsp_plot("cdratio", yrs, alaska = TRUE, by_rcp = .x, by_tx = .y),
    jfsp_plot("cdba", yrs, by_rcp = .x, by_tx = .y),
    jfsp_plot("pfire", yrs, by_rcp = .x, by_tx = .y),
    jfsp_plot("fs_box", yrs, by_rcp = .x, by_tx = .y)
  )
)

test_that("jfsp_plot runs without error or gives correct error", {
  purrr::walk(p[[1]], ~expect_is(.x, "ggplot"))
  purrr::walk(p[[2]], ~expect_is(.x, "ggplot"))
  purrr::walk(p[[3]], ~expect_is(.x, "ggplot"))
  purrr::walk(p[[4]], ~expect_is(.x, "ggplot"))

  err <- "Years must be in 1950:2099 for JFSP data."
  expect_error(jfsp_plot("ba_box", 2100), err)
  err <- "Projected decadal cost estimates summary data set, `costSummary`, covers 2020 - 2099."
  expect_error(jfsp_plot("cost_dec", 2019:2099), err)
})

test_that("jfsp_plot hidden options and alternate data work", {
  expect_is(jfsp_plot("ba_box", yrs, x = fmoba), "ggplot")
  expect_is(jfsp_plot("ba_box", yrs, dashtype = c("16", "88")), "ggplot")
})

test_that("jfsp_plot color and file args work", {
  expect_is(jfsp_plot("cost", yrs, col = 1:4), "ggplot")
  expect_is(jfsp_plot("ba_box", yrs, file = "plot.png"), "NULL")
  unlink("plot.png")
})
