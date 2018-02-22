context("plot")

p <- jfsp_plot("ba_box", 1950:2013, log = TRUE)

test_that("jfsp_plot returns a ggplot object", {
  expect_is(p, "ggplot")
})
