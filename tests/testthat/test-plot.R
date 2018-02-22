context("plot")

p <- jfsp_plot(fmoba, 1950:2013, "ba_box", log = TRUE)

test_that("jfsp_plot returns a ggplot object", {
  expect_is(p, "ggplot")
})
