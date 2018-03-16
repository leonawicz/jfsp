context("docs")

x <- list(basecost, cdratio, fbxfire, fmoba, fmobaSummary, cost, costSummary, firesize)

test_that("datadoc runs without error", {
  expect_is(datadoc(basecost, open_doc = FALSE), "NULL")
  expect_is(datadoc(cdratio, "out.html", open_doc = FALSE), "NULL")
  expect_is(datadoc(fbxfire, "out.pdf", open_doc = FALSE), "NULL")
  expect_is(datadoc(fmoba, "out.docx", open_doc = FALSE), "NULL")
  expect_is(datadoc(fmobaSummary, title = "title", open_doc = FALSE), "NULL")
  expect_is(datadoc(cost, subtitle = "subtitle", open_doc = FALSE), "NULL")
  expect_is(datadoc(costSummary, metadata = TRUE, open_doc = FALSE), "NULL")
  expect_is(datadoc(firesize, keep_rmd = TRUE, open_doc = FALSE), "NULL")
  expect_is(datadoc(cdba, details = FALSE), "NULL")
  unlink(c("firesize.Rmd", list.files(pattern = ".html"), "out.pdf", "out.docx"))
})
