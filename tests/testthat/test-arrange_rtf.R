test_that("Shiny object has right class", {
  x <- arrange_rtf()
  expect_equal(class(x), "shiny.appobj")
})
