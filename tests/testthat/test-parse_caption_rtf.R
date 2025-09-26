test_that("single table caption returned", {
  library(r2rtf)
  file <- tempfile(fileext = ".rtf")
  file1 <- head(iris) %>%
    rtf_title(title = c("Table 1. My table", "Table 2. My other table")) %>%
    rtf_body() %>%
    rtf_encode() %>%
    write_rtf(file)
  x <- striprtf::read_rtf(file)

  res_caption <- parse_caption_rtf(x)
  xl <- length(res_caption)

  expect_equal(xl, 1)
})



