skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making requests")

test_that("links are extracted in example", {
  dir <- tempdir()
  url <- "https://oir.yale.edu/common-data-set"
  links <- cds_get_links("yale", dir, "https://oir.yale.edu/common-data-set")
  expect_true(length(links$links) != 0)
  expect_true(file.exists(paste0(dir, "/record/yale/links.json")))
  reload <- cds_get_links("yale", dir)
  expect_identical(links, reload)
})
