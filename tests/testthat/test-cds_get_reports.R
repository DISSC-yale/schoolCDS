skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making requests")

test_that("reports are downloaded in example", {
  dir <- tempdir()
  links <- cds_get_links("yale", dir, "https://oir.yale.edu/common-data-set")
  manifest <- cds_get_reports("yale", dir)
  expect_identical(length(manifest), length(links$links))
  expect_true(all(names(manifest) %in% basename(links$links)))
  expect_identical(manifest, cds_get_reports("yale", dir))
})
