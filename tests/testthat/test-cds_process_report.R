skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading file")

test_that("extraction works", {
  file <- tempfile(fileext = ".pdf")
  download.file(
    "https://oir.yale.edu/sites/default/files/cds_yale_2023-24_vf_20240320.pdf", file,
    mode = "wb"
  )
  if (file.exists(file)) {
    values <- cds_process_report("Yale University", file)
    expect_true(values$year == 2023L)
    expect_identical(
      as.integer(values[c("B1_undergrad", "B1_grad", "B1_all")]),
      c(6818L, 8263L, 15081L)
    )
    expect_true(values$B1_men_undergrad_new_full == 785L)
    expect_true(values$G1_total == 87150L)
    expect_true(values$G1_first_year_private == 67250L)
    expect_true(values$G5_resident_book_supplies == 3700L)
    expect_true(values$H1_need_scholarship_federal == 9661257L)
    expect_true(values$H1_non_need_parent_loan == 8334604L)
  }
})
