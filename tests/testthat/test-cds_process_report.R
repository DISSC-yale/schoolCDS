test_that("table extraction works", {
  table <- c(
    "T1                  group1",
    "                                            group2",
    "             header1      header2          header2",
    "          (sub-header 1) (sub-header 2)                  header1",
    "                                           (sub-header 3)",
    "row1       1                 3              6             9",
    "row2         2                 4                7",
    "                                                                   10",
    "row3                          5           8"
  )
  manual <- list(
    group1_header1_row1 = 1,
    group1_header1_row2 = 2,
    group1_header2_row1 = 3,
    group1_header2_row2 = 4,
    group1_header2_row3 = 5,
    group2_header1_row1 = 9,
    group2_header1_row2 = 10,
    group2_header2_row1 = 6,
    group2_header2_row2 = 7,
    group2_header2_row3 = 8
  )
  auto <- cds_table_parser(
    table[1:5], table[6:9],
    rows = c(row1 = "row1", row2 = "row2", row3 = "row3"),
    header_groups = list(
      c(group1 = "group1", group2 = "group2"),
      c(header1 = "header1", header2 = "header2")
    )
  )
  expect_identical(auto[names(manual)], manual)
})

skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not downloading file")

test_that("full extraction works", {
  file <- tempfile(fileext = ".pdf")
  download.file(
    "https://oir.yale.edu/sites/default/files/cds_yale_2023-24_vf_20240320.pdf", file,
    mode = "wb"
  )
  if (file.exists(file)) {
    values <- cds_process_report("Yale University", file)
    expect_true(values$year == 2023L)
    expect_identical(
      as.integer(values[c("B1_undergrad", "B1_grad", "B1_total")]),
      c(6818L, 8263L, 15081L)
    )
    expect_true(values$B1_men_full_new_undergrad == 785L)
    expect_true(values$G1_total == 87150L)
    expect_true(values$G1_first_year_private == 67250L)
    expect_true(values$G5_resident_book_supplies == 3700L)
    expect_true(values$H1_need_scholarship_federal == 9661257L)
    expect_true(values$H1_non_need_parent_loan == 8334604L)
  }
})
