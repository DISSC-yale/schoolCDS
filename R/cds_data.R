#' Common Data Set Extracts
#'
#' Extracted portions of school Common Data Set reports.
#'
#' @format A \code{data.frame} with a report in each row, and fields in each column.
#' A few columns contain metadata:
#' \describe{
#'   \item{school}{Name of the school.}
#'   \item{source}{URL of the original PDF file.}
#'   \item{md5}{MD5 hash of the original PDF file.}
#'   \item{retieved}{Date downloaded.}
#'   \item{processed}{Date processed.}
#'   \item{year}{Year of the report.}
#' }
#' The rest are named first by section and item number (e.g., B1), then by a combination
#' of levels such as the part, column name, and row name from the original table.
#' @examples
#' cds_data[1:10, c(
#'   "school", "retrieved", "B1_men_undergrad_new_full", "B1_women_undergrad_new_full"
#' )]
"cds_data"
