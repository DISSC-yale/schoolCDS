#' Get CDS Links
#'
#' Scrape links to individual CDS reports from school websites.
#'
#' @param school Name of the school.
#' @param out_dir Path to an output directory.
#' @param url Link to the school's CDS page, containing links to individual reports.
#' @param overwrite Logical; if \code{TRUE}, will always try to retrieve fresh links,
#'   rather than using a previous pull.
#' @examples
#' if (dir.exists("../../reports")) {
#'   report_links <- cds_get_links(
#'     "Yale University", "../../reports", "https://oir.yale.edu/common-data-set"
#'   )
#'   report_links
#' }
#' @return A list, which is also saved as \code{\{out_dir\}/record/\{school\}/links.json}:
#' \itemize{
#'   \item \strong{\code{school}}: Name of the school.
#'   \item \strong{\code{url}}: Link to the school's CDS page.
#'   \item \strong{\code{retrieved}}: Time the links were retrieved.
#'   \item \strong{\code{links}}: A vector of extracted links.
#' }
#' @export

cds_get_links <- function(school, out_dir, url = NULL, overwrite = FALSE) {
  results_file <- paste0(out_dir, "/record/", school, "/links.json")
  if (overwrite || !file.exists(results_file)) {
    if (is.null(url)) stop("specify url", call. = FALSE)
    req <- curl::curl_fetch_memory(url)
    if (req$status_code != 200) {
      stop("request failed: ", req$status_code, call. = FALSE)
    }
    page <- rawToChar(req$content)
    domain <- regmatches(url, gregexpr("[^./]+\\.edu", url, TRUE))[[1]]
    pdf_links <- sub('(?:href|value)="', "", regmatches(
      page, gregexpr('(?:href|value)="[^"]*\\.pdf', page, TRUE)
    )[[1]])
    if (!length(pdf_links)) {
      warning("no PDF links found for ", school)
      return()
    }
    su <- !grepl("http", pdf_links)
    if (any(su)) {
      pdf_links[su] <- paste0(dirname(url), "/", sub("^/", "", pdf_links[su]))
    }
    pdf_links <- grep(domain, pdf_links, value = TRUE, fixed = TRUE)
    dir.create(dirname(results_file), FALSE, TRUE)
    res <- list(school = school, url = url, retrieved = date(), links = pdf_links)
    jsonlite::write_json(res, results_file, auto_unbox = TRUE, pretty = TRUE)
  } else {
    res <- jsonlite::read_json(results_file, simplifyVector = TRUE)
  }
  invisible(res)
}
