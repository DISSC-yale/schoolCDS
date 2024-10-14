#' Download CDS Reports
#'
#' Download the CDS PDF reports from a given school,
#' using URLs retrieved with \code{\link{cds_get_links}}.
#'
#' @param school Name of the school.
#' @param out_dir Path to an output directory, which should contain the output of
#'   \code{\link{cds_get_links}} in a \code{record/{school}} sub-directory.
#' @param extract Logical; if \code{FALSE}, will not save text from the PDF file.
#' @param check Logical; if \code{FALSE}, will not check if the file is a Common Data Set report.
#' @param overwrite Logical; if \code{TRUE}, will always try to download all reports.
#' @param verbose Logical; if \code{FALSE}, will not show status messages.
#' @examples
#' if (dir.exists("../../reports")) {
#'   manifest <- cds_get_reports("Yale University", "../../reports")
#'   manifest[1:2]
#' }
#' @return A list with an entry for each report, which is also saved as
#' \code{\{out_dir\}/record/\{school\}/manifest.json}:
#' \itemize{
#'   \item \strong{\code{url}}: Link to the report.
#'   \item \strong{\code{retrieved}}: Time the report was downloaded.
#'   \item \strong{\code{md5}}: MD5 sum of the report.
#' }
#' @export

cds_get_reports <- function(
    school, out_dir, extract = TRUE, check = TRUE, overwrite = FALSE, verbose = TRUE) {
  dirs <- paste0(out_dir, "/", c("raw", "text", "record"), "/", school, "/")
  for (d in dirs) dir.create(d, FALSE, TRUE)
  links_file <- paste0(dirs[[3L]], "links.json")
  if (!file.exists(links_file)) {
    stop(paste0(
      "links file (", links_file, ") does not exist;\n",
      "use `get_report_links(", school, ", <url listing reports>, ", out_dir, ")` to create it."
    ))
  }
  links <- jsonlite::read_json(links_file)
  manifest_file <- paste0(dirs[[3L]], "manifest.json")
  manifest <- if (file.exists(manifest_file)) jsonlite::read_json(manifest_file) else list()
  report_files <- names(manifest)
  if (verbose) message("checking ", length(links$links), " files from ", school, "...")
  for (link in links$links) {
    file_name <- basename(link)
    if (!(file_name %in% report_files)) {
      manifest[[file_name]] <- list(url = link, retrieved = "", md5 = "", removed = FALSE)
    }
    report_file <- paste0(dirs[[1L]], file_name)
    if (
      overwrite || !file.exists(report_file) || manifest[[file_name]]$retrieved == "" && (
        is.null(manifest[[file_name]]$removed) || !manifest[[file_name]]$removed
      )) {
      if (verbose) message("  - downloading ", file_name)
      req <- tryCatch(
        curl::curl_fetch_disk(utils::URLencode(link), report_file),
        error = function(e) list(status_code = 000)
      )
      if (req$status_code == 200) {
        manifest[[file_name]]$retrieved <- date()
        manifest[[file_name]]$md5 <- unname(tools::md5sum(report_file))
        jsonlite::write_json(manifest, manifest_file, auto_unbox = TRUE)
      } else {
        manifest[[file_name]]$removed <- TRUE
        unlink(report_file)
        warning(school, " request failed for ", file_name)
      }
    } else {
      if (verbose) message("  - ", file_name, " retrieved ", manifest[[file_name]]$retrieved)
    }
    text_file <- paste0(dirs[[2L]], sub("pdf$", "txt", file_name, ignore.case = TRUE))
    if (extract && file.exists(report_file) && (overwrite || !file.exists(text_file))) {
      text <- tryCatch(
        paste(pdftools::pdf_text(report_file), collapse = "\n"),
        error = function(e) NULL
      )
      if (!length(text)) {
        manifest[[file_name]]$removed <- TRUE
        unlink(report_file)
        if (verbose) message("    failed to read file: ", link)
      } else {
        writeLines(text, text_file)
      }
    }
    if (
      check && file.exists(text_file) &&
        !grepl("Address Information", paste(readLines(text_file), collapse = "\n"), fixed = TRUE)
    ) {
      manifest[[file_name]]$removed <- TRUE
      unlink(report_file)
      unlink(text_file)
      if (verbose) {
        message("    file does not appear to be a Common Data Set report: ", link)
      }
    }
  }
  jsonlite::write_json(manifest, manifest_file, auto_unbox = TRUE, pretty = TRUE)
  invisible(manifest)
}
