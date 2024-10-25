#' Process a CDS Report
#'
#' Extract data from a CDS PDF report.
#'
#' @param school Name of the school, to be checked for in the document.
#' @param file Path to the PDF report.
#' @examples
#' file <- "../../reports/text/Yale University/cds2008_2009_1.txt"
#' if (file.exists(file)) {
#'   values <- cds_process_report("Yale University", file)
#'   values[1:10]
#' }
#' @return A \code{list} of data extracted from the report.
#' @export

cds_process_report <- function(school, file) {
  if (!file.exists(file)) stop("file does not exist", call. = FALSE)
  text <- if (grepl("pdf$", file, ignore.case = TRUE)) {
    strsplit(paste(pdftools::pdf_text(file), collapse = "\n"), "\n+")[[1]]
  } else {
    readLines(file)
  }
  text <- text[text != ""]
  text_start <- paste(text[seq_len(100)], collapse = " ")
  if (!grepl("Address Information", text_start, fixed = TRUE)) {
    stop("file does not appear to be a Common Data Set report: ", file, call. = FALSE)
  }
  if (!grepl(tolower(school), tolower(text_start), fixed = TRUE)) {
    message("School name (", school, ") not found in ", file)
  }
  item_starts <- grep("^\\s*[A-Z](?:\\. [A-Z]{2}|[0-9][A-Z0-9-]*)(?:[. ]|$)", text)
  item_ids <- sub("[. ]+[^ ]*$", "", sub("^\\s+", "", unlist(regmatches(
    text[item_starts],
    gregexpr("^\\s*[A-Z](?:\\. [A-Z]{2}|[0-9][A-Z0-9-]*)(?:[. ]|$)", text[item_starts])
  ))))
  item_locations <- list()
  section <- ""
  item <- ""
  for (i in seq_along(item_ids)) {
    id <- item_ids[[i]]
    part <- substr(id, 1, 1)
    if (part != section) {
      if (!is.null(item_locations[[part]])) next
      if (section != "") item_locations[[section]]$end <- item_starts[[i]] - 1
      section <- part
      item_locations[[section]] <- list(
        start = item_starts[[i]],
        end = item_starts[[i]],
        items = list()
      )
    }
    if (!grepl("[0-9]$", id)) next
    if (item != id) {
      item <- id
      existing_items <- item_locations[[section]]$items
      if (
        id == "H1" && !is.null(existing_items[[id]]) && is.null(existing_items$H2) &&
          !is.null(existing_items$H3)
      ) {
        # ignore the part of H1 that is sometimes separated by an early H3
        item_locations[[section]]$items$H1 <- NULL
        item_locations[[section]]$items$H3 <- NULL
      }
      if (!(id %in% names(item_locations[[section]]$items))) {
        item_locations[[section]]$items[[id]] <- list(
          start = item_starts[[i]],
          end = item_starts[[i]]
        )
      }
    }
  }
  item_locations <- lapply(item_locations, function(section) {
    nitems <- length(section$items)
    if (nitems) {
      for (i in seq_len(nitems - 1)) {
        section$items[[i]]$end <- section$items[[i + 1]]$start - 1
      }
      section$items[[nitems]]$end <- section$end
    }
    section
  })
  specs <- jsonlite::read_json(system.file("spec.json", package = "schoolCDS"))
  c(
    year = as.integer(regmatches(text_start, gregexpr("(?:20[012]|199)\\d", text_start))[[1]][[1]]),
    as.list(unlist(lapply(names(specs), function(section) {
      spec <- specs[[section]]
      lapply(names(spec$items), function(item) {
        locs <- item_locations[[section]]$items[[item]]
        if (is.null(locs)) {
          stop("failed to locate ", item)
        }
        parser <- cds_item_parsers[[item]]
        if (!is.null(parser)) {
          part <- text[seq(locs$start, locs$end)]
          part <- part[!grepl(paste(c(
            "^(?:CDS[^ ]+)?\\s+(?:P\\s*a\\s*g\\s*e\\s*\\|\\s*)?\\d+(?: of \\d+)?$",
            "Common Data Set",
            school,
            "^\\s*[^0-9]?\\s*\\d+\\s*[^0-9]?\\s*$",
            "^\\s*\\*",
            "[A-Z][a-z]+\\s{1,2}\\d{1,2},\\s\\d{4}$"
          ), collapse = "|"), part)]
          part <- sub(
            "\\((?:row|page)[ |+-]\\d+[ +-]*(?:row|page)?\\s*\\d*\\)\\s*$", "", part,
            ignore.case = TRUE
          )
          values <- cds_item_parsers[[item]](part)
          names(values) <- paste(item, names(values), sep = "_")
          values
        }
      })
    })))
  )
}
