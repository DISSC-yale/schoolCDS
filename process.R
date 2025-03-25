library(schoolCDS)
source_dir <- "reports"
sources <- read.csv("reports/sources.csv")

# collect links
for (i in seq_len(nrow(sources))) {
  source <- sources[i, ]
  message("retrieving links for ", source$school, "...")
  cds_get_links(source$school, source_dir, source$url)
}

# download reports
for (school in sources$school) {
  cds_get_reports(school, source_dir)
}

# reformat Mistral results
for (file in list.files("reports/mistral", recursive = TRUE, full.names = TRUE)) {
  out_file <- sub("mistral", "mistral_text", sub("json", "txt", file))
  dir.create(dirname(out_file), FALSE)
  write(paste(vapply(
      jsonlite::read_json(file)$pages, "[[", "", "markdown"
  ), collapse = "\n"), out_file)
}

# process reports
mistrel_ocr <- FALSE
manifests <- list.files(source_dir, "manifest", recursive = TRUE, full.names = TRUE)
process_log <- list()
processed <- list()
for (manifest_file in manifests) {
  manifest <- jsonlite::read_json(manifest_file)
  school_dir <- sub("record", if (mistrel_ocr) "mistral" else "text", dirname(manifest_file))
  school <- basename(school_dir)
  process_log[[school]] <- list()
  for (file in names(manifest)) {
    info <- manifest[[file]]
    path <- paste0(school_dir, "/", sub(
      "pdf$", if (mistrel_ocr) "json" else "txt", file, ignore.case = TRUE
    ))
    if (is.null(process_log[[school]][[info$md5]])) {
      if (file.exists(path)) {
        message("trying ", path)
        report <- tryCatch(
          cds_process_report(school, path),
          error = function(e) e$message
        )
        if (is.list(report)) {
          processed[[info$md5]] <- c(
            school = school,
            source = info$url,
            md5 = info$md5,
            retrieved = info$retrieved,
            processed = date(),
            report$values
          )
          process_log[[school]][[info$md5]] <- list(
            file = file,
            items = report$locations,
            values = processed[[info$md5]]
          )
        } else {
          message("failed to process ", school, " file ", file, ": ", report)
          process_log[[school]][[info$md5]] <- paste("failed:", report)
        }
      } else {
        message("file doesn't exist: ", file)
        process_log[[school]][[info$md5]] <- "not found"
      }
    }
  }
}

cds_data <- do.call(rbind, unname(lapply(processed, as.data.frame)))
save(cds_data, file = "data/cds_data.rda", compress = "xz")
write.csv(cds_data, "docs/cds_data.csv", row.names = FALSE)
