library(schoolCDS)
source_dir <- "reports"
sources <- read.csv("reports/sources.csv")

# collect links
for (i in seq_len(nrow(sources))) {
  source <- sources[i, ]
  message("retrieving links for ", source$school, "...")
  cds_get_links(source$school, source_dir, source$url, overwrite = TRUE)
}

# download reports
for (school in sources$school) {
  cds_get_reports(school, source_dir)
}

# process reports
manifests <- list.files(source_dir, "manifest", recursive = TRUE, full.names = TRUE)
process_log <- list()
processed <- list()
for (manifest_file in manifests) {
  manifest <- jsonlite::read_json(manifest_file)
  school_dir <- sub("record", "text", dirname(manifest_file))
  school <- basename(school_dir)
  process_log[[school]] <- list()
  for (file in names(manifest)) {
    info <- manifest[[file]]
    path <- paste0(school_dir, "/", sub("pdf$", "txt", file, ignore.case = TRUE))
    if (is.null(process_log[[school]][[info$md5]])) {
      if (file.exists(path)) {
        values <- tryCatch(
          cds_process_report(school, path),
          error = function(e) e$message
        )
        if (is.list(values)) {
          process_log[[school]][[info$md5]] <- "processed"
          processed[[info$md5]] <- c(
            school = school,
            source = info$url,
            md5 = info$md5,
            retrieved = info$retrieved,
            processed = date(),
            values
          )
        } else {
          message("failed to process ", school, " file ", file, ": ", values)
          process_log[[school]][[info$md5]] <- paste("failed:", values)
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
