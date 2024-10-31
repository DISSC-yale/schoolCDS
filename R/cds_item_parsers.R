cds_item_parsers <- list(
  B1 = function(part) {
    slots <- c(paste(
      rep(c("men", "women", "other"), each = 24L),
      rep(c(
        paste(
          rep(c("full", "part"), each = 6L),
          c("new", "first", "other", "degree", "credit", "total"),
          rep("undergrad", 12L),
          sep = "_"
        ),
        paste(
          rep(c("full", "part"), each = 5L),
          c("new", "other", "degree", "credit", "total"),
          rep("grad", 10L),
          sep = "_"
        ),
        "full_total",
        "part_total"
      ), 3L),
      sep = "_"
    ), "undergrad", "grad", "total")
    slots <- as.list(structure(rep(NA_integer_, length(slots)), names = slots))
    rows <- c(
      first = "Other first",
      other = "other degree",
      degree = "Total degree",
      credit = "All other [UuGgPp]|credit",
      total = "Total",
      new = "first.time",
      full = ":\\s*[Ff]ull.[Tt]ime",
      part = ":\\s*[Pp]art.[Tt]ime"
    )
    headers <- list(
      c(women = "\\s[Ww]omen", men = "\\s[Mm]en", other = "Anot(?:her Gender)?|Other|\\s{2}Gender"),
      c(full = "F(?:ull|ULL).(?:TIME|[Tt]ime)", part = "P(?:ART|art).(?:TIME|[Tt]ime)")
    )
    std_student_type <- function(label) {
      label <- tolower(label)
      if (grepl("all student", label, fixed = TRUE)) {
        "total"
      } else if (grepl("under", label, fixed = TRUE)) {
        "undergrad"
      } else if (grepl("grad", label, fixed = TRUE)) {
        "grad"
      } else {
        ""
      }
    }
    # parse out-of-grid grand totals
    ## consolidate broken lines
    grand_total_lines <- grep(
      "^(?:B1)?[ _]*(?:grand|total|students).*:?[ _*\\(]*$", part,
      ignore.case = TRUE
    )
    number_rows <- grep("(?:[A-Za-z]|^)[ _*\\(:]+[0-9,.]+\\d[ _*\\)]*$", part)
    if (length(number_rows)) {
      orphans <- which(
        number_rows %in% (grand_total_lines - 1L) & !(number_rows %in% grand_total_lines)
      )
      for (orphan in orphans) {
        starter <- part[number_rows[orphan] - 1L]
        part[number_rows[orphan] - 1L] <- paste0(
          starter, substring(part[number_rows[orphan]], nchar(starter))
        )
        part[number_rows[orphan]] <- ""
      }
      number_rows[orphans] <- number_rows[orphans] - 1L
    }
    grand_total_lines <- grand_total_lines[grand_total_lines %in% number_rows]
    if (length(grand_total_lines)) {
      totals <- part[grand_total_lines]
      part <- part[-grand_total_lines]
      for (l in strsplit(sub("^(?:b1)?\\s*", "", tolower(totals)), ":[ _*\\(]*|[ _*\\)]{2,}")) {
        if (length(l) == 2L) {
          label <- std_student_type(l[[1L]])
          if (!(label %in% names(slots))) next
          value <- gsub("^[ _*\\(]+|[ _*\\)]+$", "", l[[2L]])
          if (grepl("\\s", value)) next
          value <- gsub("[^0-9.]+", "", value)
          if (value != "" && value != ".") slots[[label]] <- as.integer(value)
        }
      }
    }
    # identify headers and row groups
    student_ind <- grep(
      "^(?:B1)?\\s*(?:undergr|graduate|total all students|first.professional)",
      part,
      ignore.case = TRUE
    )
    if (!any(grepl("full|part", part[student_ind], ignore.case = TRUE))) {
      student_ind <- student_ind[!duplicated(vapply(part[student_ind], std_student_type, ""))]
    }
    student_ind <- c(student_ind, length(part) + 1L)
    header_inds <- get_header_inds(headers, part)
    if (!length(unlist(header_inds))) {
      return(slots)
    }
    for (i in seq_len(length(student_ind) - 1L)) {
      start <- student_ind[[i]]
      student_type <- std_student_type(part[start])
      if (student_type == "") next
      cpart <- part[seq(start, student_ind[[i + 1L]] - 1L)]
      end <- grep("^[ -]*(?:grand )?total[^0-9]+[0-9.,]+\\s+[0-9]", cpart, ignore.case = TRUE)
      if (!length(end)) end <- length(cpart)
      end <- max(end)
      cpart <- cpart[seq(1L, end)]
      has_values <- grep("\\d\\s*$", cpart)
      if (!length(has_values)) next
      cheaders <- headers
      chead <- lapply(header_inds, function(g) {
        lapply(g, function(l) {
          if (length(l)) {
            l[which.min(abs(l - start))]
          }
        })
      })
      if (!length(unlist(chead[[2L]]))) {
        cheaders[[2L]] <- if (
          any(grepl(headers[[2L]][[1L]], cpart))
        ) {
          "full"
        } else {
          "part"
        }
      }
      values <- tryCatch(
        cds_table_parser(part[unique(unlist(chead))], cpart, rows, cheaders),
        error = function(e) NULL
      )
      if (is.null(values)) next
      if (length(values)) {
        names(values) <- paste0(names(values), "_", student_type)
        for (slot in names(values)) {
          if (slot %in% names(slots)) slots[[slot]] <- values[[slot]]
        }
      }
    }
    slots
  },
  G1 = function(part) {
    slots <- c(paste(
      rep(c("first_year", "undergrad"), each = 10L),
      c(
        "private", "public_district", "public_state", "public_out_state",
        "non_resident", "fees", "food_room", "room", "food", "total"
      ),
      sep = "_"
    ), "total")
    slots <- as.list(structure(rep(NA_integer_, length(slots)), names = slots))
    rows <- c(
      private = "PRIVATE|[Pp]rivate",
      fees = "REQUIRED|[Rr]equired",
      food_room = "AND|[Aa]nd",
      non_resident = "RESIDENT|[Rr]esident|[Ii]nternational",
      public_district = "[Ii]n.[Dd]istrict|PUBLIC|[Pp]ublic",
      public_state = "[Ii]n.[Ss]tate",
      public_out_state = "[Oo]f.[Ss]tate",
      room = "(?:HOUSING|[Hh]ousing|ROOM|[Rr]oom) (?:ONLY|[Oo]nly)",
      food = "(?:BOARD|[Bb]oard|FOOD|[Ff]ood) (?:ONLY|[Oo]nly)"
    )
    headers <- list(c(
      first_year = "FIRST.YEAR|First.[Yy]ear",
      undergrad = "UNDERGRADUATES|Undergraduates"
    ))

    # get separate comprehensive value
    comprehensive <- grep("comprehensive|other:", part, ignore.case = TRUE)
    if (length(comprehensive) == 2L) {
      end <- strsplit(paste(
        tolower(part[seq(comprehensive[[1L]], comprehensive[[2L]])]),
        collapse = " "
      ), "other:")[[1L]][[1L]]
      value <- regmatches(end, gregexpr("[0-9,]+", end))[[1L]]
      if (length(value) == 1L) {
        value <- gsub("^\\s+|\\s+$", "", value)
        if (!grepl("\\s", value)) {
          value <- gsub("[^0-9.]+", "", value)
          if (value != "" && value != ".") {
            slots[["total"]] <- as.integer(value)
          }
        }
      }
    }
    end <- grep("board only|meal plan", part, ignore.case = TRUE)
    if (length(end)) part <- part[seq_len(max(end))]
    header_ind <- get_header_inds(headers, part)
    if (!length(unlist(header_ind))) {
      return(slots)
    }
    # align public parts
    off_row_labels <- grep("(in|out.of).state[^0-9]*$", part, ignore.case = TRUE)
    if (length(off_row_labels)) {
      off_row_values <- grep("^[0-9,. ]+$", part)
      off_row_values <- off_row_values[off_row_values %in% (off_row_labels - 1L)]
      if (length(off_row_values) == length(off_row_labels)) {
        part[off_row_labels] <- paste0(
          part[off_row_labels], substring(part[off_row_values], nchar(part[off_row_labels]))
        )
        part[off_row_values] <- ""
      }
    }

    part_inds <- sort(unique(c(unlist(header_ind), length(part) + 1L)))
    chunks <- list()
    ci <- 0L
    last_ind <- part_inds[[1L]]
    for (i in part_inds) {
      if (ci && abs(last_ind - i) < 2L) {
        chunks[[ci]] <- c(chunks[[ci]], i)
      } else {
        ci <- ci + 1L
        chunks[[ci]] <- i
      }
    }
    for (i in seq_len(length(chunks) - 1L)) {
      chead <- chunks[[i]]
      values <- tryCatch(
        cds_table_parser(
          part[chead], part[seq(max(chead), min(chunks[[i + 1L]]) - 1L)], rows, headers
        ),
        error = function(e) NULL
      )
      if (length(values)) {
        for (slot in names(values)) {
          if (slot %in% names(slots)) slots[[slot]] <- values[[slot]]
        }
      }
    }
    slots
  },
  G5 = function(part) {
    slots <- paste(
      rep(c("resident", "commuter_home", "commuter_not_home"), each = 6L),
      c("book_supplies", "room", "food", "room_food", "transport", "other"),
      sep = "_"
    )
    slots <- as.list(structure(rep(NA_integer_, length(slots)), names = slots))
    rows <- c(
      transport = "[Tt]ransportation",
      other = "[Oo]ther [Ee]xpenses",
      book_supplies = "[Bb]ooks\\s*(?:[Aa]nd|,|&)\\s+(?:[Ss]upplies|[Pp]ersonal)",
      room = "(?:[Rr]oom|[Hh]ousing) [Oo]nly",
      food = "(?:[Bb]oard|[Ff]ood) [Oo]nly",
      room_food = " [Aa]nd "
    )
    headers <- list(c(
      resident = "Residents",
      commuter_home = "\\([Ll]iving[^\\(\\)]*(?:\\)|$)",
      commuter_not_home = "\\([Nn]ot[^\\(\\)]*(?:\\)|$)"
    ))
    header_ind <- unique(unlist(get_header_inds(headers, part)))
    if (length(header_ind)) {
      values <- tryCatch(
        cds_table_parser(part[header_ind], part[-seq(1L, max(header_ind))], rows, headers),
        error = function(e) NULL
      )
      if (length(values)) {
        for (slot in names(values)) {
          if (slot %in% names(slots)) slots[[slot]] <- values[[slot]]
        }
      }
    }
    slots
  },
  H1 = function(part) {
    slots <- paste(
      rep(c("need", "non_need"), each = 12L),
      c(
        "scholarship_federal", "scholarship_state", "scholarship_institutional",
        "scholarship_external", "scholarship_total", "self_loan", "self_federal", "self_other",
        "self_total", "parent_loan", "waiver", "athletic"
      ),
      sep = "_"
    )
    slots <- as.list(structure(rep(NA_integer_, length(slots)), names = slots))
    rows <- c(
      self_other = "State and",
      self_federal = "[Ww]ork.[Ss]tudy",
      scholarship_institutional = "Endowed",
      self_loan = "Student loans",
      parent_loan = "Parent Loans",
      scholarship_external = "external",
      scholarship_total = "Total [Ss]cholarships",
      self_total = "Total [Ss]elf",
      scholarship_state = "State (?:[Aa]l|\\()",
      scholarship_federal = "Federal\\s{2}",
      waiver = "Waivers",
      athletic = "Athletic"
    )
    headers <- list(c(need = "Need[^ ]*", non_need = "Non[^ ]*"))
    last <- grep("athletic awards", part, ignore.case = TRUE)
    if (length(last) != 1L) {
      stop("H1: failed to identify end of table", call. = FALSE)
    }
    part <- part[seq_len(last)]
    header_ind <- get_header_inds(headers, part)
    part_inds <- unique(c(unlist(header_ind), length(part) + 1L))
    chunks <- list()
    ci <- 0L
    last_ind <- part_inds[[1L]]
    for (i in part_inds) {
      if (ci && abs(last_ind - i) < 5L) {
        chunks[[ci]] <- c(chunks[[ci]], i)
      } else {
        ci <- ci + 1L
        chunks[[ci]] <- i
      }
    }
    for (i in seq_len(length(chunks) - 1L)) {
      chead <- chunks[[i]]
      values <- tryCatch(
        cds_table_parser(
          part[chead], part[seq(max(chead) + 1L, min(chunks[[i + 1L]]) - 1L)], rows, headers
        ),
        error = function(e) NULL
      )
      if (length(values)) {
        for (slot in names(values)) {
          if (slot %in% names(slots)) slots[[slot]] <- values[[slot]]
        }
      }
    }
    slots
  }
)

cds_table_parser <- function(header, body, rows, header_groups) {
  if (!is.list(header_groups)) header_groups <- list(header_groups)
  width <- max(nchar(c(header, body)))
  header_spans <- lapply(header_groups, function(headers) {
    if (length(headers) == 1L) {
      return(if (is.character(headers)) headers else names(headers))
    }
    label_pos <- do.call(rbind, lapply(names(headers), function(label) {
      pos <- gregexpr(headers[[label]], header)
      p <- do.call(rbind, lapply(seq_along(pos), function(i) {
        data.frame(
          label = label,
          row = i,
          start = as.integer(pos[[i]]),
          length = attr(pos[[i]], "match.length")
        )
      }))
      p[p$start != -1L, ]
    }))
    if (nrow(label_pos)) {
      label_pos <- label_pos[order(label_pos$start), ]
      label_pos$end <- label_pos$start + label_pos$length
      nmathes <- nrow(label_pos)
      if (nmathes > 1L) {
        for (i in seq(1L, nmathes - 1L)) {
          label_pos$end[[i]] <- label_pos$start[[i + 1L]] - 1L
        }
      }
      label_pos$end[[nmathes]] <- width
      label_pos$rcenter <- if (nrow(label_pos) > 3) {
        (label_pos$start + label_pos$end) / 2L
      } else {
        label_pos$start + label_pos$length / 2L
      }
    }
    label_pos
  })
  claimed <- NULL
  row_spans <- list()
  last_start <- 0L
  for (i in seq_along(rows)) {
    row_pos <- data.frame(label = names(rows)[[i]], start = -1L, end = -1L)
    start <- grep(rows[[i]], body)
    start <- start[!(start %in% claimed)]
    if (length(start)) {
      start <- start[[1L]]
      claimed <- c(claimed, start)
      row_pos$start[[1L]] <- start
      row_spans[[i]] <- row_pos
      last_start <- start
    }
  }
  if (!length(row_spans)) {
    return(NULL)
  }
  row_spans <- do.call(rbind, row_spans)
  row_spans <- row_spans[order(row_spans$start), ]
  nmatches <- nrow(row_spans)
  if (nmatches > 1L) {
    for (i in seq(1L, nmatches - 1L)) {
      row_spans$end[[i]] <- row_spans$start[[i + 1L]] - 1L
    }
  }
  row_spans$end[[nmatches]] <- length(body)
  value_pos <- gregexpr("[0-9.][0-9.,]*", body)
  row_start <- vapply(
    gregexpr("^(?:[A-Z][A-Z0-9-]*)?\\s*(?:[^ ]+[A-Za-z][^0-9]*)?(?:$|\\s{2})", body),
    attr, 0L, "match.length"
  )
  values <- list()
  for (r in seq_along(value_pos)) {
    pos <- value_pos[[r]]
    valid <- which(pos != -1L & pos > row_start[[r]])
    if (length(valid)) {
      row <- r >= row_spans$start
      if (!any(row)) next
      len <- attr(pos, "match.length")
      row <- row_spans$label[[max(which(row))]]
      for (i in valid) {
        span <- c(pos[[i]], pos[[i]] + len[[i]] - 1L)
        value <- gsub(",", "", substr(body[[r]], span[[1L]], span[[2L]]), fixed = TRUE)
        if (value != ".") {
          rcenter <- mean(span)
          headers <- vapply(header_spans, function(s) {
            if (is.character(s)) {
              return(s)
            }
            if (nrow(s)) {
              s$label[if (nrow(s) == 1L) {
                1L
              } else {
                which.min(abs(s$rcenter - rcenter))
              }]
            } else {
              ""
            }
          }, "")
          id <- paste(c(headers[headers != ""], row), collapse = "_")
          values[[id]] <- as.numeric(value)
        }
      }
    }
  }
  values
}

get_header_inds <- function(headers, part) {
  lapply(headers, function(h) {
    lapply(h, function(l) {
      which(
        grepl(l, part) & grepl("\\s{2}", part) & !grepl("\\d$", part)
      )
    })
  })
}
