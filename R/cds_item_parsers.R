cds_item_parsers <- list(
  B1 = function(part) {
    slots <- c(paste(
      rep(c("men", "women", "other"), each = 22L),
      rep(c(
        paste(
          rep("undergrad", 12L),
          c("new", "first", "other", "degree", "credit", "total"),
          rep(c("full", "part"), each = 6L),
          sep = "_"
        ),
        paste(
          rep("grad", 8L),
          c("new", "other", "credit", "total"),
          rep(c("full", "part"), each = 4L),
          sep = "_"
        ),
        "all_full",
        "all_part"
      ), 3L),
      sep = "_"
    ), "undergrad", "grad", "all")
    slots <- as.list(structure(rep(NA_integer_, length(slots)), names = slots))
    std_time <- function(label) {
      label <- tolower(label)
      if (grepl("full", label, fixed = TRUE)) {
        "full"
      } else if (grepl("part", label, fixed = TRUE)) {
        "part"
      } else {
        ""
      }
    }
    std_student_type <- function(label) {
      label <- tolower(label)
      if (grepl("all student", label, fixed = TRUE)) {
        "all"
      } else if (grepl("under", label, fixed = TRUE)) {
        "undergrad"
      } else if (grepl("grad", label, fixed = TRUE)) {
        "grad"
      } else {
        ""
      }
    }
    std_row_id <- function(row_id) {
      row_id <- tolower(row_id)
      if (grepl("other first", row_id, fixed = TRUE)) {
        "first"
      } else if (grepl("other degree", row_id, fixed = TRUE)) {
        "other"
      } else if (grepl("total degree", row_id, fixed = TRUE)) {
        "degree"
      } else if (grepl("all other", row_id, fixed = TRUE)) {
        "credit"
      } else if (grepl("total", row_id, fixed = TRUE)) {
        "total"
      } else if (grepl("first.time", row_id)) {
        "new"
      } else if (grepl(":\\s*full.time", row_id)) {
        "full"
      } else if (grepl(":\\s*part.time", row_id)) {
        "part"
      } else {
        ""
      }
    }
    std_sex <- function(label) {
      label <- tolower(label)
      if (grepl("women", label, fixed = TRUE)) {
        "women"
      } else if (grepl("men", label, fixed = TRUE)) {
        "men"
      } else if (grepl("other", label, fixed = TRUE)) {
        "other"
      } else {
        ""
      }
    }
    # parse out-of-grid grand totals
    grand_total_lines <- which(grepl(
      "^\\s*(?:grand|total|students).*[: ]\\s*[0-9.,]+\\d$", part,
      ignore.case = TRUE
    ) & !grepl("\\d\\s+\\d", part))
    if (length(grand_total_lines)) {
      totals <- part[grand_total_lines]
      part <- part[-grand_total_lines]
      for (l in strsplit(sub("^\\s*", "", tolower(totals)), ":\\s*|\\s{2,}")) {
        if (length(l) == 2L) {
          label <- std_student_type(l[[1L]])
          if (!(label %in% names(slots))) next
          value <- gsub("^\\s+|\\s+$", "", l[[2L]])
          if (grepl("\\s", value)) next
          value <- gsub("[^0-9.]+", "", value)
          if (value != "" && value != ".") slots[[label]] <- as.integer(value)
        }
      }
    }
    # identify headers and row groups
    time_ind <- grep("(?:full|part).time\\s+(?:full|part).time", part, ignore.case = TRUE)
    sex_ind <- grep("men\\s+(?:wo)?men", part, ignore.case = TRUE)
    if (!length(sex_ind)) stop("B1: failed to find sex header row(s)", call. = FALSE)
    student_ind <- grep(
      "^\\s*(?:undergr|graduate|total all|first.professional)[^0-9]+$",
      part,
      ignore.case = TRUE
    )
    student_ind <- student_ind[vapply(
      strsplit(sub("^\\s+", "", part[student_ind]), "\\s{2,}"), function(l) length(unique(l)), 0L
    ) == 1L]
    if (!any(grepl("full|part", part[student_ind], ignore.case = TRUE))) {
      student_ind <- student_ind[!duplicated(vapply(part[student_ind], std_student_type, ""))]
    } else {

    }
    student_ind <- c(student_ind, length(part) + 1L)
    for (i in seq_len(length(student_ind) - 1L)) {
      start <- student_ind[[i]]
      student_type <- std_student_type(part[start])
      cpart <- part[seq(start, student_ind[[i + 1L]] - 1L)]
      end <- grep("^\\s*total[^0-9]+[0-9.,]+\\s+[0-9]", cpart, ignore.case = TRUE)
      if (!length(end)) end <- length(cpart)
      end <- max(end)
      cpart <- cpart[seq(1L, end)]
      ti <- if (length(time_ind)) time_ind[which.min(abs(start - time_ind))] else 0L
      si <- sex_ind[which.min(abs(start - sex_ind))]
      nested <- grepl(": (?:full|part)|total all student", cpart[1L], ignore.case = TRUE)
      within_time <- !nested && ti > si
      std_parent <- if (nested || within_time) std_sex else std_time
      header <- part[if (within_time) ti else si]
      has_values <- grep("\\d\\s*$", cpart)
      if (!length(has_values)) next
      row_label_pos <- max(vapply(
        gregexpr("^(?:G5)?\\s*(?:[^ 0-9]+(?:\\s|$))+", cpart[has_values]), attr, 0L, "match.length"
      ))
      row_label <- substr(cpart, 1L, row_label_pos)
      row_values <- substring(c(header, cpart[-1L]), row_label_pos + 1L)
      row <- rep(" ", max(vapply(row_values, nchar, 0L)))
      any_content <- which(colMeans(do.call(rbind, lapply(strsplit(row_values, ""), function(r) {
        row[seq_along(r)] <- r
        row
      })) == " ") != 1L)
      n_contentful <- length(any_content)
      colnames <- strsplit(sub("^(?:B1)?\\s+", "", header), "\\s{2,}")[[1]]
      if (std_parent(colnames[[1L]]) == 1) colnames <- colnames[-1]
      row_values <- row_values[seq_len(end[[1]])]
      col_spans <- list()
      last_ind <- 1L
      std_col <- if (within_time) std_time else std_sex
      label_ind <- 0L
      for (colname in colnames) {
        label <- std_col(colname)
        if (label != "") {
          label <- paste0(label, label_ind)
          label_ind <- label_ind + 1L
          hit <- FALSE
          if (last_ind < n_contentful) {
            for (ci in seq(last_ind, n_contentful - 1L)) {
              if (!any(any_content[[ci]] == any_content[[ci + 1L]] - c(1L, 2L))) {
                col_spans[[label]] <- c(any_content[[last_ind]], any_content[[ci]])
                last_ind <- ci + 1L
                hit <- TRUE
                break
              }
            }
          }
          if (!hit) {
            col_spans[[label]] <- c(any_content[[last_ind]], any_content[[length(any_content)]])
          }
        }
      }
      ncols <- length(col_spans)
      row_spans <- list()
      last_id <- ""
      last_pos <- 0L
      for (r in rev(seq_along(row_label))) {
        row_id <- gsub("^\\s+|\\s+$", "", row_label[[r]])
        if (grepl("^\\s*[A-Z]", row_id)) {
          id <- std_row_id(paste(row_id, last_id))
          if (id == "") next
          if (last_pos == 0L) last_pos <- r
          row_spans[[id]] <- c(r, last_pos)
          last_id <- ""
          last_pos <- 0L
        } else {
          if (last_id == "") {
            last_id <- row_id
            last_pos <- r
          } else {
            last_id <- paste(row_id, last_id)
          }
        }
      }
      if (nested) {
        parents <- rep(std_time(cpart[[1L]]), length(row_spans))
      } else {
        pcols <- vapply(strsplit(part[if (within_time) si else ti], "\\s{2,}")[[1]], std_parent, "")
        pcols <- pcols[pcols != ""]
        parents <- rep(pcols, each = length(col_spans) / length(pcols))
      }
      for (r in seq_along(row_spans)) {
        row_id <- names(row_spans)[[r]]
        span <- row_spans[[r]]
        values <- row_values[seq(span[[1]], span[[2]])]
        values <- values[values != ""]
        if (length(values) == 1) {
          for (col in seq_along(col_spans)) {
            label <- paste(c(
              sub("\\d$", "", names(col_spans)[[col]]), student_type, row_id,
              if (parents[[col]] != "") parents[[col]]
            )[if (within_time) c(4, 2, 3, 1) else TRUE], collapse = "_")
            if (!(label %in% names(slots))) next
            crange <- col_spans[[col]]
            value <- gsub("^\\s+|\\s+$", "", substr(values, crange[[1]], crange[[2]]))
            if (grepl("[A-Za-z]", value)) next
            if (grepl("\\s", value)) next
            value <- gsub("[^0-9.]+", "", value)
            if (value != "" && value != ".") slots[[label]] <- as.integer(value)
          }
        }
      }
    }
    slots
  },
  G1 = function(part) {
    slots <- c(paste(
      rep(c("first_year", "undergraduate"), each = 10L),
      c(
        "private", "public_district", "public_state", "public_out_state",
        "non_resident", "fees", "food_room", "room", "food", "total"
      ),
      sep = "_"
    ), "total")
    slots <- as.list(structure(rep(NA_integer_, length(slots)), names = slots))
    std_row_id <- function(row_id) {
      row_id <- tolower(row_id)
      if (grepl("private", row_id, fixed = TRUE)) {
        "private"
      } else if (grepl("required", row_id, fixed = TRUE)) {
        "fees"
      } else if (grepl("and", row_id, fixed = TRUE)) {
        "food_room"
      } else if (grepl("resident|international", row_id)) {
        "non_resident"
      } else if (grepl("in.district|public", row_id)) {
        "public_district"
      } else if (grepl("in.state", row_id)) {
        "public_state"
      } else if (grepl("of.state", row_id)) {
        "public_out_state"
      } else if (grepl("(?:housing|room) only", row_id)) {
        "room"
      } else if (grepl("(?:board|food) only", row_id)) {
        "food"
      } else {
        ""
      }
    }
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
          if (value != "" && value != ".") slots[["total"]] <- as.integer(value)
        }
      }
    }
    end <- grep("board only|meal plan", part, ignore.case = TRUE)
    if (length(end)) {
      part <- part[seq_len(max(end))]
    }
    part <- gsub("$", " ", part, fixed = TRUE)
    head_ind <- grep(
      "(?:first.year|undergraduate)\\s+(?:first.year|undergraduate)", part,
      ignore.case = TRUE
    )
    headers <- strsplit(part[head_ind[[1L]]], "\\s+")[[1L]]
    headers <- if (grepl("under", tolower(headers[length(headers)]))) {
      c("first_year", "undergraduate")
    } else {
      c("undergraduate", "first_year")
    }
    head_ind <- c(head_ind, length(part) + 1L)
    for (hi in seq_len(length(head_ind) - 1L)) {
      value_part <- strsplit(
        sub("^(?:G1)?\\s+", "", part[seq(head_ind[[hi]] + 1L, head_ind[[hi + 1L]] - 1L)]),
        "\\s{2,}"
      )
      value_parts <- part[seq(head_ind[[hi]] + 1L, head_ind[[hi + 1L]] - 1L)]
      value_rows <- grep("\\d$", value_parts)
      value_parts[value_rows] <- paste0(
        value_parts[value_rows], "L", nchar(value_parts[value_rows])
      )
      value_part <- lapply(strsplit(value_parts, "\\s{2,}(?=[0-9])", perl = TRUE), function(r) {
        r[[1L]] <- sub("^(?:G1)?\\s+", "", r[[1L]])
        r
      })
      header_label <- strsplit(part[head_ind[[hi]]], "\\s{2,}")[[1]]
      header_label <- if (length(header_label) > 2L) {
        header_label[[1]]
      } else {
        ""
      }
      if (length(value_part) > 1L) {
        for (i in seq(length(value_part), 2L)) {
          if (grepl("^\\d", value_part[[i]][[1L]])) {
            value_part[[i - 1L]] <- c(value_part[[i - 1L]], value_part[[i]])
            value_part[[i]] <- NULL
          } else if (
            length(value_part[[i - 1L]]) == 1L && grepl("^[A-Z]{2}", value_part[[i - 1L]][[1L]])
          ) {
            value_part[[i - 1L]] <- c(value_part[[i - 1L]], value_part[[i]][-1L])
            value_part[[i - 1L]][[1L]] <- paste(value_part[[i - 1L]][[1L]], value_part[[i]][[1L]])
            value_part[[i]] <- NULL
          } else if (!grepl("^[A-Z]", value_part[[i]][[1]]) || (
            length(value_part[[i]]) == 1L && !grepl("^[A-Z]{2}", value_part[[i]])
          )) {
            if (grepl("^\\d", value_part[[i - 1L]][[1L]])) {
              value_part[[i - 1L]] <- c(value_part[[i]][[1L]], value_part[[i - 1L]])
              value_part[[i]] <- NULL
            } else if (
              grepl("^[A-Z]{2}", value_part[[i - 1L]][[1L]]) ||
                !grepl(":$", value_part[[i - 1L]][[1L]])
            ) {
              value_part[[i - 1L]][[1L]] <- paste(
                value_part[[i - 1L]][[1L]], value_part[[i]][[1L]]
              )
              value_part[[i]] <- NULL
            }
          }
        }
      }
      header_edge <- gregexpr("\\s[^ ]{2,}$", part[[head_ind[[hi]]]])[[1]]
      for (l in value_part) {
        if (length(l) > 1L) {
          ids <- paste(headers, std_row_id(paste(header_label, l[[1L]])), sep = "_")
          if (any(!(ids %in% names(slots)))) {
            stop(
              "G1: unrecognized value name ", paste(ids, collapse = " or "),
              call. = FALSE
            )
          }
          values <- l[-1]
          if (length(values) == 1L) {
            olen <- as.integer(strsplit(values, "L")[[1L]][[2L]])
            if (olen > header_edge) values <- c("", values)
          }
          values <- gsub("[^0-9.]", "", sub("L\\d+$", "", values))
          slots[[ids[[1L]]]] <- as.integer(values[[1L]])
          if (length(values) > 1L) slots[[ids[[2L]]]] <- as.integer(values[[2L]])
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
    std_part <- function(label) {
      label <- tolower(label)
      if (grepl("transportation", label, fixed = TRUE)) {
        "transport"
      } else if (grepl("other expenses", label, fixed = TRUE)) {
        "other"
      } else if (grepl("books\\s*(?:and|,|&)\\s+(?:supplies|personal)", label)) {
        "book_supplies"
      } else if (grepl("(?:room|housing) only", label)) {
        "room"
      } else if (grepl("(?:board|food) only", label)) {
        "food"
      } else if (grepl(" and ", label, fixed = TRUE)) {
        "room_food"
      } else {
        ""
      }
    }
    std_group <- function(label) {
      label <- tolower(label)
      if (grepl("resident", label, fixed = TRUE)) {
        "resident"
      } else if (grepl("(living", label, fixed = TRUE)) {
        "commuter_home"
      } else if (grepl("(not living", label, fixed = TRUE)) {
        "commuter_not_home"
      } else {
        ""
      }
    }
    part <- gsub("$", " ", part, fixed = TRUE)
    head_ind <- grep("Residents|Commuters|home\\)", part)
    if (!length(head_ind)) {
      stop("G5: failed to find header row(s)", call. = FALSE)
    }
    header <- part[head_ind]
    part <- part[-seq_len(max(head_ind))]
    part <- grep("[0-9]\\s*$", part, value = TRUE)
    if (!length(part)) {
      return(slots)
    }
    row_label_pos <- max(vapply(
      gregexpr("^(?:G5)?\\s*(?:[^ 0-9]+(?:\\s|$))+", part), attr, 0L, "match.length"
    ))
    row_label <- substr(part, 1L, row_label_pos)
    row_values <- substring(c(header, part), row_label_pos + 1L)
    row <- rep(" ", max(vapply(row_values, nchar, 0L)))
    any_content <- which(colMeans(do.call(rbind, lapply(strsplit(row_values, ""), function(r) {
      row[seq_along(r)] <- r
      row
    })) == " ") != 1L)
    n_contentful <- length(any_content)
    row_values <- row_values[-seq_along(header)]
    if (length(header) > 1L) {
      header <- grep(
        "resident|living", substring(header, row_label_pos),
        ignore.case = TRUE, value = TRUE
      )
      colnames <- NULL
      last_pos <- 0L
      for (i in seq_along(header)) {
        m <- gregexpr("([^ ]\\s?)+", header[[i]])[[1]]
        if (length(m)) {
          for (mi in seq_along(m)) {
            ms <- m[[mi]] - 1L
            mt <- std_group(substr(header[[i]], ms, ms + attr(m, "match.length")[[mi]] - 1L))
            if (mt != "") {
              colnames <- if (m[[i]] > last_pos) {
                c(colnames, mt)
              } else {
                c(mt, colnames)
              }
              last_pos <- ms
            }
          }
        }
      }
    } else {
      colnames <- vapply(strsplit(header, "\\s{2,}")[[1]], std_group, "")
      colnames <- colnames[colnames != ""]
    }
    ncols <- length(colnames)
    col_spans <- list()
    last_ind <- 1L
    for (i in seq_len(ncols)) {
      label <- colnames[[i]]
      hit <- FALSE
      if (last_ind < n_contentful) {
        for (ci in seq(last_ind, n_contentful - 1L)) {
          if (!any(any_content[[ci]] == any_content[[ci + 1L]] - c(1L, 2L))) {
            col_spans[[label]] <- c(any_content[[last_ind]], any_content[[ci]])
            last_ind <- ci + 1L
            hit <- TRUE
            break
          }
        }
      }
      if (!hit) {
        col_spans[[label]] <- c(any_content[[last_ind]], any_content[[length(any_content)]])
      }
    }
    ncols <- length(col_spans)
    row_spans <- list()
    last_id <- ""
    last_pos <- 0L
    for (i in rev(seq_along(row_label))) {
      row_id <- gsub("^\\s+|\\s+$", "", row_label[[i]])
      if (grepl("^[A-Z]", row_id)) {
        id <- std_part(paste(row_id, last_id))
        if (id == "") {
          stop("G5: unrecognized row ID (", row_id, ")", call. = FALSE)
        }
        if (last_pos == 0L) last_pos <- i
        row_spans[[id]] <- c(i, last_pos)
        last_id <- ""
        last_pos <- 0L
      } else {
        if (last_id == "") {
          last_id <- row_id
          last_pos <- i
        } else {
          last_id <- paste(row_id, last_id)
        }
      }
    }
    for (r in seq_along(row_spans)) {
      row_id <- names(row_spans)[[r]]
      span <- row_spans[[r]]
      values <- row_values[seq(span[[1]], span[[2]])]
      values <- values[values != ""]
      if (length(values) == 1) {
        for (col in seq_along(col_spans)) {
          label <- paste(names(col_spans)[[col]], row_id, sep = "_")
          if (!(label %in% names(slots))) next
          crange <- col_spans[[col]]
          value <- gsub("^\\s+|\\s+$", "", substr(values, crange[[1]], crange[[2]]))
          if (grepl("[A-Za-z]", value)) next
          if (grepl("[ -]", value)) {
            value <- strsplit(value, "\\s*-\\s*")[[1]]
            if (length(value) == 2L) {
              message("range collapsed to value: ", paste(value, collapse = "-"))
              value <- mean(as.integer(gsub("[^0-9.]", "", value)))
            } else {
              stop("G5: value spans columns: ", value, call. = FALSE)
            }
          }
          value <- gsub("[^0-9.]+", "", value)
          if (value != "" && value != ".") slots[[label]] <- as.integer(value)
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
    part <- gsub("$", " ", part, fixed = TRUE)
    last <- grep("athletic awards", part, ignore.case = TRUE)
    if (length(last) != 1) {
      stop("H1: failed to identify end of table", call. = FALSE)
    }
    part <- part[seq_len(last)]
    std_part <- function(label) {
      label <- tolower(label)
      if (grepl("state and other", label, fixed = TRUE)) {
        "self_other"
      } else if (grepl("federal work", label, fixed = TRUE)) {
        "self_federal"
      } else if (grepl("endowed scholarship", label, fixed = TRUE)) {
        "scholarship_institutional"
      } else if (grepl("student loans", label, fixed = TRUE)) {
        "self_loan"
      } else if (grepl("parent loans", label, fixed = TRUE)) {
        "parent_loan"
      } else if (grepl("external source", label, fixed = TRUE)) {
        "scholarship_external"
      } else if (grepl("total scholarship", label, fixed = TRUE)) {
        "scholarship_total"
      } else if (grepl("total self", label, fixed = TRUE)) {
        "self_total"
      } else if (grepl("state", label, fixed = TRUE)) {
        "scholarship_state"
      } else if (grepl("federal", label, fixed = TRUE)) {
        "scholarship_federal"
      } else if (grepl("waiver", label, fixed = TRUE)) {
        "waiver"
      } else if (grepl("athletic", label, fixed = TRUE)) {
        "athletic"
      } else {
        ""
      }
    }
    std_group <- function(label) {
      if (grepl("Non", label)) {
        "non_need"
      } else if (grepl("Need", label)) {
        "need"
      } else {
        ""
      }
    }
    head_ind <- grep("(?:Non.[Nn]eed|Need).[Bb]ased[^N]+(?:Non.[Nn]eed|Need)", part)
    if (!length(head_ind)) {
      head_ind <- grep("(?:Non.[Nn]eed|Need).[Bb]ased|(?:Non.[Nn]eed|Need)", part)
      if (length(head_ind) == 2L && head_ind[[1]] == head_ind[[2]] - 1L) {
        shortest <- which.min(nchar(part[head_ind]))
        part[[head_ind[[2L]]]] <- paste0(part[[head_ind[[shortest]]]], substring(
          part[[head_ind[[which.max(nchar(part[head_ind]))]]]],
          nchar(part[[head_ind[[shortest]]]])
        ))
        head_ind <- head_ind[[2L]]
      } else {
        stop("H1: failed to find header row(s)", call. = FALSE)
      }
    }
    head_ind <- c(head_ind, length(part) + 1L)
    for (hi in seq_len(length(head_ind) - 1L)) {
      cpart <- part[seq(head_ind[[hi]], head_ind[[hi + 1L]] - 1L)]
      head_end <- grep("need.)", cpart, fixed = TRUE)
      if (!length(head_end)) head_end <- 1L
      header <- cpart[seq(1L, head_end[[1]])]
      has_values <- grep("\\d\\s*$", cpart)
      if (!length(has_values)) next
      row_label_pos <- max(vapply(
        gregexpr("^(?:H1)?\\s*(?:[^ 0-9]+(?:\\s|$))+", cpart[has_values]), attr, 0L, "match.length"
      ))
      row_label <- substr(cpart, 1L, row_label_pos)
      row_values <- substring(c(header, cpart[has_values]), row_label_pos)
      row <- rep(" ", max(vapply(row_values, nchar, 0L)))
      any_content <- which(colMeans(do.call(rbind, lapply(strsplit(row_values, ""), function(r) {
        row[seq_along(r)] <- r
        row
      })) == " ") != 1L)
      n_contentful <- length(any_content)
      colnames <- strsplit(substring(header, row_label_pos), "\\s{2,}")[[1]]
      row_values <- substring(cpart, row_label_pos)
      col_spans <- list()
      last_ind <- 1L
      for (colname in colnames) {
        label <- std_group(colname)
        if (label != "") {
          hit <- FALSE
          if (last_ind < n_contentful) {
            for (ci in seq(last_ind, n_contentful - 1L)) {
              if (!any(any_content[[ci]] == any_content[[ci + 1L]] - c(1L, 2L))) {
                col_spans[[label]] <- c(any_content[[last_ind]], any_content[[ci]])
                last_ind <- ci + 1L
                hit <- TRUE
                break
              }
            }
          }
          if (!hit) {
            col_spans[[label]] <- c(any_content[[last_ind]], any_content[[length(any_content)]])
          }
        }
      }
      ncols <- length(col_spans)
      row_spans <- list()
      last_id <- ""
      last_pos <- 0L
      for (i in rev(seq_along(row_label))) {
        row_id <- gsub("^\\s+|\\s+$", "", sub(
          "(?:Federal )?(?:Work.)?Study captured above", "", row_label[[i]]
        ))
        if (
          !grepl("^(?:Report|Note|Merit|Nation|Kiwani|Exclud)", row_id) & grepl("^[A-Z]", row_id)
        ) {
          id <- std_part(paste(row_id, last_id))
          if (id == "") next
          if (last_pos == 0L) last_pos <- i
          row_spans[[id]] <- c(i, last_pos)
          last_id <- ""
          last_pos <- 0L
        } else {
          if (last_id == "") {
            last_id <- row_id
            last_pos <- i
          } else {
            last_id <- paste(row_id, last_id)
          }
        }
      }
      for (r in seq_along(row_spans)) {
        row_id <- names(row_spans)[[r]]
        span <- row_spans[[r]]
        rinds <- seq(span[[1]], span[[2]])
        values <- row_values[rinds[rinds %in% has_values]]
        values <- values[values != ""]
        if (length(values) == 1) {
          for (col in seq_along(col_spans)) {
            label <- paste(names(col_spans)[[col]], row_id, sep = "_")
            if (!(label %in% names(slots))) next
            crange <- col_spans[[col]]
            value <- gsub("^\\s+|\\s+$", "", substr(values, crange[[1]], crange[[2]]))
            if (grepl("\\s", value)) {
              stop("H1: values span columns: ", value, call. = FALSE)
            }
            value <- gsub("[^0-9.]+", "", value)
            if (value != "" && value != ".") slots[[label]] <- as.integer(value)
          }
        }
      }
    }
    slots
  }
)
