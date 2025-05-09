% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cds_get_links.R
\name{cds_get_links}
\alias{cds_get_links}
\title{Get CDS Links}
\usage{
cds_get_links(school, out_dir, url = NULL, overwrite = FALSE)
}
\arguments{
\item{school}{Name of the school.}

\item{out_dir}{Path to an output directory.}

\item{url}{Link to the school's CDS page, containing links to individual reports.}

\item{overwrite}{Logical; if \code{TRUE}, will always try to retrieve fresh links,
rather than using a previous pull.}
}
\value{
A list, which is also saved as \code{\{out_dir\}/record/\{school\}/links.json}:
\itemize{
  \item \strong{\code{school}}: Name of the school.
  \item \strong{\code{url}}: Link to the school's CDS page.
  \item \strong{\code{retrieved}}: Time the links were retrieved.
  \item \strong{\code{links}}: A vector of extracted links.
}
}
\description{
Scrape links to individual CDS reports from school websites.
}
\examples{
if (dir.exists("../../reports")) {
  report_links <- cds_get_links(
    "Yale University", "../../reports", "https://oir.yale.edu/common-data-set"
  )
  report_links
}
}
