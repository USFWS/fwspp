#' Export species occurrence records in an \code{fwspp} object to separate
#'  files for review
#'
#' Use this function to export species occurrence records into spreadsheets
#'  for review prior to parsing for inclusing into the FWSpecies database.
#'
#' This function **requires** that the \code{fwspp} object have taxonomy
#'  added (either via the \code{taxonomy = TRUE} argument during the call
#'  to \code{\link{fws_occ}} or subsequently via \code{\link{add_taxonomy}}).
#'  Reviewing records without additional taxonomic information (such as common
#'  name and general taxonomic groupings) is likely to be frustrating.
#'
#' @param fwspp a \code{fwspp} object returned by \code{\link{fws_occ}},
#'  **with taxonomy information added** (see Details). An output file will be created in \code{dir}
#'  for each property in \code{fwspp}.
#' @param dir a non-empty character scalar of path to store an output file for
#'  each USFWS property in \code{fwspp}. The default is to use or create a
#'  \code{fwspp_review} directory in the current working directory.
#' @param overwrite logical (default \code{FALSE}); overwrite existing files
#'  with the same name?
#' @param verbose logical (default \code{TRUE}); provide detailed messaging during
#'  export process?
#'
#' @return \code{NULL}; Exports individual Excel file(s) to \code{dir} for
#'  the review of species occurrence observations
#'
#' @import openxlsx
#' @export
#' @examples
#' \dontrun{
#' lowcountry <- find_fws(c("romain", "santee", "ace basin", "waccamaw"))
#' lc <- fws_occ(fws = lowcountry)
#' review_fwspp(lc)
#' }

fwspp_review <- function(fwspp, dir = "./fwspp_review",
                         overwrite = FALSE, verbose = TRUE) {
  if (!inherits(fwspp, "fwspp"))
    stop("Input must be an `fwspp` object. See `?fws_occ`.")

  if (!has_taxonomy(fwspp))
    stop("Additional taxonomic information will be most helpful ",
         "to the review process. \nSee `?add_taxonomy`.")

  if (is.null(dir)) stop("You must specify and output directory.\n",
                         "If it does not exist it will be created.")
  if (!dir.exists(dir)) {
    dir.create(dir)
    message("Output directory created at ", normalizePath(dir))
  }

  orgs <- names(fwspp)

  if (verbose)
    invisible(
      lapply(orgs, construct_wb, fwspp, overwrite, verbose, dir))
  else
    invisible(
      pbapply::pblapply(orgs, construct_wb, fwspp, overwrite, verbose, dir))
}
