#' Export species occurrence records in an \code{fwspp} object to separate
#'  files for review
#'
#' Use this function to export species occurrence records into spreadsheets
#'  for review prior to parsing for inclusing into the FWSpecies database
#'  (\url{https://sites.google.com/a/fws.gov/fwspecies/home-1})
#'
#' This function **requires** that the \code{fwspp} object have taxonomy
#'  added (either via the \code{taxonomy = TRUE} argument during the call
#'  to \code{\link{fws_occ}} or subsequently via \code{\link{add_taxonomy}}).
#'  Reviewing records without additional taxonomic information (such as common
#'  name and general taxonomic groupings) is likely to be frustrating.
#'
#' @section Regarding review of output spreadsheets:
#' During review, users may modify the values for any observation from the
#'  \code{taxon_code} to \code{evidence} columns. Two columns in particular,
#'  however, will demand the most attention - \code{accept_record} and
#'  \code{taxon_code}.
#'
#' Records without adequate documentation for inclusion in the FWSpecies
#'  database should have \code{accept_record = No}. Some observations may
#'  be set to \code{accept_record = No} by default because of ambiguous
#'  or misspelled scientific names.
#'
#' Should a valid taxonomic match be found (e.g., by retrieving the
#'  appropriate \code{taxon_code} for the correct scientific name with
#'  \code{\link{nps_taxonomy}}), update the \code{taxon_code} in the
#'  spreadsheet and set \code{accept_record = ModifiedTaxonCode}. Setting
#'  \code{accept_record = ModifiedTaxonCode}
#'  means any taxonomic changes will be incorporated, and the record accepted,
#'  in the file subsequently created for submission to the FWSpecies database.
#'
#' Changes to species taxonomy (\code{taxon_code} and \code{sci_name}) will
#'  be incorporated ONLY if \code{taxon_code} is corrected and
#'  \code{accept_record = ModifiedTaxonCode}.
#'
#' Changes or additions to common names (\code{com_name}) will be accepted
#'  ONLY if \code{accept_record = ModifiedTaxonCode}, but \code{taxon_code}
#'  need not be changed for changes only to common name.
#'
#' Changes to other columns (\code{occurrence} to \code{evidence}) will be
#'  incorporated as-is so long as \code{accept_record = Yes} or
#'  \code{accept_record = ModifiedTaxonCode}.
#'
#' Records with \code{accept_record = No} are excluded.

#'
#' @param fwspp a \code{fwspp} object returned by \code{\link{fws_occ}},
#'  **with taxonomy information added** (see Details). An output file will be created in \code{out_dir}
#'  for each property in \code{fwspp}.
#' @param out_dir a non-empty character scalar of path to store an output file for
#'  each USFWS property in \code{fwspp}. The default is to use or create a
#'  \code{fwspp_review} directory in the current working directory.
#' @param overwrite logical (default \code{FALSE}); overwrite existing files
#'  with the same name?
#' @param verbose logical (default \code{TRUE}); provide detailed messaging during
#'  export process?
#'
#' @return \code{NULL}; Exports individual Excel file(s) to \code{out_dir} for
#'  the review of species occurrence observations
#'
#' @import openxlsx
#' @export
#' @examples
#' \dontrun{
#' lowcountry <- find_fws(c("romain", "santee", "ace basin", "waccamaw"))
#' lc <- fws_occ(fws = lowcountry)
#' fwspp_review(lc)
#' }

fwspp_review <- function(fwspp, out_dir = "./fwspp_review",
                         overwrite = FALSE, verbose = TRUE) {
  if (!inherits(fwspp, "fwspp"))
    stop("Input must be an `fwspp` object. See `?fws_occ`.")

  if (!has_taxonomy(fwspp))
    stop("Additional taxonomic information will be most helpful ",
         "to the review process. \nSee `?add_taxonomy`.")

  if (is.null(out_dir)) stop("You must specify and output directory.\n",
                         "If it does not exist it will be created.")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
    message("Output directory created at ", normalizePath(out_dir))
  }

  orgs <- names(fwspp)

  if (verbose)
    invisible(
      lapply(orgs, xlsx_review, fwspp, overwrite, verbose, out_dir))
  else {
    message("Exporting files for review.")
    invisible(
      pbapply::pblapply(orgs, xlsx_review, fwspp, overwrite, verbose, out_dir))
  }
}
