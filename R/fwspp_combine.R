#' Consolidate two or more \code{fwspp} objects
#'
#' Use this function to combine two or more \code{fwspp} objects into a single
#'   \code{fwspp} object. Attributes are preserved. Only works if \code{fwspp}
#'   objects were generated using the same \code{boundary}, \code{scrubbing},
#'   \code{buffer}, and \code{taxonomy} settings (see \code{\link{fws_occ}}.
#'   The earliest query date/time in the combined \code{fwspp} objects is
#'   assigned to the combined object.
#'
#' @param ... two or more \code{fwspp} objects returned by \code{\link{fws_occ}}
#'
#' @return a \code{\link{fwspp}} object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ml <- find_fws("longleaf")
#' ml <- fws_occ(fws = ml)
#' mb <- find_fws("mountain bog")
#' mb <- fws_occ(fws = mb)
#' mlb <- fwspp_combine(ml, mb)
#' }

fwspp_combine <- function(...) {

  fwspp_list <- list(...)

  # Validating that objects can be combined
  if (length(fwspp_list) < 2) stop("Need at least two `fwspp` objects to use this function.")
  if (!all(sapply(fwspp_list, inherits, "fwspp")))
    stop("All inputs must be `fwspp` objects.")
  exp_attr <- c("names", "class", "boundary", "scrubbing", "buffer_km", "query_dt")
  if (!all(sapply(fwspp_list, function(x) identical(names(attributes(x)), exp_attr))))
    stop("One or more `fwspp` objects is missing expected attributes.")
  if (length(unique(sapply(fwspp_list, has_taxonomy))) > 1)
    stop("One of these things is not like the others...\n",
         "    Input objects must all lack (or all possess) taxonomic information.")
  core_attr <- exp_attr[2:5]
  core_attr <- lapply(fwspp_list, function(x) attributes(x)[core_attr])
  if (length(unique(core_attr)) != 1)
    stop("Cannot combine `fwspp` objects with different `boundary`, `scrubbing`, ",
         "or `buffer` settings.")

  # Combining
  orgnames <- unlist(lapply(fwspp_list, function(x) attributes(x)[["names"]]))
  query_dts <- lapply(fwspp_list, function(x) attributes(x)[["query_dt"]])
  earliest_query <- query_dts[[which.min(query_dts)]]
  out_fwspp <- do.call("c", fwspp_list)
  attributes(out_fwspp) <- c(list(names = orgnames),
                           core_attr[[1]],
                           list(query_dt = earliest_query))
  out_fwspp
}
