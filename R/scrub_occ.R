scrub_occ <- function(occ_recs, scrub) {

  stopifnot(scrub %in% c("strict", "moderate"))

  # Set biodiversity repo preference for retaining records
  repo_pref <- c("AntWeb", "iDigBio", "VertNet", "BISON", "EcoEngine", "GBIF")

  ## Strict scrubbing
  if (identical(scrub, "strict")) {

    all_media <- occ_recs %>%
      filter(!is.na(.data$media_url)) %>%
      group_by(.data$sci_name) %>%
      arrange(.data$sci_name, -.data$year, -.data$month, -.data$day) %>%
      slice(match(repo_pref, .data$bio_repo)) %>%
      filter(row_number() == 1)

    spp_evid <- occ_recs %>%
      filter(!(.data$sci_name %in% all_media$sci_name), !is.na(.data$evidence)) %>%
      group_by(.data$sci_name) %>%
      arrange(.data$sci_name, -.data$year, -.data$month, -.data$day) %>%
      slice(match(repo_pref, .data$bio_repo)) %>%
      filter(row_number() == 1)

    occ_recs <- bind_rows(all_media, spp_evid)

    ## Moderate scrubbing
  } else  {

    # Prioritize order biodiversity repo
    col_order <- names(occ_recs)
    occ_recs <- inner_join(data.frame(bio_repo = repo_pref,
                                      stringsAsFactors = FALSE),
                           occ_recs, by = "bio_repo")

    # Catalog number duplicates
    cat_no_dups <- occ_recs %>%
      select(.data$sci_name, .data$cat_no) %>%
      df_dups_ignore_NA()

    # Location duplicates (to 4 decimals)
    loc_dups <- occ_recs %>%
      mutate(lat = round(.data$lat, 4),
             lon = round(.data$lon, 4)) %>%
      select(.data$sci_name, .data$year, .data$month, .data$day,
             .data$lat, .data$lon) %>%
      df_dups_ignore_NA()

    occ_recs <- filter(occ_recs, !cat_no_dups, !loc_dups) %>%
      select_at(col_order)

  }

  ungroup(occ_recs) %>% as_tibble()

}

#' Remove duplicates from data.frame using multiple types of NA as incomparables
#' @noRd
df_dups_ignore_NA <- function(df, incomparables = c(NA, NA_character_))
{
  ## Pared from https://gist.github.com/ReportMort/c3ce765fa21a03460cfd
  n <- ncol(df)
  nmx <- names(df)
  lincomparables <- length(incomparables)
  tmp <- c(incomparables, as.list(rep_len(FALSE, n - lincomparables)))
  names(tmp) <- nmx
  incomparables <- tmp

  res <- duplicated(do.call("paste", c(df, sep="\r")))

  run_incomp_check <- sapply(incomparables, FUN = function(x) {!identical(x, FALSE)})

  if (sum(run_incomp_check) > 0L) {
    incomp_check <- mapply(FUN = function(column, incomparables) {match(column, incomparables)},
                           df[run_incomp_check], incomparables[run_incomp_check])
    # any rows with an incomparable match means, TRUE, it can override the duplicated result
    overwrite <- apply(data.frame(incomp_check), 1, function(x) {any(!is.na(x))})
    res[overwrite] <- FALSE
 }

  res
}
