join_itis <- function(occ_recs) {

  ## To *join* ITIS, you must first *get* ITIS
  sci_vec <- unique(occ_recs$sci_name)

  try_itis <- try_verb_n(itistools::get_itis)
  itis <- try_itis(sci_vec)

  if (is_error(itis)) return(itis)

  itis <- itis %>%
    mutate(note = ifelse(is.na(.data$valid_sci_name), "No ITIS match",
                         NA_character_))

  # Now join ITIS info to occurrence records
  occ_recs <- left_join(occ_recs, itis, by = "sci_name") %>%
    mutate(sci_name = ifelse(is.na(.data$valid_sci_name),
                             .data$sci_name, .data$valid_sci_name)) %>%
    select(.data$class, taxon_rank = .data$itis_taxon_rank, .data$sci_name,
           com_name = .data$itis_com_name, everything(), -.data$valid_sci_name)
  as_tibble(occ_recs)

}
