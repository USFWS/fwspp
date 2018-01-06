#' @param export_dir character scalar of path to store an output rds file for
#'  each \code{refuge}. The default is to create a new \code{output_rds} directory
#'  in the current working directory.

# if (!dir.exists(export_dir)) dir.create(export_dir)
#
# # FORMER CSV EXPORT CODE
# # MODIFY TO RDS
# # CHECK IF FILE EXISTS, IF SO SKIP? OR OVERWRITE?
#   invisible(
#     lapply(seq_along(out), function(i) {
#       tmp_fn <- paste0(gsub(" |[.]", "", names(out)[i]), ".csv")
#       if (inherits(out[[i]], "try-error")) {
#         cat("fw_spp failed for", names(out)[i], "\n")
#       } else if (is.null(out[[i]])) {
#         cat("No valid observations found for", names(out)[i], "\n")
#       } else {
#         utils::write.csv(out[[i]], row.names = FALSE,
#                          file = file.path(export_dir, tmp_fn))
#       }
#     })
#   )
