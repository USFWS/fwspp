# Helper to run function up to 3 times with increasing backoff times
# if request produces any error (modified from `VERB_n` in googlesheets package);
# errors (after n attempts) are kept and passed along.  Currently retrying on all
# errors timeouts, brief server errors, and non-negotiable errors can be better
# differentiated. Note that this function is also used to capture error stacks for
# non-HTTP related functions (e.g., cleaning and scrubbing)
try_verb_n <- function(verb, n = 3) {
  function(...) {
    for (i in seq_len(n)) {
      out <- try_capture_stack(verb(...))
      if (!is_error(out) || i == n) break
      wait <- stats::runif(1, min(5 ^ i, 120), min(5 ^ (i + 1), 180))
      mess <- paste("HTTP timeout or error on attempt %d.",
                    "Retrying in %0.0f s.")
      message(sprintf(mess, i, wait))
      Sys.sleep(wait)
    }
    out
  }
}

is_error <- function(obj) inherits(obj, "error")

fws_url <- function() "https://ecos.fws.gov/ServCat/DownloadFile/126665"

# From evaluate package
# https://github.com/r-lib/evaluate/blob/master/R/traceback.r
try_capture_stack <- function(quoted_code, env = new.env(parent = parent.frame())) {
  capture_calls <- function(e) {
    e$calls <- utils::head(sys.calls()[-seq_len(frame + 7)], -2)
    signalCondition(e)
  }
  frame <- sys.nframe()

  tryCatch(
    withCallingHandlers(eval(quoted_code, env), error = capture_calls),
    error = identity
  )
}

gbif_count <- function(prop, ...) {
  n <- rgbif::occ_search(limit = 0, ...,
                         geometry = get_wkt(prop),
                         return = "meta")
  pull(n, count)
}

est_timeout <- function(n_recs) {
  mins <- exp(-6 + 0.85 * log(n_recs))
  ceiling(mins * 60)
}

est_nrecs <- function(timeout) {
  mins <- timeout / 60
  recs <- round(exp((log(mins) + 6) / 0.85))
  1000 * (recs %/% 1000 + as.logical(recs %% 1000))
}
