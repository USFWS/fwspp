# Helper to run function up to 3 times with increasing backoff times
# if request produces any error (modified from `VERB_n` in googlesheets package);
# errors (after n attempts) are kept and passed along.  Currently retrying on all
# errors timeouts, brief server errors, and non-negotiable errors can be better
# differentiated. Note that this function is also used to capture error stacks for
# non-HTTP related functions (e.g., cleaning and scrubbing)
try_verb_n <- function(verb, n = 3) {
  function(...) {
    for (i in seq_len(n)) {
      out <- try_capture_stack(
        verb(...),
        env = new.env(parent = parent.frame()))
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

# From evaluate package
# https://github.com/r-lib/evaluate/blob/master/R/traceback.r
try_capture_stack <- function(quoted_code, env) {
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
