# Evaluate `expr` with deprecation warnings forced on (they are normally shown
# once per session) and fail if any is raised. Other warnings are passed
# through unless they match the `ignore` regular expression.
expect_no_deprecation <- function(expr, ignore = NULL) {
  withr::local_options(lifecycle_verbosity = "warning", rlib_warning_verbosity = "verbose")
  deprecations <- character()
  result <- withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (inherits(w, "lifecycle_warning_deprecated") || grepl("deprecated", msg, fixed = TRUE)) {
        deprecations <<- c(deprecations, msg)
        invokeRestart("muffleWarning")
      } else if (!is.null(ignore) && grepl(ignore, msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  testthat::expect(
    length(deprecations) == 0,
    paste0("Deprecation warning(s) raised:\n", paste(unique(deprecations), collapse = "\n"))
  )
  invisible(result)
}
