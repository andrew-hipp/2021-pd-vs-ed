#' Tidy up a name
#' @param x `character` object
#' @param fixes what to get rid of
#' @param makeLow `boolean` object; indicates whether to get rid of case
#' @param grepFix `boolean`; indicates whether `fixed = TRUE` in `gsub`
tidyName <- function(x, fixes = c('[_-. ]'), makeLow = TRUE, grepFix = FALSE) {
  if(makeLow) x = tolower(x)
  for(i in fixes) x <- (gsub(i, "", x, fixed = grepFix))
  x
  }
