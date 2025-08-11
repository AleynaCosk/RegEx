#' Check if text stays within a character limit (Unicode-safe)
#'
#' Vectorised over `text`. Returns integer flags and **ignores line breaks**
#' when counting characters (so " \code{"a\\nb\\nc"}" with limit 3 passes).
#'
#' @param text Character vector (can include `NA`).
#' @param limitation Single non-negative number (max character count).
#' @return Integer vector: `1L` if within limit, `0L` otherwise, and
#'   `NA_integer_` for `NA` inputs.
#' @examples
#' character.limitation("hello", 5)   # 1L (at boundary)
#' character.limitation("hello", 4)   # 0L
#' character.limitation(c("abc", NA, "abcdef"), 5)
#' @export


character.limitation <- \(text, limitation){
  if (!is.numeric(limitation) || length(limitation) != 1L || is.na(limitation) || limitation < 0) {
    stop("`limitation` must be a single non-negative number.")
  }
  lim <- as.integer(limitation)

  if (length(text) == 0L) {
    txt <- NA_character_
  } else {
    txt <- as.character(text[[1L]])
  }

  stripped <- gsub("\\R", "", txt, perl = TRUE)
  len <- nchar(stripped, allowNA = TRUE)
  within <- if (!is.na(len)) (len <= lim) else NA

  payload <- list(
    limit = lim,
    results = list(list(
      text   = txt,
      length = if (!is.na(len)) as.integer(len) else NA_integer_,
      within = isTRUE(within)   # ensures TRUE/FALSE (not 1/0)
    ))
  )

  jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null", null = "null")
}
