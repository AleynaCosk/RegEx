#' Check if text stays within a character limit (Unicode-safe)
#'
#' Vectorised over `text`. Counts spaces, but **ignores line breaks** when
#' measuring length (e.g., `"a\nb\nc"` is counted as 3). Useful for validating
#' UI copy length while allowing multi-line inputs.
#'
#' @param text Character vector (may include `NA`).
#' @param limitation Single non-negative number (max character count); coerced to integer.
#' @param output One of `"json"` (default) or `"logical"`.
#'
#' @return
#' - If `output = "logical"`: a logical vector `TRUE`/`FALSE`/`NA` indicating
#'   whether each element is within the limit.
#' - If `output = "json"`: a single JSON string with the shape:
#'   ```
#'   {
#'     "limit": <number>,
#'     "results": [
#'       { "text": <string|null>, "length": <number|null>, "within": <boolean|null> },
#'       ...
#'     ]
#'   }
#'   ```
#'   where R `NA` values are encoded as JSON `null`.
#'
#' @details
#' Character counting removes Unicode line separators via `\\R`, but leaves all
#' other characters (including spaces and tabs). This mirrors typical UI limits
#' where newlines don’t consume quota but spaces do.
#'
#' @examples
#' # Logical output (vectorised)
#' character.limitation(c("hi", "hello world", NA_character_, ""), 5, output = "logical")
#' #> TRUE FALSE NA TRUE
#'
#' # Newlines are ignored; spaces are counted
#' character.limitation("hello\nworld", 10, output = "logical")  # "helloworld" -> 10
#' #> TRUE
#' character.limitation("a b", 3, output = "logical")            # counts space -> 3
#' #> TRUE
#'
#' # JSON output (default): single JSON string for all inputs
#' js <- character.limitation(c("abc", NA, "abcdef"), 5)
#' jsonlite::fromJSON(js, simplifyVector = FALSE)
#'
#' @export



character.limitation <- \(text, limitation, output = c("json", "logical")) {
  output <- match.arg(output)
  if (!is.numeric(limitation) || length(limitation) != 1L || is.na(limitation) || limitation < 0)
    stop("`limitation` must be a single non-negative number.")
  lim <- as.integer(limitation)

  txt   <- as.character(text)
  lens  <- txt |> (\(x) gsub("\\R", "", x, perl = TRUE))() |> nchar(allowNA = TRUE)
  within <- lens <= lim 

  if (output == "logical") return(within)

  results <- Map(\(t, len, win) list(
    text   = if (is.na(t))   NULL else t,
    length = if (is.na(len)) NULL else as.integer(len),
    within = if (is.na(win)) NULL else isTRUE(win)
  ), txt, lens, within)

  jsonlite::toJSON(list(limit = lim, results = results),
                   auto_unbox = TRUE, na = "null", null = "null")
}
