character.limitation <- \(text, limitation){
  if (!is.numeric(limitation) || length(limitation) != 1L || is.na(limitation) || limitation < 0) {
    stop("`limitation` must be a single non-negative number.")
  }
  lim <- as.integer(limitation)
  if (length(text) == 0L) return(integer(0))

  text |>
    (\(x) if (!is.character(x)) as.character(x) else x)() |>
    (\(x) gsub("\\R", "", x, perl = TRUE))() |>
    nchar(allowNA = TRUE) |>
    (\(n) n <= lim)() |>
    as.integer()
}
