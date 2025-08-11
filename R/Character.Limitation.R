character.limitation <- \(text, limitation){
  if (length(text) == 0) return(integer(0))
  vapply(text, function(t) if (is.na(t)) NA_integer_ else 0L, integer(1))
}
