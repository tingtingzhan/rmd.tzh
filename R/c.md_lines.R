
#' @title [c.md_lines()]
#' 
#' @param ... one or more \linkS4class{md_lines} objects
#' 
#' @export c.md_lines
#' @export
c.md_lines <- function(...) {
  
  x <- list(...)
  
  z <- x |>
    lapply(FUN = \(i) {
      c(unclass(i), '\n\n')
    }) |>
    do.call(what = c, args = _) # ?base::c, Primitive
  
  bib_ <- x |> 
    lapply(FUN = slot, name = 'bibentry') 
  id <- (lengths(bib_, use.names = FALSE) > 0L)
  if (any(id)) {
    bib <- bib_[id] |> # ?utils:::c.bibentry cannot take non-bibentry input
      do.call(what = c, args = _) |> 
      # ?base::c 
      # ?utils:::c.bibentry
      unique() 
    # ?base::unique.default
    # ?utils:::unique.bibentry
    dup_bibKey <- bib |>
      unclass() |>
      vapply(FUN = attr, which = 'key', exact = TRUE, FUN.VALUE = '') |>
      anyDuplicated.default()
    if (dup_bibKey) stop('same key(s) from different bibliography items')
  } else bib <- bibentry()
  
  new(Class = 'md_lines', z, bibentry = bib)
  
}


# old function
collect_attr_ <- function(x, which = c('bibentry', 'package')) {
  # `x` is a 'list' with 'character' elements
  
  which <- match.arg(which)
  
  if (!is.recursive(x)) x <- list(x)
  
  z <- x |> 
    lapply(FUN = attr, which = 'bibentry', exact = TRUE) 
  
  id <- (lengths(z, use.names = FALSE) > 0L)
  if (!any(id)) return(invisible())
  
  ret <- z[id] |> # ?utils:::c.bibentry cannot take non-bibentry input
    do.call(what = c, args = _) |> 
    # ?base::c 
    # ?utils:::c.bibentry
    unique() 
  # ?base::unique.default
  # ?utils:::unique.bibentry
  
  # some specific checks
  switch(which, bibentry = {
    dup_key <- ret |>
      unclass() |>
      vapply(FUN = attr, which = 'key', exact = TRUE, FUN.VALUE = '') |>
      anyDuplicated.default()
    if (dup_key) stop('same key(s) from different bibliography items')
  })
  
  return(ret)
  
}
