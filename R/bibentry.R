
# S3 method dispatches for \link[utils]{bibentry}

# '\u201c|\u201d' # quotation marks created by ?base::dQuote
# '\u2018|\u2019' # quotation marks created by ?base::sQuote



#' @title Subset \link[utils]{bibentry}
#' 
#' @param x a \link[utils]{bibentry} object
#' 
#' @param subset ..
#' 
#' @param ... ..
#' 
#' @details
#' Function [subset.bibentry()] ..
#' @examples
#' (bib_survival = 'survival' |> citation())
#' bib_survival |> subset(subset = (bibtype != 'Manual'))
#' bib_survival |> subset(subset = (bibtype == 'Manual'))
#' 
#' (bib_ggplot2 = 'ggplot2' |> citation())
#' bib_ggplot2 |> subset(subset = (bibtype != 'Manual'))
#' bib_ggplot2 |> subset(subset = (bibtype == 'Manual'))
#' bib_ggplot2 |> subset(subset = (bibtype == 'Book'))
#' @keywords internal
#' @export subset.bibentry
#' @export
subset.bibentry <- function(x, subset, ...) {
  
  e <- substitute(subset)
  if (!all(all.vars(e) %in% c('bibtype'))) stop('unsupported criterion')
  
  x0 <- x |>
    unclass() # avoid dispatching to ?utils:::`[[.bibentry`
  
  bibtype <- x0 |>
    vapply(FUN = attr, which = 'bibtype', exact = TRUE, FUN.VALUE = '')
  
  return(x[eval(e), drop = FALSE]) # ?utils:::`[.bibentry`
  # len-0 compatible
  
}









#' @title Sort \link[utils]{bibentry} By
#' 
#' @param x a \link[utils]{bibentry} object
#' 
#' @param y \link[base]{character} scalar
#' 
#' @param ... additional parameters of function \link[base]{order}
#' 
#' @details
#' 
#' Function [sort_by.bibentry()] sorts multiple citations of one package by some criteria (default being `'year'`).
#' 
#' @examples
#' 'rmarkdown' |> citation()
#' 'rmarkdown' |> citation() |> sort_by(y = 'year', decreasing = TRUE)
#' @keywords internal
#' @export sort_by.bibentry
#' @export
sort_by.bibentry <- function(x, y = 'year', ...) {
  
  nx <- length(x)
  if (nx == 1L) return(x)
  
  if (!is.character(y) || length(y) != 1L || is.na(y) || !nzchar(y)) stop('`y` must be len-1 character')
  
  o <- x |>
    unclass() |> # to avoid using ?utils:::`[[.bibentry`
    vapply(FUN = \(i) i[[y]], FUN.VALUE = '') |> # all fields are \link[base]{character}
    order(...)
  
  # um, if (y == 'bibtype'), we need to grab the base::attr ..
  
  return(x[o, drop = FALSE]) # utils:::`[.bibentry`
  
}




