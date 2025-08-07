


#' @title Set Key of \link[utils]{bibentry} and/or \link[utils]{citation}
#' 
#' @param x a \link[utils]{bibentry} and/or \link[utils]{citation}
#' 
#' @param value \link[base]{character} scalar
#' 
#' @returns 
#' This syntactic sugar returns a \link[utils]{bibentry} and/or \link[utils]{citation} object.
#' 
#' @keywords internal
#' @export
`key<-` <- function(x, value) UseMethod(generic = 'key<-') 


#' @export
`key<-.bibentry` <- function(x, value) {
  
  # also used as `key<-.citation` :)
  
  nx <- length(x)
  
  if (!is.character(value) || anyNA(value) || any(!nzchar(value))) stop('illegal `value`')
  value <- unique.default(value)
  nv <- length(value)
  
  if (nx > 1L && nv == 1L) {
    value <- paste0(value, seq_len(nx))
    nv <- nx
  }
  
  if (nx != nv) stop(sprintf(fmt = '`value` must be len-1 or len-%d', nx))  
  
  x0 <- mapply(
    FUN = `attr<-`, # syntactic sugar here does not modify the input..
    x = unclass(x), value = value,
    MoreArgs = list(which = 'key'),
    SIMPLIFY = FALSE
  )
  class(x0) <- class(x)
  
  return(x0)
  
}

#' @title Use Package as Key in \link[utils]{citation}
#' 
#' @param x a \link[utils]{citation} object
#' 
#' @returns
#' Function [package2key()] returns a \link[utils]{citation} object.
#' 
#' @examples
#' 'rmarkdown' |> citation() |> toBibtex()
#' 'rmarkdown' |> citation() |> package2key() |> toBibtex()
#' @keywords internal
#' @export
package2key <- function(x) {
  # read carefully
  # ?utils::citation
  # ?utils:::.citation
  if (!inherits(x, 'citation')) stop('input must be package citation')
  # length(x) must be 1, by design of ?utils::citation
  key(x) <- attr(x, which = 'package', exact = TRUE)
  return(x)
}