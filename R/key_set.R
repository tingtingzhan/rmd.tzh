


#' @title key set
#' 
#' @param x ..
#' 
#' @param value ..
#' 
#' @keywords internal
#' @export
`key<-` <- function(x, value) UseMethod(generic = 'key<-') 


#' @export
`key<-.bibentry` <- function(x, value) {
  
  # also used as `key<-.citation` :)
  
  if (length(x) != 1L) stop('bibentry must have len-1')
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) stop('illegal `value`')
  
  x0 <- unclass(x)
  attr(x0[[1L]], which = 'key') <- value
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