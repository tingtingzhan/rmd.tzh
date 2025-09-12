


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

