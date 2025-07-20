

setOldClass(Classes = 'bibentry')

#' @title \linkS4class{md_lines}
#' 
#' @description
#' ..
#' 
#' @slot .Data \link[base]{character} scalar or \link[base]{vector}
#' 
#' @slot bibentry a \link[utils]{bibentry} object
#' 
#' 
#' @keywords internal
#' @importFrom utils bibentry
#' @export
setClass(Class = 'md_lines', contains = 'character', slots = c(
  bibentry = 'bibentry'
), prototype = prototype(
  bibentry = bibentry()
))