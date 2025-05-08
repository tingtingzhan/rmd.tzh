

#' @title Extract Package Name(s)
#' 
#' @description
#' Extract package name(s) from a \link[base]{character} \link[base]{vector}
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @param pattern \link[base]{regex}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @note
#' Note of function names `pkgload::pkg_name` and `pkgbuild:::pkg_name`.
#' 
#' @examples
#' '<u>**`R`**</u> package <u>**`patchwork`**</u>' |> extract_pkg_name()
#' '<u>**`R`**</u> package <u>**`stats`**</u>' |> extract_pkg_name() # 'stats' is a base-package
#' c('[R] package [patchwork]', '[ggplot2]') |> extract_pkg_name(pattern = '(?<=\\[)(.*?)(?=\\])')
#' # exception handling
#' character() |> extract_pkg_name()
#' '' |> extract_pkg_name()
#' NA_character_ |> extract_pkg_name()
#' @keywords internal
#' @importFrom stringi stri_extract_all_regex
#' @export
extract_pkg_name <- function(
    x, 
    pattern = '(?<=\\<u\\>\\*\\*`)(.*?)(?=`\\*\\*\\<\\/u\\>)',
    ...
) {
  
  x <- x[!is.na(x)]
  
  # workhorse
  ret <- x |>
    stri_extract_all_regex(pattern = pattern) |>
    unlist(use.names = FALSE) |>
    unique.default()
  
  if (any(!nzchar(ret))) stop('should not happen')
  # if (anyNA(ret)) may happen! (`x = ''`)
  
  # installed.packages(priority = 'base') |> rownames() # RStudio do not have a delete button for base-packages
  return(setdiff(
    x = c('base', ret[!is.na(ret)]), 
    y = c('R', 'compiler', 'datasets', 'graphics', 'grDevices', 'grid', 'methods', 'parallel', 'splines', 'stats', 'stats4', 'tcltk', 'tools', 'utils')
  ))
  
}



#' @title Text for Package to Create an R Object
#' 
#' @param x an R object
#' 
#' @returns 
#' Function [pkg_text()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @importFrom stats getCall
#' @importFrom utils getAnywhere
#' @export
pkg_text <- function(x) {
  
  if (isS4(x)) {
    return(x |> 
             class() |> 
             attr(which = 'package', exact = TRUE) |> 
             sprintf(fmt = '<u>**`R`**</u> package <u>**`%s`**</u>'))
  } 
  
  f <- getCall(x)[[1L]]
  pkg <- tryCatch(expr = {
    f |>
      eval() |> # function call could be un-exported, e.g., nlme:::lme.formula, and err
      environment() |>
      getNamespaceName()
  }, error = \(e) {
    aw <- f |>
      as.character() |>
      getAnywhere()
    if (length(aw$where) > 1L) stop('really shouldnt happen...')
    (aw$where) |>
      gsub(pattern = '^namespace\\:', replacement = '')
  })
  
  # utils::installed.packages(priority = 'base') |> rownames() 
  # also, RStudio do not have a delete button for base-packages
  if (pkg %in% c('base', 'compiler', 'datasets', 'graphics', 'grDevices', 'grid', 'methods', 'parallel', 'splines', 'stats', 'stats4', 'tcltk', 'tools', 'utils')) {
    return('<u>**`R`**</u>')
  } 
  
  pkg |> 
    sprintf(fmt = '<u>**`R`**</u> package <u>**`%s`**</u>')
  
}




