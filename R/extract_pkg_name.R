

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
#' @examples
#' extract_pkg_name('<u>**`R`**</u> package <u>**`patchwork`**</u>')
#' extract_pkg_name('<u>**`R`**</u> package <u>**`tools`**</u>') # 'tools' is a base-package
#' extract_pkg_name(c('[R] package [patchwork]', '[ggplot2]'), pattern = '(?<=\\[)(.*?)(?=\\])')
#' # exception handling
#' extract_pkg_name(character())
#' extract_pkg_name('')
#' extract_pkg_name(NA_character_)
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
