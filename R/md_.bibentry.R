

#' @rdname md_
#' 
#' @examples 
#' list(
#'  stringi = 'String manipulation by <u>**`R`**</u> package <u>**`stringi`**</u>.',
#'  texreg = 'R regression output to LaTeX or HTML by <u>**`R`**</u> package <u>**`texreg`**</u>.',
#'  stats = 'Linear regression by <u>**`R`**</u> package <u>**`stats`**</u>.'
#' ) |> render_(file = 'bibentry')
#' 
#' @export md_.bibentry
#' @export
md_.bibentry <- function(x, ...) {
  
  pkg <- attr(x, which = 'package', exact = TRUE)
  
  sprintf(
    fmt = '<u>**`%s`**</u> %s\n', 
    if (pkg == 'base') 'R' else pkg, 
    x |> bibentry2text()
  ) |> new(Class = 'md_lines')
  
}



#' @title Slight Improvement over `utils:::format.bibentry`
#' 
#' @param x a \link[utils]{bibentry} object
#' 
#' @details
#' Function [bibentry2text()] beautifies the output from 
#' function `utils:::format.bibentry(., style = 'text')`
#' in the following ways.
#' \itemize{
#' \item{Line break `'\n'` is replaced by a white space;}
#' \item{Fancy quotes \eqn{``}, \eqn{''}, \eqn{`} and \eqn{'} are removed;}
#' \item{doi entries are shown as URLs with labels in markdown grammar.}
#' }
#' 
#' @examples
#' 'texreg' |> citation() |> format(style = 'text')
#' 'texreg' |> citation() |> bibentry2text()
#' @keywords internal
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed stri_replace_all_regex
#' @export
bibentry2text <- function(x) {
  
  format_doi <- function(x, regex_pattern, fixed_pattern, fixed_replacement) {
    
    has_doi <- x |> grepl(pattern = regex_pattern)
    
    doi <- x[has_doi] |> 
      stri_extract_all_regex(pattern = regex_pattern)
    if (!all(lengths(doi) == 1L)) stop('one citation cannot have >1 doi')
    
    x[has_doi] <- doi |>
      unlist() |>
      stri_replace_all_fixed(pattern = fixed_pattern, replacement = fixed_replacement, vectorize_all = FALSE) |> # to get [topic](url)
      stri_replace_all_regex(str = x[has_doi], pattern = regex_pattern)
    
    return(x)
    
  }
  
  x |> 
    dropManual() |>
    sort_by.bibentry(y = 'year', decreasing = TRUE) |>
    url2doi() |>
    format(style = 'text') |> # ?utils:::format.bibentry
    
    # tzh does not know where '\n' comes from
    # or whether tzh is capable of removing it using parameters of ?utils:::format.bibentry  
    gsub(pattern = '\n', replacement = ' ') |> 
    
    # REMOVED!!!  do not do this
    #gsub(pattern = '\"|\u201c|\u201d|\u2018|\u2019', replacement = '') |>
    # '\u201c|\u201d' # quotation marks created by ?base::dQuote
    # '\u2018|\u2019' # quotation marks created by ?base::sQuote
    
    format_doi(
      regex_pattern = '( doi:)(.*?)( <https://doi.org/)(.*?)(>)(.|,)', 
      fixed_pattern = c(' doi:', ' <https://doi.org/', '>'), 
      fixed_replacement = c(' [doi:', '](https://doi.org/', ')')
    )
  
}




if (FALSE) { # disabled for ?devtools::check
  library(parallel)
  ct = installed.packages() |>
    rownames() |>
    mclapply(FUN = citation, mc.cores = detectCores())
  
  tmp = ct |> 
    mclapply(FUN = format, style = 'text', mc.cores = detectCores()) |>
    unlist(recursive = TRUE)
  grep('\u2018', tmp) # none!!
  tmp[grep('\u2019', tmp)]
  # grep('\u201c', tmp)
  
  
  tmp = ct |> mclapply(FUN = md_.bibentry, mc.cores = detectCores())
  tmp[lengths(tmp) > 1L]
} 





#' @title Handy Tools for \link[utils]{bibentry}
#' 
#' @param x a \link[utils]{bibentry} object
#' 
#' @details
#' Function [url2doi()] converts a `url` field to `doi`, if it is a DOI URL.
#' 
#' @returns
#' All functions returns a \link[utils]{bibentry} object
#' 
#' @examples
#' 'stringi' |> citation() # using doi field, correct
#' 
#' 'texreg' |> citation() # doi in url field, not good!
#' 'texreg' |> citation() |> url2doi()
#' @seealso `` utils:::`[.bibentry` ``
#' @keywords internal
#' @name citation_ext
#' @export
url2doi <- function(x) {
  
  x0 <- x |>
    unclass()
  # must!!
  # see # methods(class = 'bibentry')
  # otherwise ?utils:::`[<-.bibentry` causes error hahaha
  
  x0[] <- x0 |> 
    lapply(FUN = url2doi.)
  
  class(x0) <- class(x)
  # attributes intact
  return(x0)
  
}




url2doi. <- function(b) {
  # (b = unclass(x)[[1L]]) # `x` is ?utils::bibentry object
  if (!length(b[['url']])) return(b)
  if (!grepl(pattern = 'https://doi.org/', x = b[['url']])) return(b)
  doi <- gsub(pattern = 'https://doi.org/', replacement = '', x = b[['url']])
  if (length(b[['doi']])) {
    if (!identical(b[['doi']], doi)) stop()
    # else do nothing
  } else b[['doi']] <- doi
  b[['url']] <- NULL
  return(b)
}





#' @rdname citation_ext
#' @details
#' Function [dropManual()] removes the CRAN `Manual` citation, 
#' if a package has one-or-more `Article` citation.
#' @examples
#' 'survival' |> citation()
#' 'survival' |> citation() |> dropManual()
#' @export
dropManual <- function(x) {
  
  # tzh is not sure whether to define [subset.bibentry], i.e.,
  # x |> subset.bibentry(subset = (bibtype != 'Manual'))
  
  nx <- length(x)
  if (nx == 1L) return(x)
  
  bibtype <- x |>
    unclass() |>
    vapply(FUN = attr, which = 'bibtype', exact = TRUE, FUN.VALUE = '')
  # see function ?utils:::`[[.bibentry` very very closely!!!
  
  isManual <- (bibtype == 'Manual')
  if (!any(isManual)) return(x)
    
  return(x[!isManual, drop = FALSE])
  
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









