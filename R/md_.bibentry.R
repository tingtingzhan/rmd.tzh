

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
  
  return(sprintf(
    fmt = '<u>**`%s`**</u> %s\n', 
    if (pkg == 'base') 'R' else pkg, 
    x |> bibentry2text()
  ))
  
}



#' @title Slight Improvement over `utils:::format.bibentry`
#' 
#' @param x a \link[utils]{citation} and/or \link[utils]{bibentry}
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
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed stri_replace_all_regex
#' @keywords internal
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
    sortYear(decreasing = TRUE) |>
    url2doi() |>
    format(style = 'text') |> # ?utils:::format.citation -> ?utils:::format.bibentry
    gsub(pattern = '\n', replacement = ' ') |>
    gsub(pattern = '\"|\u201c|\u201d|\u2018|\u2019', replacement = '') |>
    # '\u201c|\u201d' # quotation marks created by ?base::dQuote
    # '\u2018|\u2019' # quotation marks created by ?base::sQuote
    format_doi(
      regex_pattern = '( doi:)(.*?)( <https://doi.org/)(.*?)(>)(.|,)', 
      fixed_pattern = c(' doi:', ' <https://doi.org/', '>'), 
      fixed_replacement = c(' [doi:', '](https://doi.org/', ')')
    )
  
}




if (FALSE) { # disabled for ?devtools::check
  ct = installed.packages() |>
    rownames() |>
    lapply(FUN = citation) # slow
  tmp = ct |> lapply(FUN = md_.bibentry)
  tmp[lengths(tmp) > 1L]
} 





#' @title Handy Tools for \link[utils]{citation}
#' 
#' @param x a \link[utils]{citation} and/or \link[utils]{bibentry}
#' 
#' @details
#' Function [url2doi()] converts a `url` field to `doi`, if it is a DOI URL.
#' 
#' @returns
#' All functions returns a \link[utils]{citation} and/or \link[utils]{bibentry}.
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
  
  ret <- x |>
    unclass() |> 
    lapply(FUN = \(b) { # (b = unclass(x)[[1L]])
      url_ <- b[['url']] # name clash ?base::url
      if (!length(url_)) return(b)
      if (!grepl(pattern = 'https://doi.org/', x = url_)) return(b)
      doi <- gsub(pattern = 'https://doi.org/', replacement = '', x = url_)
      if (length(b[['doi']])) {
        if (!identical(b[['doi']], doi)) stop()
        # else do nothing
      } else b[['doi']] <- doi
      b[['url']] <- NULL
      return(b)
    })
  
  attributes(ret) <- attributes(x)
  return(ret)
  
}


#' @rdname citation_ext
#' @details
#' Function [dropManual()] removes the CRAN `Manual` citation, 
#' if a package has one-or-more `Article` citation.
#' @examples
#' 'kernlab' |> citation()
#' 'kernlab' |> citation() |> dropManual()
#' @export
dropManual <- function(x) {
  
  nx <- length(x)
  if (nx == 1L) return(x)
  
  bibtype <- x |>
    unclass() |>
    vapply(FUN = attr, which = 'bibtype', exact = TRUE, FUN.VALUE = '')
  
  isManual <- (bibtype == 'Manual')
  if (!any(isManual)) return(x)
    
  return(x[!isManual, drop = FALSE])
  
}



#' @rdname citation_ext
#' @param decreasing \link[base]{logical} scalar, see function \link[base]{order}
#' @details
#' Function [sortYear()] sorts multiple citations of one package by year.
#' Tingting Zhan doesn't want to define `sort_by.citation()`, which sounds a little creepy :))
#' @examples
#' 'rmarkdown' |> citation()
#' 'rmarkdown' |> citation() |> sortYear()
#' @export
sortYear <- function(x, decreasing = TRUE) {
  
  nx <- length(x)
  if (nx == 1L) return(x)
  
  year <- x |>
    unclass() |>
    vapply(FUN = \(i) i[['year']], FUN.VALUE = '') |> 
    as.numeric()
  
  return(x[order(year, decreasing = decreasing), drop = FALSE])
  
}




