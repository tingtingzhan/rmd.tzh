

#' @title Markdown Lines of \link[utils]{bibentry} Object
#' 
#' @param x a \link[utils]{bibentry} object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @examples 
#' list(
#'  texreg = 'R regression output to LaTeX or HTML by <u>**`R`**</u> package <u>**`texreg`**</u>.' |> 
#'    new(Class = 'md_lines', package = 'texreg'),
#'  stats = 'Linear regression by <u>**`R`**</u> package <u>**`stats`**</u>.' |> 
#'    new(Class = 'md_lines', package = 'stats')
#' ) |> render_(file = 'bibentry')
#' 
#' @keywords internal
#' @export md_.bibentry
#' @export
md_.bibentry <- function(x, ...) {
  
  pkg <- attr(x, which = 'package', exact = TRUE)
  
  sprintf(
    fmt = '<u>**`%s`**</u> %s\n', 
    switch(pkg, base = 'R', pkg), 
    pkg |>
      citation() |>
      url2doi() |> 
      format(style = 'text') # utils:::format.bibentry
  ) |> 
    new(Class = 'md_lines')
  
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
  
  
  tmp = ct |> 
    mclapply(FUN = md_.bibentry, mc.cores = detectCores())
  tmp[lengths(tmp) > 1L]
} 





#' @title Prioritize `doi` over `url` in \link[utils]{bibentry}
#' 
#' @param x a \link[utils]{bibentry} object
#' 
#' @details
#' Function [url2doi()] converts a `url` field to `doi` field in \link[utils]{bibentry}, 
#' if the `url` field is a DOI URL.
#' 
#' @returns
#' Function [url2doi()] returns a \link[utils]{bibentry} object.
#' 
#' @examples
#' 'scales' |> citation() # using doi field, correct
#' 
#' 'texreg' |> citation() # doi in url field, not good!
#' 'texreg' |> citation() |> url2doi()
#' @keywords internal
#' @export
url2doi <- function(x) {
  
  x0 <- x |>
    unclass()
  # must!!
  # see # methods(class = 'bibentry')
  # otherwise both ?utils:::`[[.bibentry` and ?utils:::`[<-.bibentry` cause error hahaha
  
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
  doi <- b[['url']] |>
    gsub(pattern = 'https://doi.org/', replacement = '', x = _)
  if (length(b[['doi']])) {
    if (!identical(b[['doi']], doi)) stop()
    # else do nothing
  } else b[['doi']] <- doi
  b[['url']] <- NULL
  return(b)
}


