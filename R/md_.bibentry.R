

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
#' @importFrom bibentry url2doi
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
    mclapply(mc.cores = detectCores(), FUN = citation)
  
  tmp = ct |> 
    mclapply(mc.cores = detectCores(), FUN = format, style = 'text') |>
    unlist(recursive = TRUE)
  grep('\u2018', tmp) # none!!
  tmp[grep('\u2019', tmp)]
  # grep('\u201c', tmp)
  
  
  tmp = ct |> 
    mclapply(mc.cores = detectCores(), FUN = md_.bibentry)
  tmp[lengths(tmp) > 1L]
} 





