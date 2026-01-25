

#' @title [md_.htest]
#' 
#' @param x an `htest`
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [md_.htest()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @importFrom methods new
#' @export md_.htest
#' @export
md_.htest <- function(x, xnm, ...) {
  
  data.name <- tryCatch(expr = {
    x$data.name |>
      str2lang()
  }, error = identity)
  dnm <- if (inherits(data.name, what = 'error')) {
    x$data.name
  } else if (is.symbol(data.name)) {
    x$data.name
  } else if (data.name[[1L]] == 'xtabs') {
    if (is.null(data.name$formula)) stop('call stats::xtabs() with `formula =`')
    data.name$formula[[2L]] |>
      deparse1() |>
      strsplit(split = ' \\+ ') |>
      unlist() |>
      paste0('`', .x =_, '`', collapse = ' and ')
  } else { # exception handling
    x$data.name |>
      deparse1()
  }
  
  z1 <- sprintf(
    fmt = '%s of %s (%s) is performed using <u>**`R`**</u>.',
    x$method,
    dnm,
    x$p.value |>
      label_pvalue_sym(add_p = TRUE)()
  ) |>
    new(Class = 'md_lines', package = 'survival')
  
  z2 <- if (!missing(xnm)) {
    c(
      '```{r}',
      '#| echo: false', 
      '#| comment: ',
      xnm,
      '```'
    ) |> 
      new(Class = 'md_lines')
  } # else NULL
  
  c(z1, z2)
  
}
