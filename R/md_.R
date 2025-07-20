

# '<any-text>'
# 'word_document' needs *more* than '\n' to correctly separate two flextable's.  
# 'html_document' needs nothing, and will take '<>' as meaningless html tag (thus ignored) :)


#' @title Markdown Script
#' 
#' @description
#' To create markdown script for various objects.
#' 
#' @param x see **Usage**
#' 
#' @param xnm ..
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [md_()] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @keywords internal
#' @name md_
#' @export
md_ <- function(x, ...) {
  if (!length(x)) return(invisible())
  UseMethod(generic = 'md_')
}



#' @rdname md_
#' @examples
#' list('`data.frame`' = Formaldehyde) |> render_(file = 'data.frame')
#' 
#' @export md_.data.frame
#' @export
md_.data.frame <- function(x, xnm, ...) {
  c(
    '```{r}',
    xnm |> sprintf(fmt = '%s |> format4flextable() |> flextable() |> autofit(part = \'all\')'),
    '```'
  ) |> new(Class = 'md_lines')
}


#' @rdname md_
#' @examples
#' list('`xtabs`' = xtabs(~ cyl + vs, data = mtcars)) |> render_(file = 'xtabs')
#' 
#' @export md_.xtabs
#' @export
md_.xtabs <- function(x, xnm, ...) {
  c(
    '```{r}',
    xnm |> sprintf(fmt = '%s |> as_flextable() |> autofit(part = \'all\')'),
    '```'
  ) |> new(Class = 'md_lines')
}


#' @rdname md_
#' @examples
#' list('`matrix`' = VADeaths) |> render_(file = 'matrix')
#' 
#' @export md_.matrix
#' @export
md_.matrix <- function(x, xnm, ...) {
  c(
    '```{r}',
    paste0('as_flextable.matrix(', xnm, ')'), # 3-dimension not working well now!!
    '```'
  ) |> new(Class = 'md_lines')
}



#' @rdname md_
#' @export md_.list
#' @export
md_.list <- function(x, xnm, ...) {
  
  ret0 <- x |> 
    seq_along() |>
    lapply(FUN = \(i) {
      md_(x = x[[i]], xnm = paste0(xnm, '[[', i, ']]'), ...)
    }) 
  
  ret <- ret0 |> 
    unlist(recursive = FALSE, use.names = FALSE)
  
  attr(ret, which = 'bibentry') <- ret0 |> 
    collect_attr_(which = 'bibentry')
  return(ret)
  
}

#' @rdname md_
#' @export md_.numeric
#' @export
md_.numeric <- function(x, ...) {
  paste(x, collapse = ', ') |> new(Class = 'md_lines')
}

#' @rdname md_
#' @export md_.character
#' @export
md_.character <- function(x, ...) x |> new(Class = 'md_lines')


#' @rdname md_
#' @export md_.noquote
#' @export
md_.noquote <- function(x, xnm, ...) {
  md_(x = unclass(x), xnm = sprintf(fmt = 'unclass(%s)', xnm), ...)
}



