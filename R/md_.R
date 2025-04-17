

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
#' list('`data.frame`' = swiss) |> render_(file = 'data.frame')
#' 
#' @export md_.data.frame
#' @export
md_.data.frame <- function(x, xnm, ...) {
  return(c(
    '```{r}',
    paste0('as_flextable_dataframe(', xnm, ')'),
    '```', 
    '<any-text>'
  ))
}


#' @rdname md_
#' @examples
#' list('`matrix`' = VADeaths) |> render_(file = 'matrix')
#' 
#' @export md_.array
#' @export
md_.array <- function(x, xnm, ...) {
  return(c(
    '```{r}',
    paste0('as_flextable.array(', xnm, ')'), # 3-dimension not working well now!!
    '```', 
    '<any-text>'
  ))
}



#' @rdname md_
#' @export md_.list
#' @export
md_.list <- function(x, xnm, ...) {
  x |> 
    seq_along() |>
    lapply(FUN = \(i) {
      c(md_(x = x[[i]], xnm = paste0(xnm, '[[', i, ']]'), ...), '\n\n')
    }) |> 
    unlist(recursive = FALSE, use.names = FALSE)
}

#' @rdname md_
#' @export md_.numeric
#' @export
md_.numeric <- function(x, ...) {
  paste(x, collapse = ', ')
}

#' @rdname md_
#' @export md_.character
#' @export
md_.character <- function(x, ...) x # not ?base::identity


#' @rdname md_
#' @export md_.noquote
#' @export
md_.noquote <- function(x, xnm, ...) {
  md_(x = unclass(x), xnm = sprintf(fmt = 'unclass(%s)', xnm), ...)
}



