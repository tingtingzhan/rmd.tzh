

#' @title Create and Render R Markdown file
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @param document \link[base]{character} scalar, type of output document,
#' `'html'` (default), `'word'` or `'pdf'`
#' 
#' @param path \link[base]{character} scalar 
#' 
#' @param file \link[base]{character} scalar
#' 
#' @param rmd.rm \link[base]{logical} scalar, whether to remove the R markdown `'.rmd'` file,
#' default `TRUE`
#' 
#' @param bib.rm \link[base]{logical} scalar, whether to remove the bibliography `'.bib'` file,
#' default `TRUE`
#' 
#' @param ... ..
#' 
#' @importFrom rmarkdown render
#' @importFrom utils citation
#' @export
render_ <- function(
    x, 
    path = tempdir(),
    document = c('html', 'word', 'pdf'),
    file = stop('must specify `file` explicitly'),
    rmd.rm = TRUE,
    bib.rm = TRUE,
    ...
) {
  
  x <- x[lengths(x) > 0L]
  if (!(nx <- length(x))) return(invisible())
  
  document <- match.arg(document)
  
  path <- file.path(path, document)
  dir.create(path = path, showWarnings = FALSE, recursive = TRUE)
  
  if (length(file) != 1L || !is.character(file) || is.na(file) ||
      grepl(pattern = '\\:', x = file)) stop('`file` must be len-1 character, without ', sQuote(':'))
  frmd <- file.path(path, sprintf(fmt = '%s %s.rmd', format.Date(Sys.Date()), file))
  fout <- file.path(path, sprintf(fmt = '%s %s.%s', format.Date(Sys.Date()), file, switch(document, word = 'docx', document)))
  if (file.exists(fout)) {
    if (document == 'word') system('osascript -e \'quit app "Word"\'') # Word will not automatically close when the .docx file is deleted
    file.remove(fout)
    message('Existing ', sQuote(basename(fout)), ' removed')
  }

  nm <- names(x)
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('names must be complete')
  
  md. <- nx |> 
    seq_len() |>
    lapply(\(i) md_(x = x[[i]], xnm = sprintf(fmt = 'x[[%d]]', i)))
  
  md_ <- .mapply(FUN = c, dots = list(
    nm |> sprintf(fmt = '\n# %s\n'), # must use an extra '\n' to separate from previous 'character'
    md.
  ), MoreArgs = NULL) |>
    unlist(use.names = FALSE)
  
  c(
    r_yaml_(title = file, document = document, bib = md. |> collect_bibentry(), path = path, ...), 
    '\n', 
    r_css_(),
    '\n',
    '```{r}',
    '#| include: false',
    'options(bitmapType = \'cairo\')', # for correct unicode support; DO I STILL NEED THIS ??
    'library(knitr)',
    'opts_chunk$set(echo = FALSE)',
    'library(flextable)', # e.g., flextable::as_flextable
    'library(flextable.tzh)', # e.g. flextable.tzh::as_flextable.matrix
    'library(scales.tzh)',
    '```',
    '\n',
    md_, 
    '\n',
    '# Citations',
    md_ |> 
      extract_pkg_name() |> 
      lapply(FUN = \(i) i |> citation() |> md_.bibentry()) |>
      unlist(use.names = FALSE)
  ) |>
    writeLines(con = frmd, sep = '\n')
  
  render(input = frmd, output_file = fout, intermediates_dir = path, quiet = TRUE)
  paste0('open \'', normalizePath(fout), '\'') |> system()
  
  if (rmd.rm) file.remove(frmd) else paste0('open \'', normalizePath(frmd), '\'') |> system()
  
  bibfile <- path |> 
    list.files(pattern = '\\.bib$', full.names = TRUE)
  if (length(bibfile)) {
    if (bib.rm) file.remove(bibfile) else paste0('open \'', normalizePath(bibfile), '\'') |> system()
    # system() probably works with len-1 file only
  }
  
  return(invisible(fout))
}


collect_bibentry <- function(x) {
  # `x` is a 'list' with 'character' elements
  
  bib <- x |> 
    lapply(FUN = attr, which = 'bibentry', exact = TRUE) 
  
  id <- (lengths(bib, use.names = FALSE) > 0L)
  if (!any(id)) return(invisible())
  
  ret <- bib[id] |> # ?utils:::c.bibentry cannot take non-bibentry input
    do.call(what = c, args = _) |> # ?utils:::c.bibentry
    unique() # ?utils:::unique.bibentry
  
  keys <- ret |>
    unclass() |>
    vapply(FUN = attr, which = 'key', exact = TRUE, FUN.VALUE = '')
  if (anyDuplicated.default(keys)) stop('same key(s) from different bibliography items')
  
  return(ret)

}









