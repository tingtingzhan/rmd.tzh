

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
#' @param rmd.rm \link[base]{logical} scalar, whether to remove the generated `'.rmd'` file,
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
  
  lrmd <- r_yaml_(title = file, document = document, ...)
  
  lrmd <- c(lrmd, '\n', r_css_())
  
  lrmd <- c(
    lrmd,
    '\n',
    '```{r}',
    '#| include: false',
    'options(bitmapType = \'cairo\')', # for correct unicode support; DO I STILL NEED THIS ??
    'library(knitr)',
    'opts_chunk$set(echo = FALSE)',
    'library(flextable)', # e.g., flextable::as_flextable
    'library(flextable.tzh)', # e.g. flextable.tzh::as_flextable.matrix
    'library(scales.tzh)',
    '```'
  )

  nm <- names(x)
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('names must be complete')
  
  for (i in seq_len(nx)) {
    lrmd <- c(
      lrmd, 
      '\n', # must use an extra '\n' (to separate from previous 'character')
      nm[i] |> sprintf(fmt = '# %s'),
      '\n',
      md_(x = x[[i]], xnm = sprintf(fmt = 'x[[%d]]', i)), 
      '\n'
    )
  }
  
  c(
    lrmd, 
    '\n',
    '# Citations',
    lrmd |> 
      extract_pkg_name() |> 
      lapply(FUN = \(i) i |> citation() |> md_.bibentry()) |>
      unlist(use.names = FALSE)
  ) |> 
    writeLines(con = frmd, sep = '\n')
  
  render(input = frmd, output_file = fout, intermediates_dir = path, quiet = TRUE)
  paste0('open \'', normalizePath(fout), '\'') |> system()
  
  if (rmd.rm) file.remove(frmd) else paste0('open \'', normalizePath(frmd), '\'') |> system()
  
  return(invisible(fout))
}








