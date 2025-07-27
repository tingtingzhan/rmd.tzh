

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
  
  # **not** [md_.list()]; as we need section titles
  md <- nx |> 
    seq_len() |>
    lapply(FUN = \(i) {
      c.md_lines(
        nm[i] |> 
          sprintf(fmt = '\n# %s\n') |> 
          new(Class = 'md_lines'), # must use an extra '\n' to separate from previous 'character'
        md_(x = x[[i]], xnm = sprintf(fmt = 'x[[%d]]', i))
      )
    }) |> 
    do.call(what = c.md_lines, args = _)
  # end of **not** [md_.list()]
  
  c(
    r_yaml_(title = file, document = document, bib = md@bibentry, path = path, ...), 
    '\n', 
    r_css_(),
    '\n',
    '```{r}',
    '#| include: false',
    # 'options(bitmapType = \'cairo\')', # for correct Unicode support
    # 'library(knitr)',
    'knitr::opts_chunk$set(echo = FALSE)',
    'library(flextable.tzh)', # also loads \CRANpkg{flextable}
    'library(scales.tzh)',
    '```',
    '\n',
    md, 
    '\n',
    '# Citations',
    c('base', md@package) |> 
      sort.int() |>
      lapply(FUN = \(i) i |> citation() |> md_.bibentry()) |>
      unlist(use.names = FALSE)
  ) |>
    writeLines(con = frmd, sep = '\n')
  
  render(input = frmd, output_file = fout, intermediates_dir = path, quiet = TRUE)
  paste0('open \'', normalizePath(fout), '\'') |> system()
  
  if (rmd.rm) file.remove(frmd) else {
    frmd |>
      normalizePath() |>
      sprintf(fmt = 'open \'%s\'') |> 
      system()
  }
  
  bibfile <- path |> 
    list.files(pattern = '\\.bib$', full.names = TRUE)
  if (length(bibfile)) {
    if (bib.rm) file.remove(bibfile) else {
      bibfile |>
        normalizePath() |>
        sprintf(fmt = 'open \'%s\'') |> 
        lapply(FUN = system)
    }
  }
  
  return(invisible(fout))
  
}












#' @title Which Package Created This Object?
#' 
#' @param x an R object
#' 
#' @returns 
#' Function [fromPackage()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @importFrom stats getCall
#' @importFrom utils getAnywhere
#' @export
fromPackage <- function(x) {
  
  if (isS4(x)) {
    return(x |> 
             class() |> 
             attr(which = 'package', exact = TRUE))
  } 
  
  f <- getCall(x)[[1L]]
  pkg <- tryCatch(expr = {
    f |>
      eval() |> # function call could be un-exported, e.g., nlme:::lme.formula, and err
      environment() |>
      getNamespaceName()
  }, error = \(e) {
    aw <- f |>
      as.character() |>
      getAnywhere()
    if (length(aw$where) > 1L) stop('really shouldnt happen...')
    (aw$where) |>
      gsub(pattern = '^namespace\\:', replacement = '')
  })
  
  return(unname(pkg))
  
}


#' @title Text for Package to Create an R Object
#' 
#' @param x returned object of [fromPackage()]
#' 
#' @returns 
#' Function [pkg_text()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export
pkg_text <- function(x) {
  
  # `x` is the return of function [fromPackage()]
  
  # utils::installed.packages(priority = 'base') |> rownames() 
  # also, RStudio do not have a delete button for base-packages
  if (x %in% c('base', 'compiler', 'datasets', 'graphics', 'grDevices', 'grid', 'methods', 'parallel', 'splines', 'stats', 'stats4', 'tcltk', 'tools', 'utils')) {
    return('<u>**`R`**</u>')
  } 
  
  x |> 
    sprintf(fmt = '<u>**`R`**</u> package <u>**`%s`**</u>')
  
}







