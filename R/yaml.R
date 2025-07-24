

#' @title YAML Header
#' 
#' @param title \link[base]{character} scalar
#' 
#' @param author \link[base]{character} scalar, author's name and/or email
#' 
#' @param document \link[base]{character} scalar
#' 
#' @param bib (optional) \link[utils]{bibentry} object
#' 
#' @param path (optional) \link[base]{character} scalar, directory of `.bib` file when `bib` argument exists
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @keywords internal
#' @importFrom utils bibentry toBibtex
#' @export
r_yaml_ <- function(
    title, 
    author = 'tingting.zhan@jefferson.edu',
    document,
    bib = bibentry(),
    path = NULL, # used when (length(bib) > 0L)
    ...
) {
  
  c(
    '---',
    
    title |> sprintf(fmt = 'title: %s'),
    author |> sprintf(fmt = 'author: %s'),
    Sys.time() |> format.POSIXct() |> sprintf(fmt = 'date: %s'),
    
    switch(document, html = { # 'https://bookdown.org/yihui/rmarkdown/html-document.html' |> browseURL()
      c(
        'output:',
        '  html_document:',
        '    toc: true',
        '    toc_float: true'
      )
    }, word = { # 'https://bookdown.org/yihui/rmarkdown/word-document.html' |> browseURL()
      c(
        'output: word_document',
        'always_allow_html: true'
      )
    }, pdf = { # 'https://bookdown.org/yihui/rmarkdown/pdf-document.html' |> browseURL()
      c(
        'output:', 
        '  pdf_document:',
        '    latex_engine: xelatex',
        '    toc: true',
        'always_allow_html: true'
      )
    }),
    
    if (length(bib)) {
      if (!length(path)) stop('must provide `path` when `bib` exists')
      bibfile <- file.path(path, 'bibliography.bib')
      if (file.exists(bibfile)) file.remove(bibfile) # without warning
      bibfile |> file.create()
      bibfile |> sink()
      bib |> toBibtex() |> print() # here it is *difficult* to use my [bibentry2text] !!!!!
      sink()
      'bibliography: bibliography.bib' # written to YAML
    }, # else NULL
    
    '---'
  )
  
}


#' @title CSS Rule
#' 
#' @keywords internal
#' @name css
#' @export
r_css_ <- function() {
  # 'https://stackoverflow.com/questions/34906002' |> browseURL()
  c(
    '<style type=\"text/css\">',
    '  .main-container {',
    '    max-width: 100% !important;',
    # '    margin: auto;',
    # '    margin-left: auto;',
    # '    margin-right: auto;',
    # `auto` looks the same as `0px`
    '  }',
    '</style>'
  )  
}



