
#' @title Markdown Script for Objects from Package \pkg{stats}
#' 
#' @param x an object returned by a function from package \pkg{stats}
#' 
#' @param ... ..
#' 
#' @examples
#' list(factanal = datasets::USJudgeRatings[-1L] |> stats::factanal(factors = 3L)) |>
#'   render_(file = 'factanal')
#' 
#' @keywords internal
#' @name md_stats
#' @importFrom utils bibentry
#' @export
md_.factanal <- function(x, ...) {
  
  attr(x, which = 'text') <- x$call$x |>
    deparse1() |>
    sprintf(fmt = 'Factor analysis [@LawleyMaxwell71] of `%s` is performed using <u>**`R`**</u>.') |>
    new(Class = 'md_lines', bibentry = bibentry(
      bibtype = 'Article', key = 'LawleyMaxwell71',
      title = 'Factor Analysis as a Statistical Method',
      journal = 'Journal of the Royal Statistical Society. Series D (The Statistician)',
      author = 'D. N. Lawley and A. E. Maxwell', 
      year = '1962',
      doi = '10.2307/2986915',
      pages = '209--229',
      volume = '12', number = '3',
      publisher = 'Royal Statistical Society, Wiley'
      #SN  - 00390526, 14679884
    ))
  
  md_.default(x, ...)
  
}