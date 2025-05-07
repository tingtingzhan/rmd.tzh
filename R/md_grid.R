

md_grid <- function(x, xnm, ...) {
  
  fig.cap <- attr(x, which = 'fig.cap', exact = TRUE)
  
  #label <- attr(x, which = 'label', exact = TRUE)
  # https://bookdown.org/yihui/rmarkdown-cookbook/cross-ref.html
  # rmarkdown does *not* provide cross-referencing 
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    '```{r}', 
    # let \pkg{grid} does not figure out the width and height very perfectly
    (attr(x, which = 'fig.height', exact = TRUE) %||% 4) |> sprintf(fmt = '#| fig-height: %.1f'),
    (attr(x, which = 'fig.width', exact = TRUE) %||% 7) |> sprintf(fmt = '#| fig-width: %.1f'),
    fig.cap |> sprintf(fmt = '#| fig-cap: %s'), # len-0 compatible
    # 'grid::grid.newpage()', # no need!!
    sprintf(fmt = '%s |> grid::grid.draw()', xnm), 
    # ?grid:::grid.draw.gList
    # ?grid:::grid.draw.grob
    # etc.
    '```'
  ))
}




#' @rdname md_
#' @examples
#' library(grid.tzh); list(
#'   '`venn`' = venn(list(A = state.name[1:20], B = state.name[15:30]))
#' ) |> render_(file = 'gList')
#' 
#' @export md_.gList
#' @export
md_.gList <- md_grid



#' @rdname md_
#' @note
#' Function [md_.gDesc()] is useful (e.g., returned value of function \link[grid.tzh]{consort_rx})
#' @export md_.gDesc
#' @export
md_.gDesc <- md_grid

if (FALSE) {
  list(
    figure = const_all # Rupsa's study
  ) |> render_(file = 'gDesc')
}
