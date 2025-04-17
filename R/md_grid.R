

md_grid <- function(x, xnm, ...) {
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    '```{r}', # let \pkg{grid} figure out the width and height!!!
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
