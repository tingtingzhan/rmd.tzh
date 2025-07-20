
md_plot_ <- function(x, xnm, ...) {
  
  c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    '```{r}',
    (attr(x, which = 'fig.height', exact = TRUE) %||% 4) |> sprintf(fmt = '#| fig-height: %.1f'),
    (attr(x, which = 'fig.width', exact = TRUE) %||% 7) |> sprintf(fmt = '#| fig-width: %.1f'),
    sprintf(fmt = '%s |> plot()', xnm),
    # invokes
    # ?lattice:::plot.trellis
    # ?consort:::plot.consort
    # etc.
    '```'
  ) |> new(Class = 'md_lines')
}




#' @rdname md_
#' @examples
#' library(lattice); Depth = equal.count(quakes$depth, number=8, overlap=.1)
#' list(
#'  '`trellis`' = xyplot(lat ~ long | Depth, data = quakes)
#' ) |> render_(file = 'Vanilla Plot')
#' 
#' @export md_.trellis
#' @export
md_.trellis <- md_plot_ 

