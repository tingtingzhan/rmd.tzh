
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
#' library(consort); data(dispos.data)
#' list(
#'  '`trellis`' = xyplot(lat ~ long | Depth, data = quakes),
#'  '`consort`' = consort_plot(
#'   data = dispos.data |> subset(subset = !(arm3 %in% 'Trt C')),
#'   orders = list(c(trialno = 'Population'), c(exclusion = 'Excluded'), c(arm = 'Randomized')),
#'   side_box = c('exclusion'))
#' ) |> render_(file = 'Vanilla Plot')
#' 
#' @export md_.trellis
#' @export
md_.trellis <- md_plot_ 

#' @rdname md_
#' @export md_.consort
#' @export
md_.consort <- md_plot_


