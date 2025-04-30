

md_print0_ <- function(x, xnm, ...) {
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    '```{r comment = NA}',
    (attr(x, which = 'fig.height', exact = TRUE) %||% 4) |> sprintf(fmt = '#| fig-height: %.1f'),
    (attr(x, which = 'fig.width', exact = TRUE) %||% 7) |> sprintf(fmt = '#| fig-width: %.1f'),
    xnm, # print, but not say print
    # invokes
    # ?flextable:::print.flextable
    # ?htmlwidgets:::print.htmlwidget
    # etc.
    '```',
    '<any-text>'
  ))
}


md_print_ <- function(x, xnm, ...) {
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    '```{r comment = NA}',
    (attr(x, which = 'fig.height', exact = TRUE) %||% 4) |> sprintf(fmt = '#| fig-height: %.1f'),
    (attr(x, which = 'fig.width', exact = TRUE) %||% 7) |> sprintf(fmt = '#| fig-width: %.1f'),
    sprintf(fmt = '%s |> print() |> suppressWarnings()', xnm),
    # invokes
    # ?stats:::print.htest
    # ?stats:::print.power.htest
    # ?ggplot2:::print.ggplot
    # ?GGally:::print.ggmatrix
    # etc.
    '```',
    '<any-text>'
  ))
}


#' @rdname md_
#' @examples
#' library(ggplot2); list(
#'   '`htest`' = t.test(mpg ~ am, data = mtcars),
#'   '`power.htest`' = power.t.test(power = .90, delta = 1),
#'   '`ggplot2::ggplot`' = ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'   '`GGally::ggmatrix`' = GGally::ggpairs(swiss, columns = c(1:2, 6))
#' ) |> render_(file = 'Explicit Print')
#' 
#' @export md_.gg
#' @export
md_.gg <- md_print_

#' @rdname md_
#' @export md_.htest
#' @export
md_.htest <- md_print_ # md_print0; either okay

#' @rdname md_
#' @method md_ power.htest
#' @export md_.power.htest
#' @export
md_.power.htest <- md_print_ # md_print0; either okay


#' @rdname md_
#' 
#' @note
#' Read \url{https://plotly.com/r/subplots/} 
#' on how to stack `'plotly'` objects 
#' (via function \link[plotly]{subplot}).
#' 
#' @examples
#' library(flextable); library(reactable); library(plotly); 
#' list(
#'  '`flextable::flextable`' = Formaldehyde |> flextable(),
#'  '`reactable::reactable`, an `htmlwidget`' = Formaldehyde |> reactable(),
#'  '`htmlwidget`' = list(
#'    plot_ly(economics, x = ~date, y = ~pop, type = 'scatter', mode = 'markers'),
#'    plot_ly(z = ~volcano, type = "surface")
#'  )
#' ) |> render_(file = 'Do Not Say Print')
#' 
#' @export md_.flextable
#' @export
md_.flextable <- md_print0_ # md_print_ *not* okay!!

#' @rdname md_
#' @export md_.htmlwidget
#' @export
md_.htmlwidget <- md_print0_ # md_print_ *not* okay!!

