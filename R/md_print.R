

md_print0_ <- function(x, xnm, ...) {
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    '```{r comment = NA}',
    '#| warning: false', 
    (attr(x, which = 'fig.height', exact = TRUE) %||% 4) |> sprintf(fmt = '#| fig-height: %.1f'),
    (attr(x, which = 'fig.width', exact = TRUE) %||% 7) |> sprintf(fmt = '#| fig-width: %.1f'),
    xnm, # print, but not say print
    # invokes
    # ?flextable:::print.flextable
    # ?htmlwidgets:::print.htmlwidget
    # etc.
    '```',
    '<any-text>',
    '\n\n'
  ))
}


md_print_ <- function(x, xnm, ...) {
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    '```{r comment = NA}',
    '#| warning: false', 
    (attr(x, which = 'fig.height', exact = TRUE) %||% 4) |> sprintf(fmt = '#| fig-height: %.1f'),
    (attr(x, which = 'fig.width', exact = TRUE) %||% 7) |> sprintf(fmt = '#| fig-width: %.1f'),
    xnm |> sprintf(fmt = '%s |> print()'),
    # invokes
    # ?stats:::print.htest
    # ?stats:::print.power.htest
    # ?ggplot2:::print.ggplot
    # ?GGally:::print.ggmatrix
    # etc.
    '```',
    '<any-text>',
    '\n\n'
  ))
}


#' @rdname md_
#' @examples
#' library(ggplot2); list(
#'   '`htest`' = t.test(mpg ~ am, data = mtcars),
#'   '`power.htest`' = power.t.test(power = .90, delta = 1),
#'   '`ggplot2::ggplot`' = ggplot() + geom_point(data = mtcars, mapping = aes(wt, mpg)),
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
#' library(leaflet)
#' washingtonDC = leaflet() |>
#'   addTiles() |>
#'   fitBounds(lat1 = 38.85, lat2 = 38.92, lng1 = -77.07, lng2 = -77.0) |>
#'   addPopups(
#'     lng = c(-77.0365, -77.0563), lat = c(38.8977, 38.8719), 
#'     popup = c('white house', 'pentagon')
#'   )
#' 
#' list(
#'  '`flextable::flextable`' = Formaldehyde |> flextable::flextable(),
#'  '`reactable::reactable`, an `htmlwidget`' = Formaldehyde |> reactable::reactable(),
#'  '`leaflet::leaflet`, an `htmlwidget`' = washingtonDC,
#'  '`htmlwidget`' = list(
#'    plotly::plot_ly(ggplot2::economics, x = ~date, y = ~pop, type = 'scatter', mode = 'markers'),
#'    plotly::plot_ly(z = ~volcano, type = "surface")
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

