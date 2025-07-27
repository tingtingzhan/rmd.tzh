

md_print0_ <- function(x, xnm, ...) {
  
  z1 <- (attr(x, which = 'text', exact = TRUE) %||% character()) |>
    new(Class = 'md_lines')
  
  z2 <- c(
    '```{r}',
    '#| warning: false', 
    '#| comment:',
    (attr(x, which = 'fig-height', exact = TRUE) %||% 4) |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    (attr(x, which = 'fig-width', exact = TRUE) %||% 7) |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    x |>
      attr(which = 'fig.cap', exact = TRUE) |> 
      sprintf(fmt = '#| fig-cap: %s'), # len-0 compatible
    
    xnm, # print, but not say print
    # must *not* say print, to correctly invoke
    # ?flextable:::print.flextable
    # ?htmlwidgets:::print.htmlwidget
    # etc.
    
    # okay to say or not say print, i.e., `xnm |> sprintf(fmt = '%s |> print()')`
    # ?stats:::print.htest
    # ?stats:::print.power.htest
    # ?ggplot2:::print.ggplot
    # ?GGally:::print.ggmatrix
    # ?magick:::`print.magick-image`
    # etc.
    
    '```'
  ) |> new(Class = 'md_lines')
  
  return(c(z1, z2)) # [c.md_lines()]
  
}






#' @rdname md_
#' 
#' @note
#' Read \url{https://plotly.com/r/subplots/} 
#' on how to stack `'plotly'` objects 
#' (via function \link[plotly]{subplot}).
#' 
#' @examples
#' library(ggplot2)
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
#'  '`htest`' = t.test(mpg ~ am, data = mtcars),
#'  '`power.htest`' = power.t.test(power = .90, delta = 1),
#'  '`ggplot2::ggplot`' = ggplot() + geom_point(data = mtcars, mapping = aes(wt, mpg)),
#'  '`GGally::ggmatrix`, an `S7_object`' = GGally::ggpairs(swiss, columns = c(1:2, 6)),
#'  '`flextable::flextable`' = Formaldehyde |> flextable::flextable(),
#'  '`magick-image` from package `magick`' = magick::wizard,
#'  '`reactable::reactable`, an `htmlwidget`' = Formaldehyde |> reactable::reactable(),
#'  '`leaflet::leaflet`, an `htmlwidget`' = washingtonDC,
#'  '`htmlwidget`' = list(
#'    plotly::plot_ly(ggplot2::economics, x = ~date, y = ~pop, type = 'scatter', mode = 'markers'),
#'    plotly::plot_ly(z = ~volcano, type = "surface")
#'  )
#' ) |> render_(file = 'Do Not (Need to) Say Print')
#' 
#' @export md_.flextable
#' @export
md_.flextable <- md_print0_

#' @rdname md_
#' @export md_.htmlwidget
#' @export
md_.htmlwidget <- md_print0_

#' @rdname md_
#' @export md_.gg
#' @export
md_.gg <- md_print0_

#' @rdname md_
#' @export md_.S7_object
#' @export
md_.S7_object <- md_print0_
# since GGally 2.3.0 on 2025-07-17
# 'ggmatrix' no longer inherits from 'gg' in \CRANpkg{ggplot2}
# but from ?S7::S7_object
# actually, \CRANpkg{ggplot2} v4.0.0 is switching to \CRANpkg{S7} !!! 

#' @rdname md_
#' @export md_.htest
#' @export
md_.htest <- md_print0_

#' @rdname md_
#' @method md_ power.htest
#' @export md_.power.htest
#' @export
md_.power.htest <- md_print0_


#' @rdname md_
#' @export `md_.magick-image`
#' @export
`md_.magick-image` <- md_print0_
# ?magick::image_write; write to a file
# ?magick::image_info; height and width


