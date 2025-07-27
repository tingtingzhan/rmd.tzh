

# '<any-text>'
# 'word_document' needs *more* than '\n' to correctly separate two flextable's.  
# 'html_document' needs nothing, and will take '<>' as meaningless html tag (thus ignored) :)


#' @title Markdown Script
#' 
#' @description
#' To create markdown script for various objects.
#' 
#' @param x see **Usage**
#' 
#' @param xnm ..
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [md_()] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @keywords internal
#' @name md_
#' @export
md_ <- function(x, ...) {
  if (!length(x)) return(invisible())
  UseMethod(generic = 'md_')
}



#' @rdname md_
#' 
#' @note
#' Read \url{https://plotly.com/r/subplots/} 
#' on how to stack `'plotly'` objects 
#' (via function \link[plotly]{subplot}).
#' 
#' As for now (early 2025), tzh prefers package \pkg{patchwork} over function `ggpubr::ggarrange`.
#' 
#' @examples
#' library(lattice); Depth = equal.count(quakes$depth, number=8, overlap=.1)
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
#' library(patchwork) # ?patchwork::`patchwork-package`
#' p1 = ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 = ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#' 
#' list(
#'  '`htest`' = t.test(mpg ~ am, data = mtcars),
#'  '`power.htest`' = power.t.test(power = .90, delta = 1),
#'  '`trellis` from package `lattice`' = xyplot(lat ~ long | Depth, data = quakes),
#'  '`ggplot2::ggplot`' = ggplot() + geom_point(data = mtcars, mapping = aes(wt, mpg)),
#'  '`GGally::ggmatrix`, an `S7_object`' = GGally::ggpairs(swiss, columns = c(1:2, 6)),
#'  '`patchwork` from package `patchwork`' = p1 + p2,
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
#' @export md_.default
#' @export
md_.default <- function(x, xnm, ...) {
  
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
    # ?S7:::print.S7_object
    ### since GGally 2.3.0 on 2025-07-17
    ### 'ggmatrix' no longer inherits from 'gg' in \CRANpkg{ggplot2}
    ### but from ?S7::S7_object
    ### actually, \CRANpkg{ggplot2} v4.0.0 is switching to \CRANpkg{S7} !!! 
    # ?magick:::`print.magick-image`
    ### ?magick::image_write; write to a file
    ### ?magick::image_info; height and width
    # ?lattice::trellis.*()
    # etc.
    
    '```'
  ) |> new(Class = 'md_lines')
  
  return(c(z1, z2)) # [c.md_lines()]
  
}









#' @rdname md_
#' @examples
#' list('`data.frame`' = Formaldehyde) |> render_(file = 'data.frame')
#' 
#' @export md_.data.frame
#' @export
md_.data.frame <- function(x, xnm, ...) {
  c(
    '```{r}',
    xnm |> sprintf(fmt = '%s |> format4flextable() |> flextable() |> autofit(part = \'all\')'),
    '```'
  ) |> new(Class = 'md_lines')
}


#' @rdname md_
#' @examples
#' list('`xtabs`' = xtabs(~ cyl + vs, data = mtcars)) |> render_(file = 'xtabs')
#' 
#' @export md_.xtabs
#' @export
md_.xtabs <- function(x, xnm, ...) {
  c(
    '```{r}',
    xnm |> sprintf(fmt = '%s |> as_flextable() |> autofit(part = \'all\')'),
    '```'
  ) |> new(Class = 'md_lines')
}


#' @rdname md_
#' @examples
#' list('`matrix`' = VADeaths) |> render_(file = 'matrix')
#' 
#' @export md_.matrix
#' @export
md_.matrix <- function(x, xnm, ...) {
  c(
    '```{r}',
    paste0('as_flextable.matrix(', xnm, ')'), # 3-dimension not working well now!!
    '```'
  ) |> new(Class = 'md_lines')
}



#' @rdname md_
#' @export md_.list
#' @export
md_.list <- function(x, xnm, ...) {
  x |> 
    seq_along() |>
    lapply(FUN = \(i) {
      md_(x = x[[i]], xnm = paste0(xnm, '[[', i, ']]'), ...)
    }) |>
    do.call(what = c.md_lines, args = _)
}

#' @rdname md_
#' @export md_.numeric
#' @export
md_.numeric <- function(x, ...) {
  paste(x, collapse = ', ') |> new(Class = 'md_lines')
}

#' @rdname md_
#' @export md_.character
#' @export
md_.character <- function(x, ...) x |> new(Class = 'md_lines')


#' @rdname md_
#' @export md_.noquote
#' @export
md_.noquote <- function(x, xnm, ...) {
  md_(x = unclass(x), xnm = sprintf(fmt = 'unclass(%s)', xnm), ...)
}



