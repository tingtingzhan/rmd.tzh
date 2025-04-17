
# packageDate('patchwork')
# packageDate('ggpubr')



#' @rdname md_
#' 
#' @param ncol \link[base]{integer} scalar for function [md_.patchwork()], see function \link[patchwork]{plot_layout}
#' 
#' @note
#' As for now (early 2025), tzh prefers package \pkg{patchwork} over 
#' function `ggpubr::ggarrange`.
#' 
#' @examples
#' library(survival.tzh); list(
#'  '`patchwork`' = subset_ggKM(os ~ ph.ecog, subset = list(All = TRUE, Female = (sex == 'Female')), 
#'     data = lung2)
#' ) |> render_(file = 'patchwork')
#' 
#' @export md_.patchwork
#' @export
md_.patchwork <- function(x, xnm, ncol = 2L, ...) {
  
  return(c(
    '```{r}',
    ((attr(x, which = 'fig.height', exact = TRUE) %||% 4) * ceiling(length(x) / ncol)) |> sprintf(fmt = '#| fig-height: %.1f'),
    ((attr(x, which = 'fig.width', exact = TRUE) %||% 7) * ncol) |> sprintf(fmt = '#| fig-width: %.1f'),
    sprintf(fmt = 'suppressWarnings(%s + patchwork::plot_layout(ncol = %d))', xnm, ncol), # not sure how to put in `...`
    '```',
    '<any-text>'
  ))
  
}


