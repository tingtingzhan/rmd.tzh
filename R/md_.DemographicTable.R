

#' @title R Markdown Lines for \link[DemographicTable]{DemographicTable}
#' 
#' @param x,xnm,... ..
#' 
#' @param font.size \link[base]{numeric} scalar
#' 
#' @note
#' Do not want to `Imports: rmd.tzh` in package \CRANpkg{DemographicTable}.
#' 
#' @examples
#' library(DemographicTable); list(
#'   '`DemographicTable`' = DemographicTable(CO2, groups = 'Type', include = c('conc', 'uptake'))
#' ) |> render_(file = 'DemographicTable')
#' @export md_.DemographicTable
#' @export
md_.DemographicTable <- function(x, xnm, font.size = 9, ...) {
  
  z1 <- 'Descriptive statistics, e.g., means, medians, standard deviations, inter-quartile ranges (IQR) and percentages, are provided using <u>**`R`**</u>.' |>
    new(Class = 'md_lines')

  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    font.size |> sprintf(fmt = 'set_flextable_defaults(font.size = %.1f)'),
    xnm |> sprintf(fmt = 'as_flextable(%s)'),
    'init_flextable_defaults()',
    '```'
  ) |>
    new(Class = 'md_lines')

  c(z1, z2) # [c.md_lines()]
  
}





