

#' @title R Markdown Lines for \link[DemographicTable]{DemographicTable}
#' 
#' @param x a \link[DemographicTable]{DemographicTable}
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @note
#' Do not want to `Imports: rmd.tzh` in package \CRANpkg{DemographicTable}.
#' 
#' @examples
#' library(DemographicTable); list(
#'   '`DemographicTable`' = DemographicTable(CO2, groups = 'Type', include = c('conc', 'uptake'))
#' ) |> render_(file = 'DemographicTable')
#' @keywords internal
#' @export md_.DemographicTable
#' @export
md_.DemographicTable <- function(x, xnm, ...) {
  
  z1 <- 'Descriptive statistics, e.g., means, medians, standard deviations, inter-quartile ranges (IQR) and percentages, are provided using <u>**`R`**</u> package <u>**`DemographicTable`**</u>.' |>
    new(Class = 'md_lines', package = 'DemographicTable')
  # need to 
  # @importClassesFrom rmd.tzh md_lines
  # if to move to package \CRANpkg{DemographicTable}
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    xnm |> sprintf(fmt = 'as_flextable.DemographicTable(%s)'),
    '```'
  ) |>
    new(Class = 'md_lines')

  c(z1, z2) # [c.md_lines()]
  
}





