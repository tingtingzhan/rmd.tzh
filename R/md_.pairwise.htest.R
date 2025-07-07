

#' @rdname md_
#' 
#' @examples
#' list(
#'   '`pairwise.htest`' = airquality |> 
#'     within.data.frame(expr = {
#'       Month = factor(Month, labels = month.abb[5:9])
#'     }) |>
#'     with(expr = pairwise.t.test(Ozone, Month, pool.sd = FALSE, p.adj = 'none'))
#' ) |> render_(file = 'pairwise.htest')
#' 
#' @export md_.pairwise.htest
#' @export
md_.pairwise.htest <- function(x, xnm, ...) c(
  '```{r}', # multiple ?flextable::flextable
  sprintf(fmt = '(%s) |> as_flextable.pairwise.htest()', xnm), 
  sprintf(fmt = '(%s) |> p_adjust_.pairwise.htest() |> label_pvalue_sym()() |> as_flextable.matrix()', xnm), 
  '```', 
  '<any-text>',
  '\n\n'
)

