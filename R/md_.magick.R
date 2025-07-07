


#' @rdname md_
#' @examples
#' list(
#'  '`magick-image`' = magick::wizard
#' ) |> render_(file = 'magick')
#' 
#' @export `md_.magick-image`
#' @export
`md_.magick-image` <- function(x, xnm, ...) {
  return(c(
    '```{r}',
    sprintf(fmt = '%s |> print(info = FALSE)', xnm), # ?magick:::`print.magick-image`
    '```', 
    '<any-text>',
    '\n\n'
  ))
  # ?magick::image_write; write to a file
  # ?magick::image_info; height and width
}
