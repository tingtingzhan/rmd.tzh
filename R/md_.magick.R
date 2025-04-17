


#' @rdname md_
#' 
#' @examples
#' library(magick); list(
#'  '`magick-image`' = 'https://jeroen.github.io/images/frink.png' |> image_read()
#' ) |> render_(file = 'magick')
#' 
#' @export `md_.magick-image`
#' @export
`md_.magick-image` <- function(x, xnm, ...) {
  return(c(
    '```{r}',
    sprintf(fmt = '%s |> print(info = FALSE)', xnm), # ?magick:::`print.magick-image`
    '```', 
    '<any-text>'
  ))
  # ?magick::image_write; write to a file
  # ?magick::image_info; height and width
}
