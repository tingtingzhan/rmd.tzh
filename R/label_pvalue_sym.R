

#' @title Label \eqn{p}-values with Significance Symbol
#' 
#' @description
#' Label \eqn{p}-values with significance symbol.
#' 
#' @param ... parameters of function \link[scales]{label_pvalue}
#' 
#' @details
#' 
#' Pipeline:
#' 
#' Step 1: \link[scales]{label_pvalue}.
#' 
#' Step 2: Drop leading zeros.
#' 
#' Step 3: \link[stats]{symnum} (also see function \link[stats]{printCoefmat}).
#' 
#' @note
#' Function \link[scales]{label_pvalue} is much prettier and more flexible than function \link[base]{format.pval}.
#' 
#' Function [label_pvalue_sym()] is used so extensively, 
#' therefore tzh keeps it in \pkg{rmd.tzh} instead of \pkg{scales.tzh}
#' 
#' @returns 
#' Function [label_pvalue_sym()] returns a \link[base]{function}.
#' 
#' @examples 
#' p1 = p2 = c(pi^-100, .02, .05, .1, .9999, NA_real_)
#' 
#' dim(p1) = c(2, 3); p1
#' p1 |> label_pvalue_sym()() # attr-dim kept
#' p1 |> label_pvalue_sym(add_p = TRUE)()
#' 
#' names(p2) = letters[1:6]; p2
#' p2 |> label_pvalue_sym()() # attr-name kept
#' 
#' # below: exception handling
#' double() |> scales::label_pvalue()() 
#' # do not like!
#' # ?scales::pvalue is bad; ?scales::number is fine!
#' 
#' double() |> label_pvalue_sym()()
#' @keywords internal
#' @importFrom scales label_pvalue
#' @importFrom stats symnum
#' @export
label_pvalue_sym <- function(...) {
  
  function(x) {
    
    ret <- x
    storage.mode(ret) <- 'character'
    
    if (!length(x)) return(ret)
    
    ret[] <- x |> 
      label_pvalue(...)() |>
      sub(pattern = '([-]?)0[.]', replacement = '\\1.') # http://stackoverflow.com/questions/12643391
    
    sym <- symnum(
      x, corr = FALSE, na = FALSE, 
      cutpoints = c(0, .001, .01, .05, .1, 1), 
      symbols = if (getOption('bitmapType') %in% c('cairo')) {
        #c('\u2605\u2605\u2605', '\u2605\u2605', '\u2605', '\u2606', '') # star
        c('\u2b51\u2b51\u2b51', '\u2b51\u2b51', '\u2b51', '\u2b52', '') # small star
      } else c("***", "**", "*", ".", " ")
    ) # see ?stats::printCoefmat
    ret[] <- ret |> paste(sym) |> trimws()
    
    ret[is.na(x)] <- '' # *not* NA_character_
    
    return(ret)
      
  }

}


