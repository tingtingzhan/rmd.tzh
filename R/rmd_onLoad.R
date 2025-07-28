

# ?ggplot2::theme_minimal

#' @importFrom ggplot2 theme_set theme_bw theme
#' @importFrom ggplot2 element_blank element_rect
#' @importFrom ggplot2 %+replace%
#' @importFrom grid unit
.onLoad <- function(libname, pkgname) {
  
  Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
  # https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time
  # to disable "checking for future file timestamps" in R Check
  
  #options(
    #bitmapType = 'cairo'
    # default was 'quartz'
    # do NOT need to generate html file!!
  #)

  theme_set(
    theme_bw(
      base_size = 11, 
      #base_family = 'Arial Unicode MS', # seems not needed!!!
      base_line_size = 11/22, 
      base_rect_size = 11/22
    ) %+replace% 
      theme(
        axis.ticks = element_blank(), 
        #legend.background = element_blank(), 
        legend.background = element_rect(color = 'grey95'),
        #legend.key.spacing.y = unit(.015, units = 'npc'),
        legend.key.spacing.y = unit(.02, units = 'npc'),
        legend.key = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank(), 
        complete = TRUE
      )
  )
  
}


# theme(
#  legend.position = 'inside',
#  legend.position.inside = c(.8, .65),
#  legend.background = element_rect(color = 'grey95'),
#  legend.key.spacing.y = grid::unit(.05, units = 'npc')
# )
# 


