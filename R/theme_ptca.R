#_____________________________________________________________________
# function theme_csvd2()
# Inspired from function theme_ptca()
# Hervé Abdi from original from Vincent Guillemot
# April 11, 2018,  May 7, 2018.
# September 2, 2019. HA
# Last version is September 10. HA.
# needs improvements
# A good place to pick up colors for theme_ptca() and alternative is
# https://www.w3schools.com/colors/colors_picker.asp
#_____________________________________________________________________
# Preambule theme_ptca ----
#' @title  A \code{ggplot2} theme that matches the defaults
#' of the factorial figures in the package \code{PTCA4CATA}
#'
#' @description \code{theme_ptca2}:
#' A \code{ggplot2} theme that matches the defaults
#' of the factorial figures in \code{PTCA4CATA}.
#' \code{theme_ptca2} is obviously a (slightly smarter) 
#' cousin of 
#' \code{PTCA4CATA::theme_ptca} (which will be deprecated in the
#' new future).
#'
#' @param mire (Default = \code{TRUE}) when \code{TRUE}
#' plot the factorial axis at values \code{x0} and \code{y0}.
#' @param x0 (Default = 0) the X-axis origin. Relevant only
#' when \code{mire = TRUE}.
#' @param y0 (Default = 0) the Y-axis origin.
#' Relevant only
#' when \code{mire = TRUE}.
#' @param coor.fixed (Default = \code{TRUE}), when \code{TRUE}
#'  the X and Y axis have the same units, when \code{FALSE} 
#' the units are set by \code{ggplot2}, if a number is provided
#' it will give the ratio (i.e., Y/X) to be used.
#' @param base_family the font family for the text 
#' (e.g., on a mac \code{'Times', 'Arial', 'Courier', 'Symbol'},
#' or plain \code{R, 'sans', 'serif', 'mono'}, can also be other fonts 
#' if installed). Default is "" (i.e., uses the current default).
#' @author Vincent Guillemot & Hervé Abdi
#' @import ggplot2
#' @importFrom grDevices adjustcolor
#' @examples
#'\dontrun{
#' x <- c(5, 8, 1, 70)
#' y <- c(10, 12, 20, 50)
#' ( # parentheses are for print
#'  p <-  ggplot(as.data.frame(cbind(x, y)), aes(x = x, y = y)) +
#'            geom_point(size = 2, shape = 19) +
#'            theme_ptca2() 
#'            )
#' }
#' @export
theme_ptca2 <- function(mire = TRUE, x0 = 0, y0 = 0, 
                       coor.fixed = TRUE, 
                       base_family = "") {
  dark.col  <- '#26004d'  # a darker version of purple
  col.fill  <-  grDevices::adjustcolor('lavender', alpha.f = .2)
  col.facet <- 'lavender'
  col.bkg   <- 'darkorchid'
  col.axes  <-  grDevices::adjustcolor('darkorchid', alpha.f = .2)
  width.axes <- 1.1
  theme_list <- list(
      theme_grey(base_family = base_family) + #  %+replace%
        theme(
        text = element_text(colour = dark.col, face = 'bold'), # HA
        legend.key   = element_rect(fill = NA, color = NA),
        legend.title = element_text(color = dark.col),
        legend.text  = element_text(color = dark.col, face = 'bold'),
        axis.text    = element_text(color = dark.col),
        axis.ticks   = element_line(color = dark.col),
        axis.title   = element_text(color = dark.col),
        panel.background = element_rect(color = col.bkg,
                                        fill = col.fill),
        strip.background = element_rect(fill = col.facet),
        strip.text = element_text(colour = dark.col, face = 'italic')
      ))
  if (mire) theme_list <- append(theme_list, list(
    geom_vline(xintercept = x0, color = col.axes, size = width.axes),
    geom_hline(yintercept = y0, color = col.axes, size = width.axes)))
  if(isTRUE(coor.fixed)){
  theme_list <- append(theme_list, coord_fixed())
  }
  if (is.double(coor.fixed)){
    theme_list <- append(theme_list, coord_fixed(coor.fixed))
      # if 1: will force the graph to be a square
  }
  return(theme_list)
}
# end of theme_ptca() ----
# ____________________________________________________________________
