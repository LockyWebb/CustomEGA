# Function 3 - upplftarea ####
# Upper Left Area
#
# Determine the size of the area on the upper left of each line.
# Adds up the upper trapezoids defined by each segment.

#' Upper Left Area
#'
#' Upper Left Area of each zone boundary
#'
#' @param df dataframe with variables x and y,
#' @param ymin numeric, minimum y of grid
#' @param ymax numeric, maximum y of grid
#' @param xmin numeric, minimum x of grid
#' @param xmax numeric, maximum x of grid
#'
#' @importFrom magrittr "%<>%"
#' @importFrom dplyr "arrange"
#'
#' @return numeric area above zone boundary
#' @export
#'
#' @examples
#' \dontrun{
#' upplftarea(df,ymin,ymax,xmin,xmax)
#' }
upplftarea <- function(df,ymin,ymax,xmin,xmax){
  x<-NULL
  y<-NULL
  df %<>% arrange(x,y)
  sum <- 0
  for(hh in 1:(dim(df)[1]-1)){
    sum = sum + (df$x[hh]-xmin + df$x[hh+1]-xmin)/2*(df$y[hh+1]-df$y[hh])
  }
  sum = sum + (ymax - max(df$y)) * (xmax - xmin)
  return(sum)
}
