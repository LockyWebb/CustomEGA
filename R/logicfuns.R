# Function 1 - change_log ####
# Change Logic
#
# The workflow for assigning zones starts with assigning
# everything to the top left zone. This function assesses
# whether a point should be assigned to a new zone.
# This code doe not work for a vertical line.
# It assesses the trapezoid made by a segment of a line.
# The line is defined by the points x1,y1 and x2,y2

#' Calculate change logic
#'
#' Calculate logic whether to change a data point's zone label. Points on line are given to inner zone
#'
#' @param x numeric, x coordinate of data point
#' @param y numeric, y coordinate of data point
#' @param x1 numeric, x coordinate of min point of zone boundary line
#' @param y1 numeric, y coordinate of min point of zone boundary line
#' @param x2 numeric, x coordinate of max point of zone boundary line
#' @param y2 numeric, y coordinate of max point of zone boundary line
#'
#' @return A logic value
#' @export
#'
#' @examples
#' ## Not Run
#' #change_log(data$x, data$y, line$x[ii], line$y[ii], line$x2[ii], line$y2[ii])
#' ## End Not Run
change_log = function(x,y,x1,y1,x2,y2){
  # x is x axis data, y is y axis data
  # x1, y1, x2, y2 define the two vertices of the segment
  # ul is upper/lower, whether or not the line is an upper line or a lower line
  # currently, ul = 1 for upper and 0 for lower
  ul = y1 > x1

  # points that sit on a line take the zone closest to the centre zone
  # upper lines take zone below the line, lower lines take zone above the line

  #log = x < x2 & x > x1 & y <= y1 + (y2 - y1) / (x2 - x1) * (x - x1)      # where should <=/< or >=/> be?
  # left edge & right edge & below slope

  if(ul == 1){
    log = x <= x2 & x >= x1 & y <= y1 + (y2 - y1) / (x2 - x1) * (x - x1)   # log = x <= x2 & x > x1 & y <= y1 + (y2 - y1) / (x2 - x1) * (x - x1)
  }else if(ul == 0){
    log = x <= x2 & x > x1 & y < y1 + (y2 - y1) / (x2 - x1) * (x - x1)    # log = x < x2 & x >= x1 & y < y1 + (y2 - y1) / (x2 - x1) * (x - x1)
  }else{
    stop("define upper or lower")
  }

  return(log)
}

# Function 1 (alternate) - change_log_out ####
# Change Logic
#
# The workflow for assigning zones starts with assigning
# everything to the top left zone. This function assesses
# whether a point should be assigned to a new zone.
# This code doe not work for a vertical line.
# It assesses the trapezoid made by a segment of a line.
# The line is defined by the points x1,y1 and x2,y2

#' Calculate change logic outer direction
#'
#' Calculate logic whether to change a data points zone label. Points on line are given to outter zone
#'
#' @param x numeric, x coordinate of data point
#' @param y numeric, y coordinate of data point
#' @param x1 numeric, x coordinate of min point of zone boundary line
#' @param y1 numeric, y coordinate of min point of zone boundary line
#' @param x2 numeric, x coordinate of max point of zone boundary line
#' @param y2 numeric, y coordinate of max point of zone boundary line
#' @param minx numeric, min x value of grid
#'
#' @return A logic value
#' @export
#'
#' @examples
#' ## Not Run
#' #change_log_out(data$x, data$y, line$x[ii], line$y[ii], line$x2[ii], line$y2[ii], minx)
#' ## End Not Run
change_log_out = function(x,y,x1,y1,x2,y2,minx){
  # x is x axis data, y is y axis data
  # x1, y1, x2, y2 define the two vertices of the segment
  # ul is upper/lower, whether or not the line is an upper line or a lower line
  # currently, ul = 1 for upper and 0 for lower
  ul = y1 > x1

  # points that sit on a line take the zone furthest to the centre zone
  # upper lines take zone above the line, lower lines take zone below the line

  #log = x < x2 & x > x1 & y <= y1 + (y2 - y1) / (x2 - x1) * (x - x1)      # where should <=/< or >=/> be?
  # left edge & right edge & below slope

  if(ul == 1){
    log = (x <= x2 & x > x1 & y < y1 + (y2 - y1) / (x2 - x1) * (x - x1)) | x == minx & x1 == minx & y < y1   # log = x <= x2 & x > x1 & y <= y1 + (y2 - y1) / (x2 - x1) * (x - x1)
  }else if(ul == 0){
    log = x <= x2 & x >= x1 & y <= y1 + (y2 - y1) / (x2 - x1) * (x - x1)    # log = x < x2 & x >= x1 & y < y1 + (y2 - y1) / (x2 - x1) * (x - x1)
  }else{
    stop("define upper or lower")
  }

  return(log)
}

# Function 2 - change_log_ver ####
# Change Logic Vertical
#
# If a line segment is vertical, this function assess
# if there are any points on the line.

#' Calculate change logic on vertical zone boundaries. Favouring inner direciton if points on line.
#'
#' @param x numeric, x coordinate of data point
#' @param y numeric, y coordinate of data point
#' @param x12 numeric, x coordinate of zone boundary line
#' @param y1 numeric, y coordinate of min point of zone boundary line
#' @param y2 numeric, y coordinate of max point of zone boundary line
#'
#' @return A logic value
#' @export
#'
#' @examples
#' ## Not Run
#' #change_log_ver(data$x, data$y, line$x[ii], line$y[ii], line$y2[ii])
#' ## End Not Run
change_log_ver = function(x,y,x12,y1,y2){
  # x is x axis data, y is y axis data
  # x12 is the x1 and x2 values, which have to b equal, otherwise this function shouldn't be called
  # y1, y2 define the two y values at the ends of the segment
  # ul is upper/lower, whether or not the line is an upper line or a lower line
  # currently, ul = 1 for upper and 0 for lower
  ul = y1 > x12

  # points that sit on a line take the zone closest to the centre zone
  # upper lines take zone below the line, lower lines take zone above the line

  #log = x < x2 & x > x1 & y <= y1 + (y2 - y1) / (x2 - x1) * (x - x1)      # where should <=/< or >=/> be?
  # left edge & right edge & below slope

  if(ul == 1){
    log = x == x12 & y > y1 & y <= y2 # i think you only need to do it for the upper lines
  }else if(ul == 0){
    #log = x == x12 & y <= y1 + (y2 - y1) / (x2 - x1) * (x - x1)
    log = FALSE#!(x==x)
  }else{
    stop("define upper or lower")
  }

  return(log)
}

# Function 2 (alternate) - change_log_ver_out ####
# Change Logic Vertical
#
# If a line segment is vertical, this function assess
# if there are any points on the line.

#' Calculate change logic on vertical zone boundaries outer direction
#'
#' Calculate change logic on vertical zone boundaries. Favouring outer direciton if points on line
#'
#' @param x numeric, x coordinate of data point
#' @param y numeric, y coordinate of data point
#' @param x12 numeric, x coordinate of zone boundary line
#' @param y1 numeric, y coordinate of min point of zone boundary line
#' @param y2 numeric, y coordinate of max point of zone boundary line
#'
#' @return A logic value
#' @export
#'
#' @examples
#' ## Not Run
#' #change_log_ver_out(data$x, data$y, line$x[ii], line$y[ii], line$y2[ii])
#' ## ENd Not Run
change_log_ver_out = function(x,y,x12,y1,y2){
  # x is x axis data, y is y axis data
  # x12 is the x1 and x2 values, which have to b equal, otherwise this function shouldn't be called
  # y1, y2 define the two y values at the ends of the segment
  # ul is upper/lower, whether or not the line is an upper line or a lower line
  # currently, ul = 1 for upper and 0 for lower
  ul = y1 > x12

  # points that sit on a line take the zone closest to the centre zone
  # upper lines take zone below the line, lower lines take zone above the line

  #log = x < x2 & x > x1 & y <= y1 + (y2 - y1) / (x2 - x1) * (x - x1)      # where should <=/< or >=/> be?
  # left edge & right edge & below slope

  if(ul == 1){
    log = FALSE#x == x12 & y >= y1 & y < y2 # i think you only need to do it for the upper lines
  }else if(ul == 0){
    log = x == x12 & y > y1 & y <= y2
    #log = !(x==x)
  }else{
    stop("define upper or lower")
  }

  return(log)
}
