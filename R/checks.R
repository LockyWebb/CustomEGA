# Function 5  - check_lines ####
# Check lines hit graph edges and are in order

#' Check Lines
#'
#' @param coords coordinate points of lines on custom error grid
#'
#' @return Error message if lines are not entered correctly. Otherwise nothing.
#' @export
#'
#' @examples
#' \dontshow{
#'coordinates <-  data.frame(x = c(0,1,3,4,4,
#'0,2,2,5,5,7,
#'2,2,4,6,6,8,8,10,
#'6,8,8,10),
#'y = c(7,7,9,9,10,
#'      3,5,7,7,8,10,
#'      0,1,1,3,4,4,6,8,
#'      0,2,3,3),
#'line_id=c(1,1,1,1,1,
#'          2,2,2,2,2,2,
#'          3,3,3,3,3,3,3,3,
#'          4,4,4,4))
#' }
#' check_lines(coordinates)
check_lines <- function(coords){
  ularea<-NULL
  colnames(coords) <- c("x","y","z")

  ymin = min(coords$y, na.rm = TRUE)
  ymax = max(coords$y, na.rm = TRUE)
  xmin = min(coords$x, na.rm = TRUE)
  xmax = max(coords$x, na.rm = TRUE)

  areas <- data.frame(ularea = cbind(by(coords[-1,], coords$z[-1], upplftarea, ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax)))
  #areas <- data.frame(ularea = cbind(by(coords[-1,], coords$z[-1], function(x) upplftarea(x, ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax))))
  areas$line = rownames(areas)
  areas <- dplyr::arrange(areas, ularea)

  areas <- rbind(areas[is.na(areas$ularea),],tidyr::drop_na(areas))
  all(areas$line == as.character(unique(coords$z)))

  if(!all(areas$line == as.character(unique(coords$z)))){
    stop("Lines are not in order. ")
  }

  for(i in 1:length(unique(coords$z))){
    unique(coords$z)[i]

    # logic, for the ith line, is the x min and the y max any of the points
    xminymax_log = any(xmin %in% coords$x[coords$z == unique(coords$z)[i]]) & any(ymax %in% coords$y[coords$z == unique(coords$z)[i]])
    # logic, for the ith line, is the x max and the y min any of the points
    xmaxymin_log = any(xmax %in% coords$x[coords$z == unique(coords$z)[i]]) & any(ymin %in% coords$y[coords$z == unique(coords$z)[i]])

    #prevx = any(coords$x[coords$z == unique(coords$z)[i]] %in% coords$x[coords$z == unique(coords$z)[i-1]])
    #prevy = any(coords$y[coords$z == unique(coords$z)[i]] %in% coords$y[coords$z == unique(coords$z)[i-1]])


    # areas <- data.frame(ularea = cbind(by(coords[-1,], coords$z[-1], upplftarea, ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax)))
    # #areas <- data.frame(ularea = cbind(by(coords[-1,], coords$z[-1], function(x) upplftarea(x, ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax))))
    # areas$line = rownames(areas)
    # areas %<>% arrange(ularea)
    #
    # areas <- rbind(areas[is.na(areas$ularea),],drop_na(areas))
    # all(areas$line == as.character(unique(coords$z)))

    ## lagging
    xlag <- any((coords$x[coords$z == unique(coords$z)[i]] - dplyr::lag(coords$x[coords$z == unique(coords$z)[i]], default = 0))<0)
    ylag <- any((coords$y[coords$z == unique(coords$z)[i]] - dplyr::lag(coords$y[coords$z == unique(coords$z)[i]], default = 0))<0)


    #if(!(xminymax_log | xmaxymin_log) & !(prevx | prevy)){
    if(!(xminymax_log | xmaxymin_log)){

      stop("Some lines do not go to the edge. ")

    }

    # if(!all(areas$line == as.character(unique(coords$z)))){
    #   warning("lines are not in order")
    # }

    if(xlag | ylag){
      stop("Points are not in order in a line. ")
    }

  }

}


# Function 6 - check_points() ####
# Check points are in error grid area

#' Check Points
#'
#' @param data dataframe dataset of points
#' @param coords dataframe coordinate points of lines on custom error grid
#'
#' @return Error message if data is outside area defined by grid coordinates. Otherwise nothing.
#' @export
#'
#' @examples
#' \dontshow{
#'  datapoints <- data.frame(x = c(1, 2, 4, 6, 5, 5, 5, 3, 4, 9, 8, 8, 5, 3, 2, 2, 7, 8, 9, 9, 1, 2, 2, 1, 4, 6, 9, 3, 4,10, 0, 5, 9, 6,10,10, 0, 7, 0, 8, 5, 2, 4, 8),
#'y = c(9, 8, 8, 9, 7, 8, 5, 3, 6, 7, 4, 5, 2, 1, 2, 0, 1, 3, 1, 3,10, 7, 5, 4, 9, 4, 5, 7,10, 8, 5, 0,10, 0, 3, 6, 7,10, 0, 2,10, 6, 1, 6))
#'coordinates <-  data.frame(x = c(0,1,3,4,4,
#'0,2,2,5,5,7,
#'2,2,4,6,6,8,8,10,
#'6,8,8,10),
#'y = c(7,7,9,9,10,
#'      3,5,7,7,8,10,
#'      0,1,1,3,4,4,6,8,
#'      0,2,3,3),
#'line_id=c(1,1,1,1,1,
#'          2,2,2,2,2,2,
#'          3,3,3,3,3,3,3,3,
#'          4,4,4,4))
#' }
#' check_points(data = datapoints, coords = coordinates)
check_points <- function(data, coords){

  # names in data is x and y
  colnames(data) <- c("x","y")
  # names in coords is x, y, and z
  colnames(coords) <- c("x","y","z")

  ymin = min(coords$y, na.rm = TRUE)
  ymax = max(coords$y, na.rm = TRUE)
  xmin = min(coords$x, na.rm = TRUE)
  xmax = max(coords$x, na.rm = TRUE)

  if(any(data$x < xmin) | any(data$x > xmax) | any(data$y < ymin) | any(data$y > ymax)){
    stop("Data is outside the bounds of error grid coordinates. ")
  }

}


# Function 9 - check_line_cross ####
# check lines don't cross

#' Check Line Cross
#'
#' @param coords dataframe coordinate points of lines on custom error grid
#'
#' @return Error message if lines cross. Otherwise nothing.
#' @export
#'
#' @examples
#' \dontshow{
#'coordinates <-  data.frame(x = c(0,1,3,4,4,
#'0,2,2,5,5,7,
#'2,2,4,6,6,8,8,10,
#'6,8,8,10),
#'y = c(7,7,9,9,10,
#'      3,5,7,7,8,10,
#'      0,1,1,3,4,4,6,8,
#'      0,2,3,3),
#'line_id=c(1,1,1,1,1,
#'          2,2,2,2,2,2,
#'          3,3,3,3,3,3,3,3,
#'          4,4,4,4))
#' }
#' check_line_cross(coordinates)
check_line_cross <- function(coords){
  # name columns x, y and z
  colnames(coords) <- c("x","y","z")

  for(i in 2:length(unique(coords$z))){
    unique(coords$z)[i]

    dir_f = ifelse(coords$x[coords$z == unique(coords$z)[i]][1] < coords$y[coords$z == unique(coords$z)[i]][1], "in", "out")

    # get coordinates of previous lines
    prev_coords <- coords[coords$z %in% unique(coords$z)[1:(i-1)],]
    # make label for all previous zones. all zones to the upper left of the most recent line are bad.
    # the zone to the right of the most recent line (which should include the current line) is good
    prev_zones <- c(rep("bad",i-1),"good")

    # get the zone for the points on the current line
    zones = get_zones(coords[coords$z == unique(coords$z)[i], c("x","y")], prev_coords, prev_zones, dir = dir_f)

    # if any of the points on the line are in a previous zone, then it gives a warning
    if(any(zones == "bad")){
      stop("Check lines do not cross. Lines can overlap, but cannot cross.")
    }

  }

}
