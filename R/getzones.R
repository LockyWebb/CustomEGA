# Function 4 version 2 - getZones ####
# Get Zones
#
# Assigns the zones to each point in data based on the lines defined in coords

#' Calculate the Zones
#'
#' @param data dataframe, dataset of points
#' @param coords dataframe coordinate points of lines on custom error grid
#' @param area_labels vector of area labels for each zone
#' @param dir character direction, should points on line go to the inner or outer zone c("in","out")
#'
#' @importFrom magrittr "%>%"
#'
#' @return vector of zones
#' @export
#'
#' @examples
#' \dontshow{
#' datapoints <- data.frame(x = c(1, 2, 4, 6, 5, 5, 5, 3, 4, 9, 8, 8, 5, 3, 2, 2, 7, 8, 9, 9, 1, 2, 2, 1, 4, 6, 9, 3, 4,10, 0, 5, 9, 6,10,10, 0, 7, 0, 8, 5, 2, 4, 8),
#' y = c(9, 8, 8, 9, 7, 8, 5, 3, 6, 7, 4, 5, 2, 1, 2, 0, 1, 3, 1, 3,10, 7, 5, 4, 9, 4, 5, 7,10, 8, 5, 0,10, 0, 3, 6, 7,10, 0, 2,10, 6, 1, 6))
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
#' get_zones(data = datapoints, coords = coordinates, area_labels = c("C1","B1","A","B2","C2"))
get_zones <- function(data, coords, area_labels, dir = "in"){
  z<-NULL
  # names in data is x and y
  colnames(data) <- c("x","y")
  # names in coords is x, y, and z
  colnames(coords) <- c("x","y","z")

  # get the number of zones to iterate through/ essentially number of lines to apply function to
  num_areas = length(unique(coords$z)) # number of zones, including central zone, excluding top left zone

  # get size of zone output and assign everything to the top left zone
  zone = rep(area_labels[1],dim(data)[1])#rep(unique(coords$z)[1],dim(data)[1])

  # get min x
  minx = min(coords$x)

  for(i in 1:(num_areas)){     #for each line

    # get line information into a row for each segment
    line = coords %>% dplyr::filter(z == unique(coords$z)[i]) # filter by line ID
    line$x2 = dplyr::lead(line$x)
    line$y2 = dplyr::lead(line$y)
    line = line[1:(dim(line)[1]-1),]

    for(ii in 1:(dim(line)[1])){        # for each segment

      # if not a vertical line
      if(line$x[ii] != line$x2[ii]){
        if(dir == "in"){
          zone[change_log(data$x, data$y, line$x[ii], line$y[ii], line$x2[ii], line$y2[ii])] <- area_labels[i+1]#unique(coords$z)[i]
        }else if (dir == "out"){
          zone[change_log_out(data$x, data$y, line$x[ii], line$y[ii], line$x2[ii], line$y2[ii],minx)] <- area_labels[i+1]#unique(coords$z)[i]
        }else{
          stop("dir must be a charcter of 'in' or 'out' ")
        }


        # if a vertical line
      }else if(line$x[ii] == line$x2[ii]){
        if(dir == "in"){
          zone[change_log_ver(data$x, data$y, line$x[ii], line$y[ii], line$y2[ii])] <- area_labels[i+1]#unique(coords$z)[i]
        }else if (dir == "out"){
          zone[change_log_ver_out(data$x, data$y, line$x[ii], line$y[ii], line$y2[ii])] <- area_labels[i+1]#unique(coords$z)[i]
        }else{
          stop("dir must be a charcter of 'in' or 'out' ")
        }

      }

    }
    #if to the right of the last point
    zone[data$x > max(line$x2)] <-  area_labels[i+1]#unique(coords$z)[i]

  }
  # remove zone if NA
  zone[is.na(data$x) | is.na(data$y)] <- NA

  # remove upper or lower numbering
  #zone <- as.factor(substr(zone,1,1))

  # return zones
  return(zone)

}
