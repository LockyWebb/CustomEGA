# Function 8
# descriptives of zones

#' Summarise Zones
#'
#' Summarises the zones calculated for a data set. Can either have zones provided
#' or calculate them for you before summarising.
#'
#' @param data dataframe, dataset of points
#' @param coords dataframe of coordinates of points making up lines of error grid
#' @param group Null, currently unused
#' @param rounding integer, the number of decimal places to round to
#' @param area_labels vector of area labels for each zone, only given if zones need to be calculated
#'
#' @return List frequency and percent of each zone in the dataset
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
#' desc_EGA(data = datapoints,
#'          coords = coordinates,
#'          area_labels = c("C","B","A","B","C"))
#' desc_EGA(data = datapoints,
#'          coords = coordinates,
#'          area_labels = c("C1","B1","A","B2","C2"))
#'
#' datapoints$zone <- zones(data = datapoints,
#'                          coords = coordinates,
#'                          area_labels = c("C","B","A","B","C"))
#' desc_EGA(datapoints)
#'
#' desc_EGA(datapoints$zone)
desc_EGA <- function(data, coords = NULL, group=NULL, rounding=2, area_labels = NULL){    #x_lab = "x", y_lab = "y"




  # names in coords is x, y, and z
  if(!is.null(coords)){
    colnames(coords) <- c("x","y","z")
  }

  if(is.vector(data)){
    data = data.frame(zone = data)
  }else if(dim(data)[2] == 2 & !is.null(area_labels)& !is.null(coords)){  # hasn't got zones yet
    # names in data is x and y
    colnames(data)[1:2] <- c("x","y")
    data$zone = zones(data = data, coords = coords, area_labels = area_labels)
  }else if(dim(data)[2] == 3){ # has got zones
    colnames(data)[3] <- "zone"
  }else{
    stop("Check data has been provided properly. If zone not yet calculated, provide area labels. Data should be either: 1 column (zone only), 2 columns (zone not calculated, column 1 - x, column 2 - y), or 3 columns (column 1 - x, column 2 - ycolumn 3 - zone. ")
  }

  nprop <- nprop.zones(data$zone,group=group,rounding=rounding)
  return(nprop)

}
