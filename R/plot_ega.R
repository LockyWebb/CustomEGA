# Function 7 - plot_EGA ####
# plot points on grid in consistent way

#' Plot data in an Error Grid Analysis
#'
#' @param data dataframe, data to plot in zones
#' @param coords dataframe, coordinates of lines to make up the Error Grid Analysis (required)
#' @param x_lab character, label for x axis
#' @param y_lab character, label for y axis
#' @param x vector, x coordinate of data points to plot
#' @param y vector, x coordinate of data points to plot
#' @param zones vector, zones that each data point is in
#' @param area_labels vector, label for each zone
#' @param check_zone TRUE or FALSE, whether to check the zones have been calculated correctly
#'
#' @import ggplot2
#'
#' @return a ggplot object
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
#' # zones not yet calculated
#' plot_EGA(data = datapoints, coords = coordinates,
#'          x_lab = "my gold standard", y_lab = "my new measure",
#'          area_labels = c("C1","B1","A","B2","C2"))
#' # zones calculated (data is then 3 columns - x,y,zone)
#' datapoints$zone <- zones(data = datapoints, coords = coordinates,
#'                          area_labels = c("C1","B1","A","B2","C2"))
#' plot_EGA(data = datapoints, coords = coordinates,
#'          x_lab = "my gold standard", y_lab = "my new measure")

plot_EGA <- function(data = NULL, coords, x_lab = "x", y_lab = "y", x = NULL, y = NULL, zones = NULL, area_labels = NULL, check_zone = FALSE){
  z<-NULL
  if(is.null(data) & !(is.null(x) | is.null(y) | is.null(zones))){
    data = cbind(as.data.frame(x), as.data.frame(y), as.data.frame(zones))
  }
  # names in data is x and y
  colnames(data)[1:2] <- c("x","y")
  # names in coords is x, y, and z
  colnames(coords) <- c("x","y","z")

  if(dim(data)[2] == 2 & !is.null(area_labels)){  # hasn't got zones yet
    data$zones = get_zones(data = data, coords = coords, area_labels = area_labels)
  }else if(dim(data)[2] == 3 & check_zone == FALSE){ # has got zones
    colnames(data)[3] <- "zones"
  }else if(dim(data)[2] == 3 & check_zone == TRUE){
    if(is.null(area_labels)){
      stop("If you want to check the zones, you need to supply the zone labels that macth the supplied line coordinates. ")
    }
    zone_check <- get_zones(data[,c("x","y")], coords, area_labels)
    if(any(!(zones))){
      stop("Some zones appear to not be correct. Check your zone calculation, or calculate them again. ")
    }
    ## check functions add here ##
  }else{
    stop("Check data has been provided properly. If zone not yet calculated, provide area labels. Data should be: column 1 - x, column 2 - y, column 3 - zone. ")
  }

  EGApallete = c("#0072B2", "#CC79A7","#009E73","#ed8d1f","#32edd4","#ff195b","#33FF33")[1:length(unique(data$zones))]

  plot = ggplot() +
    geom_line(data=coords,aes(x, y,group=z)) +
    geom_point(data=data,aes(x=x,y=y,colour=factor(zones)),size=1) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    xlab(x_lab) + ylab(y_lab) + labs(colour="Zones") +
    scale_colour_manual(values=EGApallete) +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          plot.margin = unit(c(0.3,0.5,0.3,0.3),"cm")) #top, right, bottom, left)

  return(plot)

}
