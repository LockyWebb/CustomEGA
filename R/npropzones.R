# Function 8 part 2
# get proportion of zones

#' Summarise Freq and Prop Zones
#'
#' @param zone vector calcualted zones
#' @param group NULL, ignore
#' @param rounding integer, the number of decimal places to round to
#'
#' @return list frequency and percent of each zone in the dataset
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
#' zone <- zones(data = datapoints, coords = coordinates, area_labels = c("C","B","A","B","C"))
#' nprop.zones(zone,rounding=2)
nprop.zones<-function(zone,group=NULL,rounding=2){
  nproplist<-list(table(zone),round(prop.table(table(zone))*100,rounding))
  names(nproplist)[1:2]<-c("Frequency","Percent")
  return(nproplist)
}

