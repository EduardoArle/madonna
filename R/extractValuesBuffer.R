#' extractValuesBuffer
#'
#' Extracts values from raster layers for data points including a buffer.
#'
#' @importFrom bRacatus giveOcc occSpatialPoints
#' @importFrom raster extract stack
#' @importFrom rgdal spTransform CRS
#' @importFrom rgeos gBuffer
#' @importFrom sp proj4string
#' @param layer raster layer
#' @param legend table legend for the categorical values in the raster
#' @param occ table containing columns with the species name, longitude, and
#' latitude
#' @param location character, name of the column containing location IDs,
#' default is "species".
#' @param width numeric, width radius of the buffer in m.
#' @param lon character, name of the longitude column, default is "lon".
#' @param lat character, name of the latitude column, default is "lat".
#' @return This function returns the values for variables in each buffered area 
#' around the points.
#' @export
extractValues <- function(layer,legend,occ,location="location",width,
                          lon="lon",lat="lat") {
  
  t <- giveOcc(occ, location, lon, lat)
  t2 <- occSpatialPoints(t)
  t3 <- spTransform(t2, CRS(proj4string(layer)))
  t4 <- gBuffer(t3, width = width, byid = TRUE)
  
  values <- extract(layer,t4)
  
  LU <- as.data.frame(matrix(data = 0, nrow = nrow(occ), 
                             ncol = nrow(legend)))
  
  names(LU) <- legend$Land.Use
  
  for(i in 1:length(values))
  {
    areas <- table(values[[i]]) * 10000
    proportion <- areas/sum(areas) *100
    LU[i,which(as.character(legend$Value) %in% names(areas))] <- proportion
  }
  
  table <- cbind(occ,LU)
  return(table)
}
