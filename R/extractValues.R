#' extractValues
#'
#' Extracts bioclimatic data from WorldClim layers for data points.
#'
#' @importFrom bRacatus giveOcc
#' @importFrom bRacatus occSpatialPoints
#' @importFrom raster extract
#' @importFrom raster stack
#' @param path character, the path to the folder containing the variables
#' @param occ table containing columns with the species name, longitude, and
#' latitude
#' @param specie character, name of the column containing species names,
#' default is "species".
#' @param lon character, name of the longitude column, default is "lon".
#' @param lat character, name of the latitude column, default is "lat".
#' @return This function returns the values for variables in each point.
#' @export
extractValues <- function(path,occ,species="species",
                          lon="lon",lat="lat") {
  
  t <- giveOcc(occ,"species","lon","lat")
  t2 <- occSpatialPoints(t)
  
  setwd(path)
  vars <- stack(list.files(pattern = ".bil"))
  
  values <- extract(variables,t2)
  
  return(values)
}