#' getWorldClim
#' 
#' Downloads bioclimatic data from WorldClim
#'
#' @importFrom raster getData
#' @importFrom rworldmap getMap
#' @param res numeric, resolution of the data. Possible values are 10, 5, 2.5,
#' or 0.5 arc minutes.
#' @param country character, country name.
#' @param path character, the path to the folder where the variables shall
#' be saved
#' @return This function downloads all 19 climatic variables and returns them
#' in a raster stack.
#' @examples
#' worldclim <- getWorldClim(res = 0.5,country = "Czech Republic",
#' path = getwd())
#' @export
getWorldClim <- function(res,country,path) {
  
  world <- getMap(resolution = "coarse")
  region <- world[which(world$SOVEREIGNT == country),]
  
  worldclim <- getData("worldclim", var = "bio", res = res,
                       lon = region$LON, lat = region$LAT,
                       path = path)
  return(worldclim)
}