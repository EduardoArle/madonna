#' plotPoints
#' 
#' Plot occurrence points with background for visualisation
#'
#' @importFrom graphics points
#' @importFrom raster extent plot
#' @importFrom rgeos gIntersection gBuffer
#' @importFrom rworldmap getMap
#' @importFrom sp over proj4string
#' @importFrom bRacatus giveOcc occSpatialPoints
#' @param occ table containing columns with the species name, longitude, and
#' latitude
#' @param specie character, name of the column containing species names,
#' default is "species".
#' @param lon character, name of the longitude column, default is "lon".
#' @param lat character, name of the latitude column, default is "lat".
#' @param colours character, options are "bicha" or "standard". Default is
#' "bicha"
#' @return This function downloads all 19 climatic variables and returns them
#' in a raster stack.
#' @examples
#' worldclim <- getWorldClim(res = 0.5,country = "Czech Republic",
#' path = getwd())
#' @export
plotPoints <- function(occ,species="species",lon="lon",lat="lat",
                       colours = "bicha") {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  occ2 <- giveOcc(occ,"species","lon","lat")
  world <- getMap(resolution = "low")
  world <- suppressWarnings(gBuffer(world, byid = TRUE, width = 0))
  occ2 <- giveOcc(occ,"species","lon","lat")
  occ_sp <- occSpatialPoints(occ2)
  countries <- unique(over(occ_sp,world)$NAME)
  countries <- world[world$NAME %in% countries,]
  CP <- as(extent(countries), "SpatialPolygons")
  sp::proj4string(CP) <- CRS(proj4string(world))
  map <- suppressWarnings(gIntersection(world,
                                        CP,
                                        byid = TRUE, 
                                        checkValidity = 2))
  par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  if(colours != "bicha"){
    plot(map, col = "khaki", bg = "azure2",
         main = unique(occ_sp$species), font.main = 3)
    points(occ_sp, pch = 21, cex = 1, bg = "red")
  }else{
    plot(map, col = "magenta", bg = "white",
         main = unique(occ_sp$species), font.main = 3)
    points(occ_sp, pch = 21, cex = 1, bg = "cyan")
  }
}