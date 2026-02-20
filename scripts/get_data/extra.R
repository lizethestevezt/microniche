
bbox <- function (r) # to get the bounding box of a SpatRaster
{
  e <- ext(r)
  xy <- data.frame(x = c(e$xmin, e$xmin, e$xmax, e$xmax), 
                   y = c(e$ymin, e$ymax, e$ymin, e$ymax))
  xy <- sf::st_as_sf(xy, coords = c("x", "y"), crs = crs(r))
  ll <- sf::st_transform(xy, 4326)
  ll <- data.frame(sf::st_coordinates(ll))
  out <- c(min(ll$X), min(ll$Y), max(ll$X), max(ll$Y))
  return(out)
}