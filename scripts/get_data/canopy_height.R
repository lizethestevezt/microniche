# ---- helper
.assert_lonlat <- function(lon, lat) {
  if (!is.numeric(lon) || !is.numeric(lat)) {
    stop("lon and lat must be numeric")
  }
  if (abs(lat) > 90 && abs(lon) <= 90) {
    stop("Coordinates look swapped: lat > 90 but lon <= 90")
  }
  if (abs(lon) > 180 || abs(lat) > 90) {
    stop("Invalid lon/lat values")
  }
}
# ---- extract height (using Earth Engine)
get_canopy_height <- function(lon, lat) {
  
  .assert_lonlat(lon, lat)
  
  canopy_img <- ee$Image("users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1")$select("b1")
  
  pt <- ee$Geometry$Point(list(lon, lat))  # explicit list, not c()
  
  val <- canopy_img$sample(region = pt, scale = 10, geometries = FALSE)$first()
  
  if (is.null(val)) return(NA_real_)
  
  h <- val$get("b1")$getInfo()
  
  if (length(h) == 0 || is.null(h) || is.na(h)) {
    return(NA_real_)
  }
  
  as.numeric(h)
}