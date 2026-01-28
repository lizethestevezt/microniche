get_landcover <- function(site, out_dir, type = "ESA", year = 2018, overwrite = FALSE) {
  reticulate::use_virtualenv("~/.virtualenvs/rgee", required = TRUE)
  
  reticulate::py_config()
  
  outfile <- file.path(out_dir, paste0(site$site_id, "_landcover_"), type, ".tif")
  if (file.exists(outfile) && !overwrite) {
    message("Landcover already exists for site ", site$site_id)
    return(outfile)
  }
  
  r <- site$SpatRaster
  e <- terra::ext(r)
  
  aoi <- ee$Geometry$Rectangle(
    c(e$xmin, e$ymin, e$xmax, e$ymax),
    proj = "EPSG:4326",
    geodesic = FALSE
  )
  
  if (type == "ESA") {
    img <- ee$ImageCollection("ESA/WorldCover/v100")$first()
    scale <- 10
  } else if (type == "CORINE") {
    img <- ee$Image(paste0("COPERNICUS/CORINE/V20/100m/", year))$
      select("landcover")
    scale <- 100
  } else {
    stop("type must be 'ESA' or 'CORINE'")
  }
  
  # ---- local download (NO DRIVE)
  lc_rast <- ee_as_raster(
    image  = img,
    region = aoi,
    scale  = scale,
    crs    = "EPSG:4326",
    via    = "getInfo"
  )
  
  terra::writeRaster(lc_rast, outfile, overwrite = TRUE)
  
  return(outfile)
}