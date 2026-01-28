get_veg <- function(r, landcover, lai, refldata, lctype = "ESA", 
                              python_env = "~/.virtualenvs/rgee") {
  
  # --- Python / rgee setup (function-local)
  reticulate::use_virtualenv(python_env, required = TRUE)
  
  # --- AOI from template raster
  e <- terra::ext(r)
  
  aoi <- ee$Geometry$Rectangle(c(e$xmin, e$ymin, e$xmax, e$ymax), 
                               proj = "EPSG:4326", geodesic = FALSE)
  
  # --- Canopy height image
  canopy_height_img <- ee$Image("users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1")
  
  # --- Download locally (NO Drive)
  vhgt <- ee_as_raster(image  = canopy_height_img, region = aoi, scale = 10, 
                       via = "getInfo")
  
  # --- Build vegetation parameters (unchanged logic)
  vegparameters <- create_vegpoint(landcover = landcover, vhgt = vhgt, lai = lai, 
                                   refldata = refldata, lctype = lctype)
  
  return(vegparameters)
}
