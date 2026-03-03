site_level_inputs <- function(site_row, credentials) {
  # site_row is one row from get_sites()
  
  # 1) Time vector for this site
  tme <- seq(from = site_row$tme_start, to = site_row$tme_end, by = "1 day")
  tme <- as.POSIXlt(tme, tz = "UTC")
  
  # 2) Template raster around site (for GEE / LAI, etc.)
  template_rast <- terra::rast(extent = terra::ext(site_row$lon_min, site_row$lon_max, site_row$lat_min, site_row$lat_max),resolution = 0.0001,
                               crs = "EPSG:4326")
  
  # 3) ERA5 → .nc → climdata
  nc_path <- build_era5(site = site_row)   # your wrapper
  climdata <- extract_clim( nc = nc_path, long = mean(c(site_row$lon_min, site_row$lon_max)), lat = mean(c(site_row$lat_min, site_row$lat_max)),
                            start_time = site_row$tme_start, end_time = site_row$tme_end, format = "micropoint")
  
  # 4) Land cover
  landcover <- get_landcover(site_row, out_dir = ESA_DIR, type = "ESA")
  
  # 5) LAI
  lai_info <- get_lai(r = template_rast, tme = tme, pathout = LAI_DIR, credentials = credentials)
  # here you’ll add code to turn lai_info$path into a terra::rast stack
  # e.g. lai <- rast(list_of_lai_files)
  lai <- NULL  # placeholder
  
  # 6) Albedo
  alb <- get_albedo(template_rast = template_rast, tme = tme, ALB_DIR = ALB_DIR, mycredentials = credentials)
  
  # 7) Reference data + vegparameters
  refldata     <- get_refldata(landcover = landcover, alb = alb, lai = lai)
  vegparameters <- get_veg( r = template_rast, landcover = landcover, lai = lai, refldata = refldata)
  
  # 8) Ground parameters
  groundparameters <- get_groundparams( r = template_rast, landcover = landcover, pathdir = SOIL_DIR, deletefiles = FALSE)
  
  site_info <- list(tme = tme, template_rast = template_rast, climdata = climdata, landcover = landcover, lai = lai, alb = alb, refldata = refldata,
       vegparameters = vegparameters, groundparameters = groundparameters)
  return(site_info)
}
