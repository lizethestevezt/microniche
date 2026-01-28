get_groundparams <- function( r, landcover, water = 80, pathdir = tempdir(), 
                              deletefiles = TRUE, python_env = "~/.virtualenvs/rgee") {
  
  # --- Python env (kept for consistency with pipeline)
  reticulate::use_virtualenv(python_env, required = TRUE)
  
  # --- ensure output dir exists
  dir.create(pathdir, recursive = TRUE, showWarnings = FALSE)
  
  # --- extent + CRS
  e <- terra::ext(r)
  
  epsg_code <- terra::crs(r, describe = TRUE)$code
  if (is.null(epsg_code) || is.na(epsg_code)) {
    stop("Could not extract EPSG code from r")
  }
  
  subsetx <- paste0("&SUBSET=X(", e$xmin, ",", e$xmax, ")")
  subsety <- paste0("&SUBSET=Y(", e$ymin, ",", e$ymax, ")")
  
  subsetcrs <- paste0(
    "&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/", epsg_code
  )
  outcrs <- paste0(
    "&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/", epsg_code
  )
  
  # --- SoilGrids parameters
  vars   <- c("bdod", "clay", "sand", "silt")
  dps_mn <- c(0, 5, 15, 30, 60, 100)
  dps_mx <- c(5, 15, 30, 60, 100, 200)
  
  soildata <- NULL
  
  for (ii in seq_along(vars)) {
    
    # ---- download depths ----
    for (jj in 1:6) {
      
      base_url <- paste0(
        "https://maps.isric.org/mapserv?map=/map/",
        vars[ii],
        ".map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage"
      )
      
      idform <- paste0(
        "&COVERAGEID=",
        vars[ii], "_", dps_mn[jj], "-", dps_mx[jj],
        "cm_Q0.5&FORMAT=GEOTIFF_INT16"
      )
      
      request_url <- paste0(
        base_url, idform, subsetx, subsety, subsetcrs, outcrs
      )
      
      resp <- httr::GET(request_url)
      if (resp$status_code != 200) {
        stop(
          "Bad query request for ",
          vars[ii], " ", dps_mn[jj], "-", dps_mx[jj], "cm"
        )
      }
      
      fo <- file.path(
        pathdir,
        paste0(vars[ii], "_", dps_mn[jj], "_", dps_mx[jj], "cm.tif")
      )
      
      writeBin(httr::content(resp, "raw"), fo)
    }
    
    # ---- weighted depth average ----
    dps  <- c(5, 10, 15, 30, 40, 100)
    wgts <- c(1, 0.5, 0.25, 0.125, 0.0625, 0.03125) * dps
    wgts <- wgts / sum(wgts)
    
    fi <- file.path(pathdir, paste0(vars[ii], "_", dps_mn[1], "_", dps_mx[1], "cm.tif"))
    ri <- terra::rast(fi) * wgts[1]
    if (deletefiles) unlink(fi)
    
    for (jj in 2:6) {
      fi <- file.path(pathdir, paste0(vars[ii], "_", dps_mn[jj], "_", dps_mx[jj], "cm.tif"))
      ri <- terra::rast(fi) * wgts[jj] + ri
      if (deletefiles) unlink(fi)
    }
    
    soildata <- if (is.null(soildata)) ri else c(soildata, ri)
  }
  
  names(soildata) <- vars
  
  # --- Downscale (UNCHANGED function)
  groundparameters <- soildata_downscale( soildata  = soildata, landcover = landcover, 
                                          water = water)
  
  return(groundparameters)
}