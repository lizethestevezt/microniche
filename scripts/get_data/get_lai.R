get_lai <- function(r, tme, reso = 10, pathout, credentials, 
                    python = "/Users/lizethestevezt/miniforge3/bin/python3") {
  # ---- Python env ----
  Sys.setenv(RETICULATE_PYTHON = python)
  reticulate::py_config()
  
  if (!reso %in% c(10, 500)) {
    stop("reso must be one of 10 or 500")
  }
  
  year <- tme$year[1] + 1900
  
  # ======================
  # HRVPP (10 m, Copernicus)
  # ======================
  if (reso == 10) {
    cyear <- as.POSIXlt(Sys.time())$year + 1900
    if (!year %in% 2019:cyear) {
      stop("Data not available for specified time period")
    }
    
    if (!py_module_available("hda")) {
      stop("Python module 'hda' not available in this environment")
    }
    
    hda <- import("hda")
    conf <- hda$Configuration(
      user     = credentials$username[2],
      password = credentials$password[2]
    )
    client <- hda$Client(config = conf)
    
    bb <- terra::bbox(r)
    
    tmes <- seq(tme[1], tme[length(tme)], by = 5 * 86400)
    ttxt <- paste0(substr(tmes, 1, 10), "T00:00:00.000Z")
    
    files <- 0
    for (i in seq_len(length(ttxt) - 1)) {
      q <- list(
        dataset_id = "EO:EEA:DAT:CLMS_HRVPP_VI",
        productType = "LAI",
        resolution  = "10",
        start = ttxt[i],
        end   = ttxt[i + 1],
        bbox  = bb
      )
      
      matches <- client$search(q)
      if (length(matches) > 0) {
        files <- files + length(matches)
        matches$download(pathout)
        
        q$productType <- "QFLAG2"
        client$search(q)$download(pathout)
      }
    }
    
    if (files == 0) stop("No data for specified location or time period")
  }
  
  # ======================
  # MODIS (500 m)
  # ======================
  else {
    library(luna)
    
    if (tme[length(tme)] < as.POSIXlt("2000-02-18", tz = "UTC")) {
      stop("No data available prior to 2000-02-18")
    }
    
    e <- terra::ext(r)
    r2 <- terra::project(terra::rast(e, crs = terra::crs(r)), "EPSG:4326")
    e2 <- terra::ext(r2)
    
    st <- substr(as.character(tme[1]), 1, 10)
    ed <- substr(as.character(tme[length(tme)]), 1, 10)
    
    mf <- luna::getNASA(
      "MOD15A2H", st, ed,
      aoi = e2,
      version = "061",
      download = FALSE
    )
    
    if (length(mf) == 0) {
      stop("No data for specified location or time period")
    }
    
    luna::getNASA(
      "MOD15A2H", st, ed,
      aoi = e2,
      version = "061",
      download = TRUE,
      path = pathout,
      username = credentials$username[1],
      password = credentials$password[1],
      server = "LPDAAC_ECS"
    )
  }
  
  return(list(
    path = normalizePath(pathout),
    reso = reso,
    source = ifelse(reso == 10, "HRVPP", "MODIS")
  ))
}