### Get climate data pipeline ----------------------------------------------

############## 0. Basic data: coordinates, dates, python/SSL, rgee ---------

# --- Seting python environment for the project
Sys.setenv(RETICULATE_PYTHON = "/Users/lizethestevezt/.virtualenvs/rgee/bin/python")

library(reticulate)
py_config()

# --- packages
library(sp)
library(terra)
library(rgee)
library(sf)
library(mcera5)
library(microclimdata)

# --- Initialize Earth Engine (do this ONCE, before any ee$ calls)
ee_Initialize(
  user    = "lizethestevezt",
  project = "ee-lizethestevezt"   # <-- your GCP project id
)

# --- time window
start_time <- as.POSIXlt("2022-08-01 00:00:00", tz = "UTC")
end_time   <- as.POSIXlt("2022-08-31 00:00:00", tz = "UTC")

# --- coordinates
coords <- 
  RWmisc::dms2dd(
  lon = "77° 07' 07.5\" W",
  lat = "05° 54' 17.9\" S"
)

lon0 <- coords[1, 1]
lat0 <- coords[1, 2]

# --- paths (ABSOLUTE, always)
BASE_DIR <- "/Users/lizethestevezt/EcoModLab"
GEE_DIR  <- file.path(BASE_DIR, "GEE_data/")
LAI_DIR  <- file.path(GEE_DIR, "lai/")
ALB_DIR  <- file.path(BASE_DIR, "albedo/")
SOIL_DIR <- file.path(BASE_DIR, "groundparameters/")
CAN_TILE_DIR <- file.path(BASE_DIR, "canopy_tiles/")

dir.create(BASE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(GEE_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(LAI_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(ALB_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(SOIL_DIR, recursive = TRUE, showWarnings = FALSE)

# --- bbox for “micro-area” around point
pad <- 0.01
lon_min <- lon0 - pad
lon_max <- lon0 + pad
lat_min <- lat0 - pad
lat_max <- lat0 + pad

# --- Dates vector for MODIS etc (microclimdata expects POSIXlt for $year etc)
tme <- as.POSIXlt(seq(from = start_time, to = end_time, by = "1 day"), tz = "UTC")

# --- credentials
mycredentials <- readRDS("/Users/lizethestevezt/EcoModLab/credentials_microclimdata.rds")

# --- macOS-safe GEE export functions (no reticulate::use_python here!)
# They assume Python is already set in step 0 and rgee is initialized.

lcover_download_macos <- function(r, type = "ESA", year = 2018,GoogleDrivefolder, projectname = NA, silent = FALSE) {
  
  typecheck <- type %in% c("ESA", "CORINE")
  if (!typecheck) stop("type must be one of ESA or CORINE")
  
  if (type == "CORINE") {
    yearcheck <- year %in% c(1986:2018)
    if (!yearcheck) stop("CORINE land cover data only available on GEE for 1986 to 2018")
  }
  
  if (!is.na(projectname)) ee$Initialize(project = projectname)
  
  e <- terra::ext(r)
  r2 <- terra::rast(e)
  terra::crs(r2) <- terra::crs(r)
  r2 <- terra::project(r2, "EPSG:4326")
  e <- terra::ext(r2)
  
  proj_string <- terra::crs(r, describe = TRUE)
  epsg_code <- paste0("EPSG:", proj_string$code)
  
  aoi <- ee$Geometry$Rectangle(c(e$xmin, e$ymin, e$xmax, e$ymax))
  aoi_coordinates <- aoi$bounds()$getInfo()$coordinates[[1]]
  
  if (type == "ESA") {
    collection <- ee$ImageCollection("ESA/WorldCover/v100")
    lcover <- collection$first()
    task <- ee$batch$Export$image$toDrive(
      image          = lcover,
      description    = "ESA_WorldCover_Export",
      folder         = GoogleDrivefolder,
      fileNamePrefix = "ESA_WorldCover_2023",
      region         = aoi_coordinates,
      scale          = 10,
      crs            = epsg_code
    )
  } else {
    dataset <- ee$Image(paste0("COPERNICUS/CORINE/V20/100m/", year))
    landCover <- dataset$select("landcover")
    task <- ee$batch$Export$image$toDrive(
      image          = landCover,
      description    = "CORINE_LandCover_Export",
      folder         = GoogleDrivefolder,
      fileNamePrefix = paste0("CORINE_LandCover_", year),
      region         = aoi_coordinates,
      scale          = 100,
      crs            = epsg_code
    )
  }
  
  task$start()
  if (!silent) {
    message("GEE export started. Task id: ", task$id)
    message("Check progress in Earth Engine Tasks tab and download from Drive.")
  }
  invisible(task)
}
vegheight_download_macos <- function(r, GoogleDrivefolder, projectname = NA, silent = FALSE) {
  
  if (!is.na(projectname)) ee$Initialize(project = projectname)
  
  e <- terra::ext(r)
  r2 <- terra::rast(e)
  terra::crs(r2) <- terra::crs(r)
  r2 <- terra::project(r2, "EPSG:4326")
  e <- terra::ext(r2)
  
  proj_string <- terra::crs(r, describe = TRUE)
  epsg_code <- paste0("EPSG:", proj_string$code)
  
  aoi <- ee$Geometry$Rectangle(c(e$xmin, e$ymin, e$xmax, e$ymax))
  aoi_coordinates <- aoi$bounds()$getInfo()$coordinates[[1]]
  
  canopy_height <- ee$Image("users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1")
  
  task <- ee$batch$Export$image$toDrive(
    image          = canopy_height,
    description    = "canopy_height_export",
    folder         = GoogleDrivefolder,
    fileNamePrefix = "canopy_height_2020",
    region         = aoi_coordinates,
    scale          = 10,
    crs            = epsg_code
  )
  
  task$start()
  if (!silent) rgee::ee_monitoring(task)
  invisible(task)
}
lai_download_macos <- function(r, tme, reso = 10, pathout, credentials) {
  
  # ---- checks ----
  if (!reso %in% c(10, 500)) stop("reso must be one of 10 or 500")
  
  if (!inherits(tme, "POSIXlt")) {
    stop("tme must be POSIXlt (needed because tme$year is used). Use: tme <- as.POSIXlt(seq(...), tz='UTC')")
  }
  
  dir.create(pathout, recursive = TRUE, showWarnings = FALSE)
  
  year <- tme$year[1] + 1900
  
  # ---- 10m LAI: WeKEO via hda ----
  if (reso == 10) {
    
    cyear <- as.POSIXlt(Sys.time(), tz = "UTC")$year + 1900
    if (!year %in% 2019:cyear) stop("10m LAI data not available for specified time period (needs >= 2019).")
    
    # WeKEO creds are row 2 in microclimdata::credentials template
    wekeo_user <- credentials$username[2]
    wekeo_pass <- credentials$password[2]
    
    if (is.na(wekeo_user) || is.na(wekeo_pass) || wekeo_user == "" || wekeo_pass == "") {
      stop("Missing WeKEO credentials (credentials row 2). Fill credentials$username[2] and credentials$password[2].")
    }
    
    # Import hda (Python) in the already-initialized reticulate env
    hda <- reticulate::import("hda", delay_load = FALSE)
    
    conf <- hda$Configuration(user = wekeo_user, password = wekeo_pass)
    hda_client <- hda$Client(config = conf)
    
    bb <- bbox(r)
    
    # hda queries in 5-day chunks
    tmes <- seq(tme[1], tme[length(tme)], by = 5 * 3600 * 24)
    ttxt <- paste0(substr(as.character(tmes), 1, 10), "T00:00:00.000Z")
    
    n <- length(ttxt)
    if (n < 2) stop("tme range too short for 5-day chunking; provide a longer time window.")
    
    st <- ttxt[1:(n - 1)]
    ed <- ttxt[2:n]
    
    files <- 0L
    
    for (i in seq_along(st)) {
      
      # LAI
      query_lai <- list(
        dataset_id  = "EO:EEA:DAT:CLMS_HRVPP_VI",
        productType = "LAI",
        resolution  = "10",
        start       = st[i],
        end         = ed[i],
        bbox        = bb
      )
      
      matches <- hda_client$search(query_lai)
      
      if (length(matches) > 0) {
        files <- files + length(matches)
        matches$download(pathout)
        
        # QFLAG2
        query_qf <- list(
          dataset_id  = "EO:EEA:DAT:CLMS_HRVPP_VI",
          productType = "QFLAG2",
          resolution  = "10",
          start       = st[i],
          end         = ed[i],
          bbox        = bb
        )
        
        matches_qf <- hda_client$search(query_qf)
        if (length(matches_qf) > 0) matches_qf$download(pathout)
      }
    }
    
    if (files == 0L) stop("No 10m LAI data for specified location/time period (WeKEO/HDA returned 0 matches).")
    
    # If you want to return downloaded filenames instead of just side effects:
    return(list(reso = 10, pathout = pathout, files_downloaded = files))
  }
  
  # ---- 500m LAI: MODIS via luna + Earthdata ----
  if (reso == 500) {
    
    tsed <- tme[length(tme)]
    tmod <- as.POSIXlt(0, origin = "2000-02-18", tz = "UTC")
    if (tsed < tmod) stop("No 500m LAI data available prior to 2000-02-18")
    
    e <- terra::ext(r)
    r2 <- terra::rast(e)
    terra::crs(r2) <- terra::crs(r)
    r2 <- terra::project(r2, "EPSG:4326")
    e2 <- terra::ext(r2)
    
    st <- substr(as.character(tme[1]), 1, 10)
    ed <- substr(as.character(tme[length(tme)]), 1, 10)
    
    # Earthdata creds are row 1 in template
    username <- credentials$username[1]
    password <- credentials$password[1]
    
    mf <- luna::getNASA("MOD15A2H", st, ed, aoi = e2, version = "061", download = FALSE)
    
    if (length(mf) > 0) {
      luna::getNASA(
        "MOD15A2H", st, ed, aoi = e2, version = "061",
        download = TRUE, path = pathout,
        username = username, password = password,
        server = "LPDAAC_ECS"
      )
      return(list(reso = 500, pathout = pathout, files_found = length(mf)))
    } else {
      stop("No 500m LAI data for specified location/time period (MODIS query returned 0 files).")
    }
  }
}
soildata_download_macos <- function(r, pathdir = getwd(), deletefiles = TRUE) {
  
  dir.create(pathdir, recursive = TRUE, showWarnings = FALSE)
  
  e <- terra::ext(r)
  subsetx <- paste0("&SUBSET=X(", e$xmin, ",", e$xmax, ")")
  subsety <- paste0("&SUBSET=Y(", e$ymin, ",", e$ymax, ")")
  
  epsg_code <- terra::crs(r, describe = TRUE)$code
  if (is.null(epsg_code) || is.na(epsg_code)) {
    stop("Could not extract EPSG code from r")
  }
  
  subsetcrs <- paste0(
    "&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/", epsg_code
  )
  outcrs <- paste0(
    "&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/", epsg_code
  )
  
  vars   <- c("bdod", "clay", "sand", "silt")
  dps_mn <- c(0, 5, 15, 30, 60, 100)
  dps_mx <- c(5, 15, 30, 60, 100, 200)
  
  ro <- NULL
  
  for (ii in seq_along(vars)) {
    
    # ---- download 6 depths ----
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
    
    # ---- weighted average across depths ----
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
    
    if (is.null(ro)) {
      ro <- ri
    } else {
      ro <- c(ro, ri)
    }
  }
  
  names(ro) <- vars
  ro
}
bbox <- function (r) {
  e <- ext(r)
  xy <- data.frame(x = c(e$xmin, e$xmin, e$xmax, e$xmax), 
                   y = c(e$ymin, e$ymax, e$ymin, e$ymax))
  xy <- sf::st_as_sf(xy, coords = c("x", "y"), crs = crs(r))
  ll <- sf::st_transform(xy, 4326)
  ll <- data.frame(sf::st_coordinates(ll))
  out <- c(min(ll$X), min(ll$Y), max(ll$X), max(ll$Y))
  return(out)
}

############## 1. ERA5 with mcera5 → climdata ------------------------------

# Local path for the ERA5 output (adjust if your mcera5 naming differs)
ERA5_NC <- file.path(BASE_DIR, "/ERA5/output_1_2022.nc")

# 1a. Build ERA5 request (mcera5 uses xmin/xmax/ymin/ymax in your setup)
request_build <- build_era5_request(
  xmin         = lon_min,
  xmax         = lon_max,
  ymin         = lat_min,
  ymax         = lat_max,
  start_time   = start_time,
  end_time     = end_time,
  by_month     = FALSE,
  outfile_name = "output_1"
)

# 1b. Set ECMWF key
mykey <- ecmwfr::wf_set_key(
  key  = "d0299130-aafa-4d28-994a-004244ab9498",
  user = "lizethestevezt@gmail.com"
)

# 1c. Submit request
# NOTE: argument names vary by mcera5 version.
# If your request_era5 wants key= instead of uid=, switch it.
submit_request <- request_era5(
  request  = request_build,
  uid      = mykey,
  out_path = BASE_DIR
)

# 1d. extract climdata based on the ERA5 data
# NOTE: this assumes the file exists at ERA5_NC.
climdata <- extract_clim(
  nc         = ERA5_NC,
  long       = lon0,
  lat        = lat0,
  start_time = start_time,
  end_time   = end_time,
  format     = "micropoint")

############## 2. microclimdata inputs → vegparameters, groundparameters -----------

# Template raster around the point
SpatRaster <- rast(
  extent     = ext(lon_min, lon_max, lat_min, lat_max),
  resolution = 0.0001,
  crs        = "EPSG:4326"
)

# --- Run the GEE exports (tasks)
# NOTE: these export to Drive; you still have to download the .tif to GEE_DIR.
lcover_download_macos(
  r                = SpatRaster,
  type             = "ESA",
  GoogleDrivefolder = "EcoMod",
  projectname      = "ee-lizethestevezt"
)

vegheight_download_macos(
  r                = SpatRaster,
  GoogleDrivefolder = "EcoMod",
  projectname      = "ee-lizethestevezt"
)

# --- Load downloaded rasters from local disk
# TODO: confirm filenames match what Drive gave you
landcover <- rast(file.path(GEE_DIR, "ESA_WorldCover_2023.tif"))
vhgt      <- rast(file.path(GEE_DIR, "canopy_height_2020.tif"))

# --- LAI download (WEkEO hda) - YOUR function placeholder
# TODO: make sure mycredentials exists & is in the right format
# NOTE: use absolute pathout
lai <- lai_download_macos(
  r            = SpatRaster,
  tme          = tme,
  reso         = 10,
  pathout      = LAI_DIR,
  credentials  = mycredentials
)

# --- Albedo download (ensure absolute path and same python env)
albedo_download(
  r            = SpatRaster,
  tme          = tme,
  pathout      = ALB_DIR,
  credentials  = mycredentials
)

alb <- albedo_process(r = SpatRaster,
                      pathin = ALB_DIR)

# --- Derived vegetation parameters
x <- x_calc(landcover = landcover, lctype = "ESA")

refldata <- reflectance_calc(
  alb     = alb,
  lai     = lai,
  x       = x,
  maxiter = 75,
  tol     = 0.001,
  bwgt    = 0.7
)

vegparameters <- create_vegpoint(
  landcover = landcover,
  vhgt      = vhgt,
  lai       = lai,
  refldata  = refldata,
  lctype    = "ESA"
)

groundparameters <- soildata_download_fixed(
  r           = SpatRaster,
  pathdir     = SOIL_DIR,
  deletefiles = FALSE
)
groundparameters <- soildata_downscale(soildata = groundparameters, landcover = landcover, water = 80 )

############## 3. Canopy height at point → reqhgt (Johansson proxy) --------
library(httr)
library(xml2)

LIBDRIVE <- "https://libdrive.ethz.ch"
TOKEN   <- "cO8or7iOe5dT2Rt"

# Get a list of file names within the folder
nc_list <- function(folder = "/3deg_cogs") {
  url <- paste0(LIBDRIVE, "/public.php/webdav", folder, "/")
  res <- httr::VERB(
    "PROPFIND", url,
    httr::authenticate(TOKEN, ""),
    httr::add_headers(Depth = "1"),
    encode = "raw"
  )
  httr::stop_for_status(res)
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  
  doc <- xml2::read_xml(txt)
  hrefs <- xml2::xml_text(xml2::xml_find_all(doc, ".//d:href", xml2::xml_ns(doc)))
  hrefs <- unique(hrefs)
  hrefs
}

hrefs <- nc_list("/3deg_cogs")
head(hrefs, 5)

# Get the file name for the tile I need
canopy_tile_id <- function(lon, lat) {
  # snap to 3-degree grid
  lon0 <- floor(lon / 3) * 3
  lat0 <- floor(lat / 3) * 3
  
  lat_tag <- ifelse(lat0 >= 0,
                    sprintf("N%02d", lat0),
                    sprintf("S%02d", abs(lat0)))
  
  lon_tag <- ifelse(lon0 >= 0,
                    sprintf("E%03d", lon0),
                    sprintf("W%03d", abs(lon0)))
  
  paste0("ETH_GlobalCanopyHeight_10m_2020_",
         lat_tag, lon_tag, "_Map.tif")
}

# Download the specific file I need
nc_download <- function(filename,
                        folder = "/3deg_cogs",
                        destdir = CAN_TILE_DIR) {
  dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
  url  <- paste0(LIBDRIVE, "/public.php/webdav", folder, "/", filename)
  dest <- file.path(destdir, filename)
  
  res <- httr::GET(url, httr::authenticate(TOKEN, ""), httr::write_disk(dest, overwrite = TRUE))
  httr::stop_for_status(res)
  
  dest
}

tile_path <- nc_download(canopy_tile_id(lon0, lat0))

# Extract the canopy height value for the point I need
get_canopy_height_m <- function(lon, lat, tile_path) {
  stopifnot(is.numeric(lon), is.numeric(lat))
  if (lon < -180 || lon > 180 || lat < -90 || lat > 90) {
    stop("Coordinates look wrong.")
  }
  if (missing(tile_path) || !nzchar(tile_path)) {
    stop("Provide tile_path (full path to the .tif tile).")
  }
  if (!file.exists(tile_path)) {
    stop("Tile file not found: ", tile_path)
  }
  
  # read tile
  r <- terra::rast(tile_path)
  
  # build point (same CRS as tile; tiles are EPSG:4326, but we use r's CRS anyway)
  pt <- terra::vect(data.frame(x = lon, y = lat), geom = c("x", "y"), crs = terra::crs(r))
  
  # extract value
  v <- terra::extract(r, pt)[1, 2]
  
  # nodata handling (255) + NA
  if (is.na(v) || v == 255) NA_real_ else as.numeric(v)
}

height_at_pt <- get_canopy_height_m(lon = lon0, lat = lat0, tile_path = tile_path)

LCR <- 0.6 # live crown rate, this is a variable that changes depending the forest. Right now is a placeholder with generalized knowledge
live_crown_height <- height_at_pt - LCR * height_at_pt

zonal_heights <- list(
  JZ1 = 1,
  JZ2 = live_crown_height,
  JZ3 = live_crown_height + (LCR * height_at_pt) / 3,
  JZ4 = live_crown_height + (LCR * height_at_pt) * 2/3,
  JZ5 = height_at_pt
)

reqhgt <- unlist(zonal_heights)


############## 4. Run micropoint point model ------------------------------
library(micropoint)

mout <- runpointmodel(
  climdata = climdata,
  reqhgt   = 10.4,
  vegp     = vegparameters,
  paii     = NA,
  groundp  = groundparameters,
  lat      = lat0,
  long     = lon0,
  zref     = 2,
  uref     = 2
)
