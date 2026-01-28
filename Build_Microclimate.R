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



######### ERA 5 #############
# Local path for the ERA5 output (adjust if your mcera5 naming differs)
ERA5_NC <- file.path(BASE_DIR, "/ERA5/output_1_2022.nc")

############## 2. microclimdata inputs → vegparameters, groundparameters -----------

# Template raster around the point


# --- Run the GEE exports (tasks)
# NOTE: these export to Drive; you still have to download the .tif to GEE_DIR.




# --- Load downloaded rasters from local disk
# TODO: confirm filenames match what Drive gave you
landcover <- rast(file.path(GEE_DIR, "ESA_WorldCover_2023.tif"))
vhgt      <- rast(file.path(GEE_DIR, "canopy_height_2020.tif"))

# --- LAI download (WEkEO hda) 
py_config()
py_module_available("hda")



# --- Albedo download (ensure absolute path and same python env)


# --- Derived vegetation parameters








############## 3. Canopy height at point → reqhgt (Johansson proxy) --------


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
