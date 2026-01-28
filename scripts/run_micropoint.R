# interactive single-site test

source("config/paths.R")
source("config/sites.R")

source("download/get_lai.R")
source("download/get_landcover.R")
source("download/get_albedo.R")
source("download/get_refldata.R")
source("download/get_vegparameters.R")
source("download/download_era5.R")
source("download/canopy_height.R")

library(terra)
library(rgee)
library(microclimdata)
library(mcera5)
library(micropoint)

ee_Initialize(user = "lizethestevezt", project = "ee-lizethestevezt")

sites <- load_sites("data/sites.csv")
site  <- sites[["ECU_01"]]

r   <- site$SpatRaster
tme <- site$tme

# 1. era5 -> climdata
clim <- build_era5(site = site, paths = paths)

# 2. landocver
landcover <- get_landcover(site, out_dir = ESA_DIR, type = "ESA")

# 3. veg height -> vhgt (this one inside vegparams)
# 4. lai
lai_dir <- get_lai(r = r, tme = tme, pathout = LAI_DIR, credentials = mycredentials)
lai <- rast(lai_dir)

# 5. albedo -> alb
alb <- get_albedo(template_rast = r, tme = tme, ALB_DIR = ALB_DIR, mycredentials = mycredentials)

# 6. refl_data
refldata <- get_refldata(landcover = landcover, alb = alb, lai = lai)

# 7. vegparams -> vegparameters
vegparameters <- get_vegparameters(landcover = landcover, lai = lai, refldata = refldata)

# 8. groundparams -> groundparameters
groundparameters <- get_groundparams(r = site$SpatRaster, landcover = landcover, pathdir = SOIL_DIR, 
                                     deletefiles = FALSE)

# 9. canopy height -> reqhgt
reqhgt <- get_canopy_height(site, CAN_TILE_DIR)

# 10. build climate
mout <- runpointmodel(climdata = climdata, reqhgt = reqhgt, vegp = vegparameters,
                      paii = NA, groundp = groundparameters, lat = site$lat0, 
                      long = site$lon0, zref = 2, uref = 2)
