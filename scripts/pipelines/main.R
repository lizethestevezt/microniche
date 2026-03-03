# keys

ecmwf_key <- ecmwfr::wf_set_key(key  = "d0299130-aafa-4d28-994a-004244ab9498",
                                user = "lizethestevezt@gmail.com")

# main.R ---------------------------------
source("paths.R")
source("sites.R")

source("get_lai.R")
source("get_landcover.R")
source("download_era5.R")
source("get_albedo.R")
source("get_vegparameters.R")
source("get_groundparameters.R")
source("get_refldata.R")
source("canopy_height.R")


library(terra)
library(rgee)
library(microclimdata)
library(micropoint)

ee_Initialize(user = "lizethestevezt", project = "ee-lizethestevezt")

sites <- load_sites("data/sites.csv")

for (site in sites) {
  
  message("Processing site: ", site$site_id)
  
  # 0. preamble
  tme <- site$tme
  
  # 1. era5 -> climdata
  climdata <- build_era5(site, paths)
  
  # 2. landocver
  landcover <- get_landcover(site, ESA_DIR)
  
  # 3. veg height -> vhgt (this one inside vegparams)
  # 4. lai
  lai_dir <- get_lai(site$SpatRaster, tme, pathout = LAI_DIR, credentials)
  lai <- rast(lai_dir)
  
  # 5. albedo -> alb
  alb <- get_albedo(site$SpatRaster, tme, ALB_DIR, mycredentials)
  
  # 6. refl_data
  refldata <- get_refldata(landcover, alb, lai)
  
  # 7. vegparams -> vegparameters
  vegparameters <- get_vegparameters(landcover, lai, refldata)
  
  # 8. groundparams -> groundparameters
  groundparameters <- get_groundparams(r = site$SpatRaster, landcover = landcover, pathdir = SOIL_DIR, 
                                       deletefiles = FALSE)
  
  # 9. canopy height -> reqhgt
  reqhgt <- get_canopy_height(site, CAN_TILE_DIR)
  
  # 10. build climate
  mout <- runpointmodel(climdata = climdata, reqhgt = reqhgt, vegp = vegparameters,
                        paii = NA, groundp = groundparameters, lat = site$lat0, 
                        long = site$lon0, zref = 2, uref = 2)
  
}