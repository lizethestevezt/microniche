# interactive single-site test
### --- load needed packages --- ####

library(terra)
library(rgee)
library(microclimdata)
library(mcera5)
library(micropoint)

### --- credentials --- ####

credentials <- readRDS("~/microniche/credentials_microclimdata.rds")

### --- Add subscripts with process' functions --- ####
source("scripts/config/paths.R")
source("scripts/config/sites.R")

source("scripts/get_data/get_lai.R")
source("scripts/get_data/get_landcover.R")
source("scripts/get_data/get_albedo.R")
source("scripts/get_data/get_refldata.R")
source("scripts/get_data/get_vegparameters.R")
source("scripts/get_data/download_era5.R")
source("scripts/get_data/canopy_height.R")
source("scripts/csv_processing.R")

### --- initialize Earth Engine --- ####
reticulate::use_python("/Users/lizethestevezt/.virtualenvs/rgee/bin/python", required = TRUE)
ee$Authenticate(auth_mode='notebook')
ee$Initialize(project='ee-lizethestevezt')

### --- Attach sites csv file for coordinates --- ####

observations <- process_csv(csv_path = "data/EpiphytesDatabase2.csv")
sites <- get_sites(csv_path = "data/EpiphytesDatabase2.csv")
site <- observations[1, ]

### --- Beginning of process --- ####

r_df <- data.frame( x = observations$lon, y = observations$lat, value = observations$hCanopy)
r_vect <- vect(r_df, geom = c("x", "y"), crs = "EPSG:4326")
template_raster <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90, res = 1, crs = "EPSG:4326")
r <- rasterize(x = r_vect, y = template_raster, field = "value", fun = mean, na.rm = TRUE)

tme <- as.POSIXlt(seq(from = site$tme_start, to = site$tme_end, by = "1 day"), tz = "UTC")

# 1. era5 -> climdata
clim_path <- build_era5(site = sites[1, ])

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

ee_extract()