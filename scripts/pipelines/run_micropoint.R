# 0. python, sources, credentials, ee, observations

reticulate::use_python("/Users/lizethestevezt/.virtualenvs/rgee/bin/python", required = TRUE)

source("scripts/config_processing/paths.R")
source("scripts/config_processing/sites.R")
source("scripts/config_processing/csv_processing.R")
source("scripts/pipelines/site_level.R")   # new
source("scripts/get_data/download_era5.R")
source("scripts/get_data/get_lai.R")
source("scripts/get_data/get_landcover.R")
source("scripts/get_data/get_albedo.R")
source("scripts/get_data/get_refldata.R")
source("scripts/get_data/get_vegparameters.R")
source("scripts/get_data/get_groundparameters.R")
source("scripts/get_data/canopy_height.R")

credentials <- readRDS("/Users/lizethestevezt/canopymicroenv/credentials_microclimdata.rds")

ee$Authenticate(auth_mode = "notebook")
ee$Initialize(project = "ee-lizethestevezt")

observations <- process_csv("data/EpiphytesDatabase2.csv")

# sites processing 

sites_df     <- get_sites("data/EpiphytesDatabase2.csv")
adj <- mapply(compress_site_window, t_start = sites_df$tme_start, t_end = sites_df$tme_end, SIMPLIFY = FALSE)

sites_df$tme_start <- vapply(adj, `[[`, as.POSIXct(NA), "tme_start")
sites_df$tme_end   <- vapply(adj, `[[`, as.POSIXct(NA), "tme_end")

sites_df$tme_start <- as.POSIXct(sites_df$tme_start, tz = "UTC")
sites_df$tme_end   <- as.POSIXct(sites_df$tme_end,   tz = "UTC")


all_outputs <- list()
k <- 1

# loop

for (s in seq_len(nrow(sites_df))) {
  site_row <- sites_df[s, ]
  
  # subset observations belonging to this site
  obs_here <- observations[observations$lat == site_row$lat_min &
                             observations$lon == site_row$lon_min, ]
  if (nrow(obs_here) == 0) next
  
  # 1) heavy site-level work (once)
  site_inputs <- site_level_inputs(site_row, credentials)
  
  # 2) fast observation-level loop
  for (i in seq_len(nrow(obs_here))) {
    obs <- obs_here[i, ]
    
    reqhgt <- obs$hMed   # height of that zone in the canopy
    
    mout <- runpointmodel(
      climdata = site_inputs$climdata,
      reqhgt   = reqhgt,
      vegp     = site_inputs$vegparameters,
      paii     = NA,
      groundp  = site_inputs$groundparameters,
      lat      = obs$lat,
      long     = obs$lon,
      zref     = 2,
      uref     = 2
    )
    
    # attach meta (species, zone, site id, etc.)
    mout$Spp  <- obs$Spp
    mout$zone <- obs$zone
    mout$Site <- site_row$Site
    
    all_outputs[[k]] <- mout
    k <- k + 1
  }
}

# e.g. bind all outputs
# results_df <- dplyr::bind_rows(all_outputs)
