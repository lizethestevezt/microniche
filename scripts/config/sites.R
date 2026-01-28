library(terra)  # for rast() and ext()

make_site <- function(site_id, lon_dms, lat_dms, start_date, end_date, pad = 0.01) {
  
  coords <- RWmisc::dms2dd(lon = lon_dms, lat = lat_dms)
  
  lon0 <- coords[1, 1]
  lat0 <- coords[1, 2]
  
  start_time <- as.POSIXlt(start_date, tz = "UTC")
  end_time   <- as.POSIXlt(end_date, tz = "UTC")
  
  tme <- seq(start_time, end_time, by = "1 day")
  tme <- as.POSIXlt(tme, tz = "UTC")
  
  # --- add SpatRaster
  template_rast <- rast(
    extent     = ext(lon0 - pad, lon0 + pad, lat0 - pad, lat0 + pad),
    resolution = 0.0001,
    crs        = "EPSG:4326"
  )
  
  list(
    site_id    = site_id,
    lon0       = lon0,
    lat0       = lat0,
    lon_min    = lon0 - pad,
    lon_max    = lon0 + pad,
    lat_min    = lat0 - pad,
    lat_max    = lat0 + pad,
    start_time = start_time,
    end_time   = end_time,
    tme        = tme,
    SpatRaster = template_rast
  )
}


load_sites <- function(csv_path) {
  sites_df <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  sites <- lapply(seq_len(nrow(sites_df)), function(i) {
    with(sites_df[i, ],
         make_site(
           site_id    = site_id,
           lon_dms    = lon_dms,
           lat_dms    = lat_dms,
           start_date = start_date,
           end_date   = end_date
         ))
  })
  
  names(sites) <- sites_df$site_id
  sites
}