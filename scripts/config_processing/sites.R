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

compress_site_window <- function(t_start, t_end) {
  t_start <- as.POSIXct(t_start, tz = "UTC")
  t_end   <- as.POSIXct(t_end,   tz = "UTC")
  
  y_start <- as.integer(format(t_start, "%Y"))
  y_end   <- as.integer(format(t_end,   "%Y"))
  
  if (y_start == y_end) {
    return(list(tme_start = t_start, tme_end = t_end))
  }
  
  rep_year <- y_start + 1L
  tme_start_new <- as.POSIXct(sprintf("%d-01-01 00:00:00", rep_year), tz = "UTC")
  tme_end_new   <- as.POSIXct(sprintf("%d-12-31 23:59:59", rep_year), tz = "UTC")
  
  list(tme_start = tme_start_new, tme_end = tme_end_new)
}
