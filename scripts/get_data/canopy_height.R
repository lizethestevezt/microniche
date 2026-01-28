get_canopy_height <- function(site, CAN_TILE_DIR, LCR = 0.6,
                              folder = "/3deg_cogs") {
  
  # ---- constants (kept local on purpose)
  LIBDRIVE <- "https://libdrive.ethz.ch"
  TOKEN   <- "cO8or7iOe5dT2Rt"
  
  lon <- site$lon0
  lat <- site$lat0
  
  # ---- helper: tile id
  canopy_tile_id <- function(lon, lat) {
    lon0 <- floor(lon / 3) * 3
    lat0 <- floor(lat / 3) * 3
    
    lat_tag <- ifelse(lat0 >= 0,
                      sprintf("N%02d", lat0),
                      sprintf("S%02d", abs(lat0)))
    
    lon_tag <- ifelse(lon0 >= 0,
                      sprintf("E%03d", lon0),
                      sprintf("W%03d", abs(lon0)))
    
    paste0(
      "ETH_GlobalCanopyHeight_10m_2020_",
      lat_tag, lon_tag, "_Map.tif"
    )
  }
  
  # ---- helper: download tile
  nc_download <- function(filename) {
    dir.create(CAN_TILE_DIR, recursive = TRUE, showWarnings = FALSE)
    
    dest <- file.path(CAN_TILE_DIR, filename)
    if (file.exists(dest)) return(dest)
    
    url <- paste0(LIBDRIVE, "/public.php/webdav", folder, "/", filename)
    
    res <- httr::GET(
      url,
      httr::authenticate(TOKEN, ""),
      httr::write_disk(dest, overwrite = TRUE)
    )
    httr::stop_for_status(res)
    
    dest
  }
  
  # ---- helper: extract height
  get_canopy_height_m <- function(lon, lat, tile_path) {
    r <- terra::rast(tile_path)
    
    pt <- terra::vect(
      data.frame(x = lon, y = lat),
      geom = c("x", "y"),
      crs  = terra::crs(r)
    )
    
    v <- terra::extract(r, pt)[1, 2]
    
    if (is.na(v) || v == 255) NA_real_ else as.numeric(v)
  }
  
  # ---- pipeline
  tile_name <- canopy_tile_id(lon, lat)
  tile_path <- nc_download(tile_name)
  
  height_at_pt <- get_canopy_height_m(lon, lat, tile_path)
  
  if (is.na(height_at_pt)) {
    stop("Canopy height is NA at site ", site$site_id)
  }
  
  live_crown_height <- height_at_pt * (1 - LCR)
  
  zonal_heights <- list(
    JZ1 = 1,
    JZ2 = live_crown_height,
    JZ3 = live_crown_height + (LCR * height_at_pt) / 3,
    JZ4 = live_crown_height + (LCR * height_at_pt) * 2 / 3,
    JZ5 = height_at_pt
  )
  
  unlist(zonal_heights)
}