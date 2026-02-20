build_era5 <- function(site, overwrite = FALSE) {
  reticulate::use_virtualenv("~/.virtualenvs/rgee", required = TRUE)
  
  reticulate::py_config()
  
  outfile_base <- file.path(ERA5_DIR, paste0("era5_", site$Site))
  nc_file <- paste0(outfile_base, ".nc")
  
  if (file.exists(nc_file) && !overwrite) {
    message("ERA5 already exists for site ", site$Site)
    return(nc_file)
  }
  
  req <- build_era5_request(xmin = site$lon_min, xmax = site$lon_max, 
                            ymin = site$lat_min, ymax = site$lat_max,
                            start_time = site$tme_start, end_time = site$tme_end,
                            by_month = TRUE, outfile_name = outfile_base)
  
  request_era5(request = req, uid = credentials[3, 2], out_path = nc_file)
  
  return(nc_file)
}