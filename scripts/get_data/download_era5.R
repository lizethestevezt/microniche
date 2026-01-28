build_era5 <- function(site, paths, overwrite = FALSE) {
  reticulate::use_virtualenv("~/.virtualenvs/rgee", required = TRUE)
  
  reticulate::py_config()
  
  outfile_base <- file.path(paths$era5, paste0("era5_", site$site_id))
  nc_file <- paste0(outfile_base, ".nc")
  
  if (file.exists(nc_file) && !overwrite) {
    message("ERA5 already exists for site ", site$site_id)
    return(nc_file)
  }
  
  req <- build_era5_request(xmin = site$lon_min, xmax = site$lon_max, 
                            ymin = site$lat_min, ymax = site$lat_max,
                            start_time = site$start_time, end_time = site$end_time,
                            by_month = FALSE, outfile_name = outfile_base)
  
  request_era5(request = req, uid = ecmwf_key, out_path = nc_file)
  
  return(nc_file)
}