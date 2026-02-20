library(readr)
library(tidyr)
library(dplyr)
library(elevatr)
library(sf)
library(parzer)

source("scripts/get_data/canopy_height.R")
source("scripts/config/paths.R")

### --- basic functions --- ####

get_crown_depth <- function(lat, lon) {
  # placeholder – later replaced by real logic
  return(2.3)  # meters
}

get_hMed <- function(zone, height_at_pt, crowndepth, crown_base) {
  
  if (zone == "JZ1") return(0.5)
  
  if (zone == "JZ2") {
    return(mean(c(1, crown_base)))
  }
  
  if (zone == "JZ3") {
    return(mean(c(crown_base,
                  crown_base + crowndepth / 3)))
  }
  
  if (zone == "JZ4") {
    return(mean(c(crown_base + crowndepth / 3,
                  crown_base + 2 * crowndepth / 3)))
  }
  
  if (zone == "JZ5") {
    return(mean(c(crown_base + 2 * crowndepth / 3,
                  height_at_pt)))
  }
}

normalize_dms <- function(x) {
  x <- trimws(x)
  x <- gsub("°", "°", x)
  x <- gsub("′|’", "'", x)
  x <- gsub("″|“|”", "\"", x)
  x <- gsub("\\s+([NSEW])$", " \\1", x)
  x
}

parse_one <- function(x, is_start = TRUE, tz = "UTC") {
  
  x <- as.character(x)
  
  if (grepl("^\\d{4}$", x)) {
    year <- as.integer(x)
    if (is_start) {
      as.POSIXlt(sprintf("%04d-01-01 00:00:00", year), tz = tz)
    } else {
      as.POSIXlt(sprintf("%04d-12-31 23:59:59", year), tz = tz)
    }
    
  } else if (grepl("^\\d{4}-\\d{2}$", x)) {
    year  <- as.integer(substr(x, 1, 4))
    month <- as.integer(substr(x, 6, 7))
    if (is_start) {
      as.POSIXlt(sprintf("%04d-%02d-01 00:00:00", year, month), tz = tz)
    } else {
      last_day <- last_day_of_month(year, month)
      as.POSIXlt(
        sprintf("%04d-%02d-%02d 23:59:59", year, month, last_day),
        tz = tz
      )
    }
    
  } else {
    as.POSIXlt(
      paste0(x, if (is_start) " 00:00:00" else " 23:59:59"),
      tz = tz
    )
  }
}

### --- processing function --- ####

process_csv <- function(csv_path){
  ### --- Import csv --- ####
  df <- read_csv(csv_path, na = c("", "NA", "N/A"))
  
  results <- list()
  k <- 1
  
  ### --- the loop --- ####
  for (i in seq_len(nrow(df))) {
    
    # ---- Species info ----
    source <- df$Source[i]
    if (is.na(df$Source[i])) break
    spp <- paste(df$Genus[i], df$species[i])
    
    lat_dms <- normalize_dms(df$lat[i])
    lon_dms <- normalize_dms(df$lon[i])
    
    coords <- RWmisc::dms2dd(lon = lon_dms, lat = lat_dms)
    
    lat <- coords[[2]]
    lon <- coords[[1]]
    lat0 <- lat + 0.00001
    lon0 <- lon + 0.00001
    
    if (is.na(df$Elevation_m[i])) {
      # Get elevation from coordinates (lat/lon in decimal degrees)
      pt_df <- data.frame(x = c(lon, lon0), y = c(lat, lat0))
      pt_sf <- st_as_sf(x = pt_df, coords = c("x", "y"), crs = 4326)
      pt_rast <- rast(pt_sf, nrow = 2, ncol = 2)
      elev <- get_elev_point(locations = pt_rast, prj = 4326, src = "aws")$elevation[1]
    } else {
      # Split existing elevation string like "123-456"
      elev <- as.numeric(strsplit(df$Elevation_m[i], "-")[[1]])
    }
    
    elevMed <- mean(elev)
    
    # --- Canopy info (from canopy_height.R) ---
    canopy_height <- get_canopy_height(lon, lat)
    if (is.na(canopy_height)) next
    
    crowndepth <- get_crown_depth(lat, lon)
    crown_base <- canopy_height - crowndepth
    
    if (crowndepth >= canopy_height) next
    # --- tme ---
    start <- df$Exp_Start[i]
    end <- df$Exp_End[i]
    
    tme_start <- parse_one(start, TRUE)
    tme_end   <- parse_one(end,   FALSE)
    
    # ---- Loop over Johansson zones ----
    for (zone in paste0("JZ", 1:5)) {
      
      abund <- df[[zone]][i]
      if (abund == 0) next
      
      hMed <- get_hMed(zone, canopy_height, crowndepth, crown_base)
      
      results[[k]] <- data.frame(
        Spp       = spp,
        lat       = lat,
        lon       = lon,
        elevMed   = elevMed,
        zone      = zone,
        hMed      = hMed,
        abund     = abund,
        hCanopy   = canopy_height,
        tme_start = tme_start,
        tme_end   = tme_end
      )
      
      k <- k + 1
      next
    }
    next
  }
  
  observations <- do.call(rbind, results)
  return(observations)
}

get_sites <- function(csv_path) {
  
  df <- read_csv(csv_path, na = c("", "NA", "N/A"))
  
  # --- keep only rows with a site ---
  df <- df |>
    dplyr::filter(!is.na(Source), !is.na(Area_or_Site))
  
  # --- unique sites only ---
  sites_df <- df |>
    dplyr::distinct(
      Area_or_Site, lat, lon,
      Exp_Start, Exp_End,
      .keep_all = TRUE
    )
  
  results <- vector("list", nrow(sites_df))
  
  for (i in seq_len(nrow(sites_df))) {
    
    site <- sites_df$Area_or_Site[i]
    
    lat_dms <- normalize_dms(sites_df$lat[i])
    lon_dms <- normalize_dms(sites_df$lon[i])
    
    coords <- RWmisc::dms2dd(lon = lon_dms, lat = lat_dms)
    
    lat_min <- coords[[2]]
    lon_min <- coords[[1]]
    lat_max <- lat_min + 0.00001
    lon_max <- lon_min + 0.00001
    
    tme_start <- parse_one(sites_df$Exp_Start[i], TRUE)
    tme_end   <- parse_one(sites_df$Exp_End[i],   FALSE)
    
    results[[i]] <- data.frame(
      Site      = site,
      lat_min   = lat_min,
      lon_min   = lon_min,
      lat_max   = lat_max,
      lon_max   = lon_max,
      tme_start = tme_start,
      tme_end   = tme_end
    )
  }
  
  dplyr::bind_rows(results)
}