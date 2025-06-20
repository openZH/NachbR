
#' Get polygons for publications
#'
#' By combining the BFS- and the cadaster-number (unique combination), this
#' function adds a polygon for each combination in a given publication - if
#' being found. Note that `clean_cadaster` cannot cover all possible cases.
#' Hence, so combinations cannot be matched. Also note that a single project can
#' involve multiple parcels.
#'
#' @inheritParams clean_df_bp
#' @inheritParams create_gpkg
#'
#' @return A data frame consisting of publication including a polygon column
#'
#' @export
#'
get_geo_info <- function(df_bp, sf_liegenschaften) {
  df_bp_geo <- df_bp |>
    dplyr::left_join(sf_liegenschaften,
                     by = c(
                       "bfs_nr" = "bfsnr",
                       "districtCadastre_relation_cadastre" = "nummer"
                     )
    ) |>
    dplyr::mutate(geo_match = ifelse(is.na(objid), NA, 1))
  
  geo_info_check(df_bp, df_bp_geo)
  
  return(df_bp_geo)
}




#' Download and Load Liegenschaften Layer from GISZH API
#'
#' Downloads the Liegenschaften shapefile from the GISZH API if the specified
#' retrieval day matches the current weekday or if the shapefile does not already
#'  exist locally. The function then reads the shapefile into an sf object.
#'
#' @param file_destination A string specifying the destination folder where the
#' shapefile should be stored.
#' @param email_address A string containing a valid email address required by
#' the GISZH API for data download.
#' @param retrieval_day A string specifying the weekday (e.g., "Monday") on which
#' the data should be re-downloaded.
#'
#' @return An sf object containing the Liegenschaften spatial data
#' @export
#'
#' @examples
#' \dontrun{
#' get_liegenschaften_layer("data", "your.email@example.com", "Monday")
#' }
get_liegenschaften_layer <- function(file_destination, email_address, retrieval_day) {
  
  sf_file <- paste0(
    file_destination, 
    "/AV_MOpublic-_Liegenschaften_-OGD/AVZH_LIEGENSCHAFTEN_F.shp"
  )
  
  if (weekdays(Sys.Date()) == retrieval_day | !file.exists(sf_file)) {
    dir.create(file_destination)
    
    url <- get_giszh_api_download_url(404, email_address)
    
    # Define the destination for the downloaded ZIP file
    zip_file <- tempfile(fileext = ".zip")
    
    # Download & unzip the ZIP file
    download.file(url, destfile = zip_file, mode = "wb")
    unzip(zip_file, exdir = file_destination)
  }
  
  sf_liegenschaf <- sf::read_sf(sf_file) |>
    dplyr::rename_with(tolower)
  
  return(sf_liegenschaf)
}


#' Request Download URL from GISZH API
#'
#' Sends a request to the GISZH API to generate a download URL for a given
#' product. Waits until the data is ready and returns the download URL.
#'
#' @param source_location An integer identifying the product ID to download
#' (e.g., 404 for Liegenschaften).
#' @param email_address A string containing a valid email address to receive the
#'  data request.
#'
#' @return A string containing the download URL for the requested product.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- get_giszh_api_download_url(404, "your.email@example.com")
#' }
get_giszh_api_download_url <- function(source_location, email_address) {
  source_location <- as.numeric(source_location)
  
  jsondata <- httr2::request("https://geoservices.zh.ch/geoshopapi/v1/orders") |>
    httr2::req_body_json(
      list(
        email = email_address,
        perimeter_type = "DIRECT",
        pdir_polygon = list(
          type = "Polygon",
          coordinates = list(
            list(
              list(2652199, 1223163),
              list(2717985, 1223163),
              list(2717985, 1288018),
              list(2652199, 1288018),
              list(2652199, 1223163)
            )
          )
        ),
        pdir_coordsys = "LV95",
        products = list(
          list(
            product_id = source_location,
            format_id = 3
          )
        )
      )
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  status <- "NO"
  
  while (!grepl("SUCCESS", status)) {
    Sys.sleep(60)
    status_json <- httr2::request(jsondata$status_url) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    
    status <- status_json$status
    print(status)
    
    if (grepl("FAILURE", status)) {
      cli::cli_abort("The API has returned an error")
    }
  }
  
  return(jsondata$download_url)
}





#' Check if data was lost in geo-matching process
#'
#' This functions checks if rows are lost in the geo-matching process and if so,
#' returns the lost IDs.
#'
#' @inheritParams clean_df_bp
#' @param df_bp_geo A data frame consisting of publication including a polygon
#' column
#'
#' @export
#'
geo_info_check <- function(df_bp, df_bp_geo) {
  # Check if orginal and resulting data frame are the same
  test1 <- nrow(df_bp) == nrow(df_bp_geo)
  test2 <- all(sort(unique(df_bp$id)) == sort(unique(df_bp_geo$id)))
  
  if (all(test1, test2)) {
    return()
  } else {
    lost_ids <- setdiff(unique(df_bp$id), unique(df_bp_geo$id))
    
    cli::cli_abort(cat("The IDs were lost in the matching process: \n", lost_ids))
  }
}

