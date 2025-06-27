#' Create or Update a GeoPackage with Spatial Building Publication Data
#'
#' Writes filtered and spatially prepared building permit data to a GeoPackage
#' file. If the file does not exist, it creates a new one. If it exists, it
#' appends only the new entries based on project IDs.
#'
#' @param df_bp A data frame containing building permit information including
#' @param gpkg_file The name of the .gpkg-file. For example `baupub.gpkg`.
#' @param sf_liegenschaften A shapefile containing the Liegenschaften-data
#' geometry and metadata.
#' 
#' @return Writes data to a GeoPackage file. Returns nothing.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_gpkg("baupub.gpkg", df_bp, sf_liegenschaften)
#' }
create_gpkg <- function(gpkg_file, df_bp, sf_liegenschaften) {
  if (!file.exists(gpkg_file)) {
    sf_bp_geo <- df_bp |>
      dplyr::filter(publicationDate >= "2025-01-01") |>
      add_spatial_information(sf_liegenschaften)

    sf::st_write(sf_bp_geo,
      dsn = gpkg_file,
      layer = "BauPub",
      driver = "GPKG",
      append = FALSE
    )
  } else {
    current_id <- sf::st_read(gpkg_file, query = "SELECT id FROM baupub") |>
      dplyr::pull()

    sf_bp_geo <- df_bp |>
      dplyr::filter(publicationDate >= "2025-01-01") |>
      dplyr::filter(!id %in% current_id) |>
      add_spatial_information(sf_liegenschaften)

    sf::st_write(sf_bp_geo,
      dsn = "baupub.gpkg",
      layer = "BauPub",
      driver = "GPKG",
      append = TRUE
    )
  }
}


#' Prepare Building Permit Data for Export as Spatial Features
#'
#' Converts building permit data into a spatial format with Swiss coordinate
#' reference system (EPSG:2056), enriches it with URL and address information,
#' and ensures entry deadlines are filled.
#'
#' @param df_bp A data frame containing building permit data.
#' @inheritParams create_gpkg
#'
#' @return An `sf` object (simple features data frame) ready for export.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sf_data <- add_spatial_information(df_bp)
#' }
add_spatial_information <- function(df_bp, sf_liegenschaften) {
  sf_bp_geo <- df_bp |>
    get_geo_info(sf_liegenschaften) |>
    sf::st_as_sf(crs = 2056) |>
    sf::st_cast("POLYGON") |>
    create_map_input() |>
    dplyr::mutate(
      entryDeadline = ifelse(is.na(entryDeadline), "siehe Online-Publikation", entryDeadline),
      url = paste0("https://amtsblatt.zh.ch/#!/search/publications/detail/", id)
    )

  return(sf_bp_geo)
}


#' Create an Interactive Leaflet Map of Building Permits
#'
#' Generates an interactive Leaflet map from spatial building permit data.
#' Each polygon includes a popup with details such as publication number,
#' publication date, entry deadline, address, and overlapping permits.
#'
#' @param sf_bp_geo An `sf` object containing spatial building permit data,
#' including #' attributes such as `publicationNumber`, `entryDeadline`,
#' `address`, and `url`.
#' @param days_of_data Integer or Character. If a numeric value is provided, it
#' specifies the number of days in the past from which to retrieve publications.
#' If set to `"all"`, all publications will be displayed starting from January 2025.
#'
#' @return A Leaflet map widget displaying the building permit polygons and
#' associated information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_map(sf_bp_geo)
#' }
create_map <- function(sf_bp_geo, days_of_data = 20) {
  
  if (days_of_data != "all"){
    start_date = as.character(Sys.Date()-days_of_data)
  } else {
    start_date = "2025-01-01"
  }
  
  sf_bp_geo <- sf_bp_geo |> 
    dplyr::filter(publicationDate >= start_date)

  ####
  intersected_poly <- sf_bp_geo |>
    sf::st_intersects()

  intersected_poly <- purrr::map(seq_along(intersected_poly), ~ {
    intersected_poly[[.x]][intersected_poly[[.x]] != .x]
  })

  ####
  publicationNumber_intersected_poly <- purrr::map(intersected_poly, ~ {
    sf_bp_geo$publicationNumber[.x]
  })

  ####
  url_intersected_poly <- purrr::map(intersected_poly, ~ {
    sf_bp_geo$url[.x]
  })

  ####
  url_intersected_poly_merged <- purrr::map2_chr(publicationNumber_intersected_poly, url_intersected_poly, ~ {
    paste("<a href='", .y, "' target = '_blank'>", .x, "</a></b>", collapse = "<br/>")
  })

  url_intersected_poly_merged <- ifelse(url_intersected_poly_merged == "<a href='  ' target = 'blank'>  </a></b>",
    "-",
    url_intersected_poly_merged
  )

  sf_bp_geo_wgs84 <- sf_bp_geo |>
    sf::st_transform(crs = 4326) |>
    dplyr::mutate(popup_text = paste0(
      "<b> Meldungsnummer: <a href='", url, "' target = 'blank'>", publicationNumber, "</a> </b>",
      "<br/>",
      "<b>Publikationsdatum: </b>", publicationDate,
      "<br/>",
      "<b>Planauflage bis: </b>", entryDeadline,
      "<br/>",
      "<b>Adresse: </b>", address,
      "<br/>",
      "<b>&Uuml;berlappende Bauprojekte:</b>",
      "<br/>",
      url_intersected_poly_merged
    ))


  leaflet::leaflet(sf_bp_geo_wgs84) |>
    leaflet::addTiles("https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
      attribution = paste(
        '&copy; <a href="https://openstreetmap.org">OpenStreetMap</a> contributors',
        '&copy; <a href="https://cartodb.com/attributions">CartoDB</a>'
      )
    ) |>
    leaflet::addPolygons(
      fillColor = "blue",
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.2,
      popup = ~popup_text
    ) |>
    leaflet::fitBounds(
      lng1 = min(sf::st_bbox(sf_bp_geo_wgs84)["xmin"]),
      lat1 = min(sf::st_bbox(sf_bp_geo_wgs84)["ymin"]),
      lng2 = max(sf::st_bbox(sf_bp_geo_wgs84)["xmax"]),
      lat2 = max(sf::st_bbox(sf_bp_geo_wgs84)["ymax"])
    )
}
