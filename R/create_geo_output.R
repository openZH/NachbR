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



#' Calculate Share of Non-Georeferenced Building Permits
#'
#' Computes the percentage of building permit applications within a given date 
#' range that do not contain valid geometries. This is useful for assessing 
#' the completeness of spatial information in the dataset. 
#'
#' @param sf_bp_geo An `sf` object containing building permit applications, 
#' including a geometry column and a `publicationDate` attribute.
#' @param start_date A character string in the format "yyyy-mm-dd" specifying 
#' the first day to include in the calculation.
#' @param end_date A character string in the format "yyyy-mm-dd" specifying 
#' the last day to include in the calculation.
#'
#' @return A numeric value representing the percentage of permits without valid 
#' geometries, rounded to two decimal places.
#'
#'
#' @examples
#' \dontrun{
#' calc_geo_availability(sf_bp_geo, start_date = "2025-01-01", end_date = "2025-07-15")
#' }
calc_geo_availability <- function(sf_bp_geo, start_date, end_date) {
  
  sf_bp_geo <- sf_bp_geo |> 
      dplyr::mutate(publicationDate = as.Date(publicationDate)) |> 
      dplyr::filter(publicationDate >= as.Date(start_date) & 
                      publicationDate <= as.Date(end_date))
  
  share <- sum(!sf::st_is_empty(sf_bp_geo)) / nrow(sf_bp_geo)
  share <- round(share * 100, 2)

  return(share)
  }



#' Create an Interactive Leaflet Map of Building Permits
#'
#' Generates an interactive Leaflet map from spatial building permit data.
#' Each polygon includes a popup with details such as publication number,
#' publication date, entry deadline, address, and overlapping permits. The
#' default observation period is set to 20 days, as this is the timeframe during
#' which objections to the project can be submitted.
#'
#' @param sf_bp_geo An `sf` object containing spatial building permit data,
#' including attributes such as `publicationNumber`, `entryDeadline`,
#' `address`, and `url`.
#' @param start_date A character date in the format "yyyy-mm-dd" representing the
#' first day for which data should be included.
#' first day for which data should be included.
#' @param end_date A character date in the format "yyyy-mm-dd" representing the 
#' the last day for which data should be included.
#' @return A Leaflet map widget displaying the building permit polygons and
#' associated information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # creating a map of the last 20 days
#' create_map(sf_bp_geo)
#' 
#' # customize period of retrieval
#' sf_bp_geo |> 
#' create_map(start_date = "20205-01-01", end_date = "2025-07-15")
#' }
create_map <- function(sf_bp_geo, 
                       start_date = as.character(Sys.Date()-20),
                       end_date = as.character(Sys.Date())) {

  
  # Warnings
  if (start_date < "2025-01-01") {
    cli::cli_warn("There are no geo-referenced building permit applications before 2025-01-01.")
  }
  
  sf_bp_geo <- sf_bp_geo |> 
    dplyr::filter(publicationDate >= start_date & publicationDate <= end_date)

  if (nrow(sf_bp_geo) == 0) {
    cli::cli_abort("There are no building permit application for this period.")
  }
    
  # find polygons that overlap (indexes of polygons)
  intersected_poly <- sf_bp_geo |>
    sf::st_intersects()
  
  # isolate the overlapping polygons that do not have the same index
  # --> making sure publication 1 is not categorized as "overlapping with 
  # publication 1"
  intersected_poly <- purrr::map(seq_along(intersected_poly), ~ {
    intersected_poly[[.x]][intersected_poly[[.x]] != .x]
  })
  
  # isolate publication numbers of overlapping polygons
  publicationNumber_intersected_poly <- purrr::map(intersected_poly, ~ {
    sf_bp_geo$publicationNumber[.x]
  })

  # isolte url of overlapping polygons
  url_intersected_poly <- purrr::map(intersected_poly, ~ {
    sf_bp_geo$url[.x]
  })

  # create hyperlinks
  url_intersected_poly_merged <- purrr::map2_chr(publicationNumber_intersected_poly, url_intersected_poly, ~ {
    paste("<a href='", .y, "' target = '_blank'>", .x, "</a></b>", collapse = "<br/>")
  })

  url_intersected_poly_merged <- ifelse(url_intersected_poly_merged == "<a href='  ' target = 'blank'>  </a></b>",
    "-",
    url_intersected_poly_merged
  )

  # define enriched input for leaflet map
  sf_bp_geo_wgs84 <- sf_bp_geo |>
    sf::st_transform(crs = 4326) |>
    dplyr::mutate(popup_text = paste0(
      "<b> Meldungsnummer: </b><a href='", url, "' target = 'blank'>", publicationNumber, "</a>",
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

  # calculate share of liegenschaften that could not be linked to a polygon
  available_share <- calc_geo_availability(sf_bp_geo, start_date, end_date) |> 
    as.character() |> gsub(pattern = "\\.", replacement = ",")
  observed_period <- paste0(format(as.Date(start_date), "%d.%m.%Y"),
                            " - " , 
                            format(as.Date(end_date), "%d.%m.%Y"))

  # Define HTML for the infobox
  info.box <- htmltools::HTML(paste0(
    htmltools::HTML(
      '<div class="modal fade" id="infobox" role="dialog">
      <div class="modal-dialog"><!-- Modal content-->
      <div class="modal-content">
      <div class="modal-header">
      <button type="button" class="close" data-dismiss="modal">&times;</button>'
    ),
    
    # Body
    htmltools::HTML(paste0('<h3>Nutzungshinweise</h3>
    <hr>
    
    <h4>Beobachtungszeitraum (', observed_period,')</h4>
    Die Karte zeigt alle Baugesuche der letzten 20 Tage in Z\u00FCrcher Gemeinden, 
    die eindeutig einer Liegenschaft zugeordnet werden k&oumlnnen. W\u00E4hrend der 
    20-t\u00E4gigen Auflagefrist k&oumlnnen bei der Baubeh&oumlrde Baurechtsentscheide verlangt 
    werden, die Grundlage f\u00FCr m&oumlgliche Rekurse sind. Die Karte wird t\u00E4glich 
    aktualisiert.
    
    <br/>
    <br/>

    
    <h4>Georeferenzierung</h4>
    <p>Aus Gr\u00FCnden der Datenqualit\u00E4t lassen sich nicht alle 
    Baugesuche eindeutig den entsprechenden Liegenschaften zuordnen. Im aktuellen 
    Beobachtungszeitraum konnten rund ', available_share, '% aller Baugesuche 
    mindestens einer Liegenschaft zugeordnet werden.
    
    <br/>
    <br/>
    
    <b>Diese Visualisierung der laufenden Baugesuche dient als erg\u00E4nzende 
    Informationsquelle. Rechtsverbindlich ist ausschliesslich die Publikation im 
    <a href="https://amtsblatt.zh.ch/#!/gazette">Amtsblatt des Kantons Z\u00FCrich</a>.</b>

    <br/>
    <br/>
    
    <h4>Datenquelle und weiterf\u00FChrende Informationen</h4>
    
    Die verbindliche Hauptquelle ist das 
    <a href="https://amtsblatt.zh.ch/#!/gazette">Amtsblatt des Kantons Z\u00FCrich</a>, 
    in dem s\u00E4mtliche Baugesuche publiziert werden. Die hier dargestellten Daten 
    werden von der Fach- und Koordinationsstelle OGD des Kantons Z\u00FCrich aufbereitet 
    und zur Verf\u00FCgung gestellt.
    
    <br/>
    <br/>
    
    Weitere Datens\u00E4tze und Informationen zu Baugesuchen im Kanton Z\u00FCrich 
    (z.B. ein Datensatz, der auch die nicht georeferenzierbaren Baugesuche umfasst) 
    sind in unserem
    <a href="https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/2982@statistisches-amt-kanton-zuerich">Datenkatalog</a>
    zu finden.</p>'
                           )
                    ),
    
    # Closing divs
    htmltools::HTML('</div><div class="modal-footer"></div></div>')
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
    ) |> 
    leaflet.extras::addBootstrapDependency() |> 
    leaflet::addEasyButton(leaflet::easyButton(
      icon = "fa-info-circle", title = "Map Information",
      onClick = htmlwidgets::JS("function(btn, map){ $('#infobox').modal('show'); }")
    )) %>% # Trigger the infobox
    htmlwidgets::appendContent(info.box)

}
