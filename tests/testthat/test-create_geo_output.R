testthat::test_that("spatial information is added correctly", {
  # Liegenschaften
  dsn <- testthat::test_path("fixtures/geo_test_data.gpkg")
  sf_liegenschaften_test <- sf::st_read(dsn = dsn, layer = "test_liegenschaften")

  # xml to df to sf
  xml_file <- testthat::test_path("fixtures/geo_test.xml")
  sf_for_map <- create_baupub_df(xml_file) |>
    add_spatial_information(sf_liegenschaften = sf_liegenschaften_test) |>
    dplyr::select(
      id, publicationNumber, publicationDate, entryDeadline,
      expirationDate, projectDescription, address, url, geom
    )



  # check if sf was correctly built
  sf_for_map_test <- sf::st_read(
    dsn = dsn, layer = "test_sf_map",
    crs = 2056
  )

  sf_for_map_test <- tibble::as_tibble(sf_for_map_test)
  sf_for_map_test <- sf::st_as_sf(sf_for_map_test)

  testthat::expect_equal(sf_for_map, sf_for_map_test)
})




testthat::test_that("a map is create", {
  # Liegenschaften
  dsn <- testthat::test_path("fixtures/geo_test_data.gpkg")
  sf_liegenschaften_test <- sf::st_read(dsn = dsn, layer = "test_liegenschaften")

  # check if sf was correctly built
  sf_for_map_test <- sf::st_read(
    dsn = dsn, layer = "test_sf_map",
    crs = 2056
  )

  map <- sf_for_map_test |> create_map()
  

  testthat::expect_equal(inherits(map, "leaflet"), TRUE)
})
