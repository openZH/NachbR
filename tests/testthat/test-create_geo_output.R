testthat::test_that("multiplication works", {
  
  # Liegenschaften
  dsn <- testthat::test_path("fixtures/geo_test_data.gpkg")
  sf_liegenschaften_test <- sf::st_read(dsn = dsn, layer = "test_liegenschaften")
  
  #xml to df to sf
  xml_file <- testthat::test_path("fixtures/geo_test.xml")
  sf_map <- create_baupub_df(xml_file) |> 
    add_spatial_information(sf_liegenschaften = sf_liegenschaften_test) |> 
    dplyr::select(id, publicationNumber, publicationDate, entryDeadline, 
                  expirationDate, projectDescription, address, url, geom)
  

  
  # check if sf was correctly built
  sf_map_test <- sf::st_read(dsn = dsn, layer = "test_sf_map",
                             crs = 2056)
  
  sf_map_test <- tibble::as_tibble(sf_map_test)
  sf_map_test <- sf::st_as_sf(sf_map_test)
  
  testthat::expect_equal(sf_map, sf_map_test)
  
  })

