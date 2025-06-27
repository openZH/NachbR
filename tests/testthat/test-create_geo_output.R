testthat::test_that("spatial information is added correctly", {
  # Liegenschaften
  dsn <- testthat::test_path("fixtures/geo_test_data.gpkg")
  sf_liegenschaften_test <- sf::st_read(dsn = dsn, layer = "test_liegenschaften")

  # xml to df to sf
  xml_file <- testthat::test_path("fixtures/geo_test.xml")
  sf_map <- create_baupub_df(xml_file) |>
    add_spatial_information(sf_liegenschaften = sf_liegenschaften_test)
  
  
  # unnest multipolygon as the github-instances seems to load sub-polygons in a
  # random order
  sf_map_cast <- suppressWarnings(sf::st_cast(sf_map, "POLYGON"))
  
  first_polygon_entry <- sapply(1:3, function(x) sf_map_cast$geom[[x]][[1]][[1]])
  
  sf_map_cast <- sf_map_cast |> 
    dplyr::mutate(order = first_polygon_entry) |> 
    dplyr::arrange(order)
  
  
  
  # prepare data that should be tested against
  sf_test <- sf::st_read(
    dsn = dsn, layer = "test_sf_map",
    crs = 2056
  )
  
  sf_test_cast <- suppressWarnings(sf::st_cast(sf_test, "POLYGON"))
  
  first_polygon_entry_test <- sapply(1:3, function(x) sf_test_cast$geom[[x]][[1]][[1]])
  
  sf_test_cast <- sf_test_cast |> 
    dplyr::mutate(order = first_polygon_entry_test) |> 
    dplyr::arrange(order)
  
  
  
  
  # check if sf was correctly built
  coordinate_equality_test <- sapply(1:3, function(x) {
    
    
    all(sf::st_equals_exact(sf_map_cast[x,], 
                            sf_test_cast[x,], 
                            par = 1e-6, 
                            sparse = FALSE))
  })
  
  testthat::expect_true(all(coordinate_equality_test))
  
})


testthat::test_that("a map is create", {
  # Liegenschaften
  dsn <- testthat::test_path("fixtures/geo_test_data.gpkg")
  sf_liegenschaften_test <- sf::st_read(dsn = dsn, layer = "test_liegenschaften")

  # check if sf was correctly built
  sf_map_test <- sf::st_read(
    dsn = dsn, layer = "test_sf_map",
    crs = 2056
  )

  map <- sf_map_test |> create_map()
  

  testthat::expect_equal(inherits(map, "leaflet"), TRUE)
})
