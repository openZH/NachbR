testthat::test_that("Cadastre numbers are cleaned correctly", {
  
  file <- testthat::test_path("fixtures/test.xml")
  
  
  fixture_path <- testthat::test_path("fixtures/mydata.rds")
  xml <- xml2::read_xml(file)
  df_raw <- xml_to_df(xml)
  
  
  cadastre <- df_raw |> 
    clean_cadaster() |> 
    dplyr::distinct(districtCadastre_relation_cadastre) |> 
    dplyr::pull()
  
  
  testthat::expect_equal(cadastre, "3344,3499,3999,3445,6666,3380,3383")
  
})


testthat::test_that("blabla", {
  
  test_data <- readRDS(testthat::test_path("fixtures/test_data.RDS"))
  
  file <- testthat::test_path("fixtures/test.xml")
  
  xml <- xml2::read_xml(file)
  df_raw <- xml_to_df(xml)
  
  df_clean <- df_raw |>
    # adjust their names, unnest cadaster nr
    clean_df_bp() |>
    get_legal_entity_info() |>
    get_columns_of_interest()
  
  
  testthat::expect_equal(df_clean, test_data)  
})
