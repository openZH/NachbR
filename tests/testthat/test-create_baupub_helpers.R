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


