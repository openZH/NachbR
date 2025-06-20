testthat::test_that("XML-file is parsed and a df is built correctly", {
  
  test_data <- readRDS(testthat::test_path("fixtures/test_data.RDS")) |> 
    # remove last updated since it would not be identical over time
    dplyr::select(-last_updated)
  
  file <- testthat::test_path("fixtures/test.xml")
  
  
  df_clean <- create_baupub_df(file) |> 
    dplyr::select(-last_updated)
  
  testthat::expect_equal(df_clean, test_data)  
})
