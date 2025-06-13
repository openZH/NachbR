#' Create or Replicate the Open Government Data (OGD) Baugesuche Data Frame
#'
#' Downloads and processes XML publications from the Amtsblattportal of the
#' canton of Zurich. If the Baugesuche OGD resource is already available online,
#'  it is used as a baseline and new entries are appended. Otherwise, all
#'  available publications are downloaded and processed.
#'
#' @param days_of_data Integer or Character. If a numeric value is provided, it
#' specifies the number of days in the past from which to retrieve publications.
#' If set to `"all"`, all available publications will be downloaded.
#' @param base_file_path Character. The base file path for storing or referencing
#' #' local files in future implementations.
#'
#' @return A data frame containing the processed publication data, including
#' legal entity information. If no new data is found and an existing dataset is
#' available, it returns the existing dataset.
#'
#' @details
#' The function interacts with:
#' - The Zurich Statistics Office's OGD resource (a CSV hosted online),
#' - The Amtsblattportal API (for retrieving current XML publications).
#'
#' It determines whether to fetch all data, new data only, or return an existing
#' resource depending on the `days_of_data` argument and the presence of new
#' publication URLs.
#'
#' @export
create_OGD_df <- function(days_of_data, base_file_path) {

  ## Setup ---------------------------------------------------------------------
  # load OGD-distribution, if it exists
 resp <-  httr2::request("https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002982_00006183.csv") |> 
   httr2::req_method("HEAD") |> 
   httr2::req_perform()
 
  if (httr2::resp_status(resp) == 200) {

    df_bp <- utils::read.csv("https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002982_00006183.csv")
    } else {
    df_bp <- NULL
  }

  # Define the Amtsblatt-URL and parameters
  url <- "https://amtsblattportal.ch/api/v1/publications/xml?"

  # Define page size
  page_size <- 100

  if (days_of_data == "all") {
    start <- ""
  } else {
    start <- as.character(Sys.Date() - days_of_data)
  }

  params <- list(
    publicationStates = "PUBLISHED",
    rubrics = "BP-ZH",
    subrubrics = "BP-ZH01",
    publicationDate.start = start,
    publicationDate.end = as.character(Sys.Date()),
    cantons = "ZH",
    pageRequest.size = page_size
  )

  ## Get the URLs of the new publication URLs ----------------------------------

  # get all necessary urls
  new_url <- get_new_pub_url(page_size, url, params, df_bp)

  ## Get publications ----------------------------------------------------------

  if (is.null(df_bp)) {
    ### Case 1: OGD-resource does not exist yet --------------------------------

    if (days_of_data != "all") {
      warning("If you want to download all available publications, set 'days_of_data' to 'all'.")
    }
    
    dp_bp_new <- create_clean_df(new_url) |> 
      dplyr::arrange(dplyr::desc(publicationDate))

    cat(paste0("actual new entries added to data frame: ", length(new_url), "\n"))

    return(df_bp_new)
  

  } else if (!is.null(df_bp) & length(new_url) != 0) {
    ### Case 2: OGD-resource exists and there are new publications -------------

    df_bp_new <- create_clean_df(new_url)

    df_bp <- dplyr::bind_rows(df_bp, df_bp_new) |>
      dplyr::arrange(dplyr::desc(publicationDate))

    cat(paste0("actual new entries added to data frame: ", length(new_url), "\n"))

    return(df_bp)

  } else {
    ### Case 3: OGD-resource exists but there are NO new publications ----------

    message("The data is already up-to-date. Hence, the OGD-resource is returned.")

    return(df_bp)
  }

}
