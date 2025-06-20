#' Create a Clean Data Frame from XML URLs
#'
#' Reads and processes a vector of XML URLs into a cleaned data frame. Each XML
#' file is parsed into a data frame, combined into one, and then cleaned and
#' enriched using several helper functions.
#'
#' @param url A character vector of URLs pointing to XML files to be read and
#' processed.
#'
#' @return A cleaned data frame containing processed information extracted from
#' the XML files.
#' The returned data is cleaned, annotated with legal entity information, and
#' limited to selected columns of interest.
#'
#' @details Each URL is read and converted to a data frame using
#' \code{xml_to_df}, then passed through a cleaning and transformation pipeline.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' urls <- c("https://amtsblattportal.ch/api/v1/publications/1cebf554-8d76-46e3-b580-0e256095b5c8/xml",
#'           "https://amtsblattportal.ch/api/v1/publications/ae8db203-9fa6-4af1-a235-190d365950b6/xml")
#' 
#' 
#' df <- create_baupub_df(urls)
#' }
create_baupub_df <- function(url){
  
  
  # process xml files
  df_bp_raw <- purrr::map2_df(url, seq_along(url), ~ {
    xml <- xml2::read_xml(.x)
    df <- xml_to_df(xml)
    print(.y)
    return(df)
  })
  
  
  df_bp_clean <- df_bp_raw |>
    # adjust their names, unnest cadaster nr
    clean_df_bp() |>
    get_legal_entity_info() |>
    get_columns_of_interest()
  
  return(df_bp_clean)
}

