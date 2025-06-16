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
#' df <- create_clean_df(urls)
#' }
create_clean_df <- function(url){
  
  
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



#' Clean raw publication data frame
#'
#' This function cleans that raw publication data that was already processed to
#' wide data frame. Its main purpose is to clean and unnests the cadaster numbers.
#'
#' @param df_bp An expanded data frame in wide format consisting of publications
#'
#' @export
#'
clean_df_bp <- function(df_bp) {
  integer_cols <- c(
    # project zip-code
    "projectLocation_address_swissZipCode",
    "localization_municipalityId_key",
    # legal forms
    "buildingContractor_company_legalForm", 
    "projectFramer_company_legalForm",
    "delegation_buildingContractor_company_legalForm",
    # zip codes of involved parties
    "buildingContractor_company_address_swissZipCode",
    "projectFramer_company_address_swissZipCode",
    "delegation_buildingContractor_company_address_swissZipCode",
    "projectLocation_address_swissZipCode",
    # indexes in case of more than one involved party per category
    "buildingContractor_index",
    "projectFramer_index",
    "delegation_buildingContractor_index",
    "projectLocation_address_index"
  )


  df_bp <- df_bp |>
    clean_cadaster() |>
    # unnest the cadaster column
    tidyr::separate_rows(districtCadastre_relation_cadastre, sep = ",") |> 
    dplyr::mutate(dplyr::across(tidyselect::any_of(integer_cols), as.integer)) |>
    dplyr::rename(
      bfs_nr = localization_municipalityId_key,
      municipality_name = localization_municipalityId_term_de
    ) |>
    # removing all possible line breaks in character columns
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.character),
      ~ gsub("\n", " ", .))) |>
    dplyr::mutate(last_updated = as.character(Sys.Date()))
    dplyr::distinct()

  return(df_bp)
}



#' Define and select the columns of interest for the OGD data frame
#'
#' @inheritParams clean_df_bp
#'
#' @export
#'
get_columns_of_interest <- function(df_bp) {
  # List of columns of interest
  columns_of_interest <- c(
    "id",
    "publicationNumber",
    "publicationDate",
    "entryDeadline",
    "expirationDate",
    # INDEX HERE
    "bfs_nr_index",
    "bfs_nr",
    "municipality_name",
    "buildingContractor_selectType",
    "buildingContractor_legalEntity_selectType",
    "buildingContractor_noUID",
    # INDEX HERE
    "buildingContractor_index",
    "buildingContractor_company_legalForm",
    "buildingContractor_company_legalForm_de",
    "buildingContractor_company_address_swissZipCode",
    "buildingContractor_company_address_town",
    "projectFramer_selectType",
    "projectFramer_legalEntity_selectType",
    "projectFramer_noUID",
    # INDEX HERE
    "projectFramer_index",
    "projectFramer_company_legalForm",
    "projectFramer_company_legalForm_de",
    "projectFramer_company_address_swissZipCode",
    "projectFramer_company_address_town",
    "delegation_selectType",
    "delegation_buildingContractor_legalEntity_selectType",
    "delegation_buildingContractor_noUID",
    # INDEX HERE
    "delegation_buildingContractor_index",
    "delegation_buildingContractor_company_legalForm",
    "delegation_buildingContractor_company_legalForm_de",
    "delegation_buildingContractor_company_address_swissZipCode",
    "delegation_buildingContractor_company_address_town",
    "projectDescription",
    # INDEX HERE
    "projectLocation_address_index",
    "projectLocation_address_street",
    "projectLocation_address_houseNumber",
    "projectLocation_address_swissZipCode",
    "projectLocation_address_town",
    "districtCadastre_relation_cadastre",
    "districtCadastre_relation_cadastre_raw",
    "districtCadastre_relation_buildingZone",
    "districtCadastre_relation_district"
    "last_updated",
    "geometry"
  )

  df <- df_bp |> dplyr::select(tidyr::any_of(columns_of_interest))

  return(df)
}


#' Clean the cadaster numbers
#'
#' Clean cadaster numbers with regular expressions. Example: For a given publication
#' `c("A2000, A2001, Vers-Nr. 20003, 20004")` becomes `c("A2000, A2001")`.
#'
#' @inheritParams clean_df_bp
#'
#' @export
#'
clean_cadaster <- function(df_bp) {
  
  # note that "gsub" and "sub" are chosen consciousely because sometimes you only
  # want to replace a pattern once
  
  df <- df_bp |>
    ## cleaning cadaster-numbers:
    dplyr::mutate(
      districtCadastre_relation_cadastre_raw = districtCadastre_relation_cadastre
      ) |>
    # replace all "und" with ","
    dplyr::mutate(
      districtCadastre_relation_cadastre = 
        gsub("und", ",", districtCadastre_relation_cadastre)
    ) |>
    # if there is the string "Vers" or "vers", only keep everything up to 
    # the "," before that pattern
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        sub("^(.*),\\s*[^,]*[Vv]ers.*", "\\1", districtCadastre_relation_cadastre)
    ) |>
    # if there is the string "Vers" or "vers", only keep everything up to 
    #the "/" before that pattern
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        sub("^(.*?)/.*[Vv]ers.*", "\\1", districtCadastre_relation_cadastre)
    ) |>
    # remove all brackets and their content
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        gsub("\\s?\\([^)]*\\)", "", districtCadastre_relation_cadastre)
    ) |>
    # remove all kat.nr
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        sub("(?i)(kat).*?\\s", "", districtCadastre_relation_cadastre)
    ) |>
    # replace all "+" with ","
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        gsub("\\+", ",", districtCadastre_relation_cadastre)
    ) |>
    # replacing all "&" with ","
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        gsub("\\&", ",", districtCadastre_relation_cadastre)
    ) |>
    # remove all "gebäude"-string and everything after until there is another comma
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        gsub("(?i)geb\\u00e4ude*", ",", districtCadastre_relation_cadastre)
    ) |>
    # replace all "/" with ","
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        gsub("\\/", ",", districtCadastre_relation_cadastre)
    ) |>
    # remove all substrings in that only consist of letters (this does not
    # include the potential commas between them)
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        gsub("\\b[A-Za-z]+\\b", "", districtCadastre_relation_cadastre)
    ) |>
    # Remove all whitespaces
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        gsub(" ", "", districtCadastre_relation_cadastre, fixed = TRUE)
    ) |>
    # Remove all potential multiple commas in a row
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        gsub(",,+", "", districtCadastre_relation_cadastre)
    ) |>
    # Remove all potential commas at the very beginning or end of a string
    dplyr::mutate(
      districtCadastre_relation_cadastre =
        gsub("^,|,$", "", districtCadastre_relation_cadastre)
    )

  return(df)
}



#' Get legal entity info for companies
#'
#' This function adds the legal entitiy info for building contractor, project
#' framer and building contractor delegation companies.
#'
#' @inheritParams clean_df_bp
#'
#' @export
#'
get_legal_entity_info <- function(df_bp) {
  legal_entity_info <- read.csv(
    "https://api.i14y.admin.ch/api/public/v1/concepts/08dad8ff-f18a-560b-bfa6-20767f2afb17/codelist-entries/exports/csv"
  ) |>
    dplyr::rename_with(tolower) |>
    dplyr::select(name_de, name_en, code)

  if ("buildingContractor_company_legalForm" %in% names(df_bp)) {
    df_bp <- df_bp |>
      dplyr::left_join(legal_entity_info,
                       by = c("buildingContractor_company_legalForm" = "code")) |>
      dplyr::rename(buildingContractor_company_legalForm_de = name_de)
  }

  if ("projectFramer_company_legalForm" %in% names(df_bp)) {
    df_bp <- df_bp |>
      dplyr::left_join(legal_entity_info,
                       by = c("projectFramer_company_legalForm" = "code")) |>
      dplyr::rename(projectFramer_company_legalForm_de = name_de)
  }

  if ("delegation_buildingContractor_company_legalForm" %in% names(df_bp)) {
    df_bp <- df_bp |>
      dplyr::left_join(legal_entity_info,
                       by = c("delegation_buildingContractor_company_legalForm" = "code")) |>
      dplyr::rename(delegation_buildingContractor_company_legalForm_de = name_de)
  }

  return(df_bp)
}





#' Prepare Address Data for Mapping
#'
#' Aggregates and formats geographic and descriptive information from a data
#' frame containing project-related geolocation and metadata for use in mapping
#' applications.
#'
#' @param df_bp_geo A data frame containing project and address information,
#' including IDs, publication dates, deadlines, and components of the project
#' location address.
#'
#' @returns A data frame with unique project entries and a cleaned, concatenated
#' address field.
#'
#' @export
#'
create_map_input <- function(df_bp_geo) {
  df_to_be_mapped <- df_bp_geo |>
    dplyr::group_by(
      id,
      publicationNumber,
      publicationDate,
      entryDeadline,
      expirationDate,
      projectDescription,
      projectLocation_address_street,
      projectLocation_address_houseNumber,
      projectLocation_address_swissZipCode,
      projectLocation_address_town
    ) |>
    dplyr::summarise() |>
    dplyr::ungroup() |>
    dplyr::distinct() |>
    dplyr::group_by(
      id,
      publicationNumber,
      publicationDate,
      entryDeadline,
      expirationDate,
      projectDescription
    ) |>
    dplyr::summarise(address = paste0(
      projectLocation_address_street, " ",
      projectLocation_address_houseNumber, ", ",
      projectLocation_address_swissZipCode, " ",
      projectLocation_address_town,
      collapse = "; "
    )) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # replacing NA with ""
      address = gsub("NA", "", address),
      # replacing potential double commas with a single comma
      address = gsub(",,+", ",", address),
      # replacing all white spaces in front of a comma (perl is required for
      # for the look ahead part of the regex "(?=,)" )
      address = gsub("\\s+(?=,)", "", address, perl = TRUE),
      # replacing all leading or trailing commas
      address = gsub("^,|,$", "", address),
      # remove all leading, trailing or otherwise excess white spaces
      address = stringr::str_squish(address)
    )

  return(df_to_be_mapped)
}
