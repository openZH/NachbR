

#' Download Baupublikationen xml-files
#'
#' Downloading available xml-files based on your input paramater consisting of 
#' the meta information and content of all Baupublikationen.
#'
#' @inheritParams create_OGD_df
#' @param path A path to a folder where new and already downloaded Baupublikatonen
#' are stored
#' @inheritParams total_entries_check
#' 
#' @details
#' Note that you can specify  `publicationDate.start` and `publicationDate.end`
#' in your "params" (also see example).
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#'   params <- list(
#'   publicationStates = "PUBLISHED",
#'   rubrics = "BP-ZH",
#'   subrubrics = "BP-ZH01",
#'   publicationDate.start = "2025-06-01",
#'   publicationDate.end = as.character(Sys.Date()),
#'   cantons = "ZH",
#'   pageRequest.size = 100
#'   )
#'   
#'   path <- "YOUR_STORAGE_FOlDER/"
#'   
#'   download_xml_files(path, params)
#' }
download_xml_files <- function(path, params) {

    # Define the Amtsblatt-URL and parameters
  url <- "https://amtsblattportal.ch/api/v1/publications/xml?"
  
  # Define page size
  page_size <- 100
  
  # Check if there are more than 9999 entries for the current API call
  total_entries_check(url, params)
  
  # get all necessary urls
  new_url <- get_new_pub_url(page_size, url, params, df_bp = NULL)
  
  purrr::walk(new_url, ~ {
    # it is clear that it would be easier to pass on the id instead of the of url
    # into the function, however, using the url simplifies things downstream
    # --> create_OGD_df()
    
    id <- .x |>
      sub(pattern = "https://amtsblattportal.ch/api/v1/publications/", 
          replacement = "") |>
      sub(pattern = "/xml", replacement = "")
    
    file <- paste0(path, id, ".xml")
    
    # Check if a given file already exists in the provided folder
    if (file.exists(file)) {
      return()
    } else {
      # retrieving a Baupublikation from Amtblatt-Portal
      
      xml_data <- xml2::read_xml(.x)
      
      print(which(new_url == .x))
      
      xml2::write_xml(xml_data, file = file)
    }
  })
}






#' Retrieving the url of all newly published Baupublikationen
#'
#' This function retrieves a vector that consists the IDs through which a single
#' publicaton can be identified and its content can be downloaded.
#'
#' @param page_size The number of entries per page through which it will be
#' iterrated. The smaller the number, the faster (hence default = 100).
#' @param df_bp Existing OGD-csv-distribution
#' @inheritParams total_entries_check
#' @export
#'
get_new_pub_url <- function(page_size = 100, 
                            url = "https://amtsblattportal.ch/api/v1/publications/xml?", 
                            params = list(
                              publicationStates = "PUBLISHED",
                              rubrics = "BP-ZH",
                              subrubrics = "BP-ZH01",
                              publicationDate.start = "2025-06-11",
                              publicationDate.end = as.character(Sys.Date()),
                              cantons = "ZH",
                              pageRequest.size = 100
                            ), 
                            df_bp = NULL) {
  
  # Check if there are more than 9999 entries for the current API call
  total_entries_check(url, params)
  
  df_all_url <- data.frame()

  max_entries_per_call <- 10000
  current_page <- 0 # pages start at 0
  entries_fetched <- 0

  # loop over pages until 10'000 entries are reached
  while (entries_fetched < max_entries_per_call) {
    # get urls and meta data for a given page (for example, 100 entries)
    response <- extract_url(current_page, url, params)

    # check the number of entries returned in response
    entries_count <- nrow(response)

    df_all_url <- rbind(df_all_url, response)

    # Update the total number of entries fetched
    entries_fetched <- entries_fetched + entries_count

    # Break if fewer than page_size entries are returned (end of data)
    if (entries_count < page_size) break

    # Move to the next page
    current_page <- current_page + 1
  }

  cat("\ndata retrieved from", current_page + 1, "pages\n")
  cat("possible new entries:", entries_fetched, "\n")

  df_all_url <- df_all_url |>
    dplyr::mutate(
      url =
        paste0("https://amtsblattportal.ch/api/v1/publications/", id, "/xml")
    )


  if (is.null(df_bp)) {

    all_url <- df_all_url |>
      dplyr::pull(url)

    return(all_url)
  } else {

    # retrieving existing IDs from OGD-distribution
    current_ids <- df_bp |>
      dplyr::pull(id) |>
      unique()

    # isolating potential new ids
    potential_new_ids <- df_all_url |>
      dplyr::pull(id)

    # isolating new ids
    new_ids <- potential_new_ids[!potential_new_ids %in% current_ids]

    new_url <- df_all_url |>
      dplyr::filter(id %in% new_ids) |>
      dplyr::pull(url)

    return(new_url)
  }
}




#' Extract all urls from a single page during the API call
#'
#' This function creates a data frame based on the current page of the pagination
#' process contain a selection of a given Baupuplikation's meta data including
#' the required `id`.
#'
#' @param page The current page number
#' @param base_url A character string, representing the base url for making the
#' API call.
#' @inheritParams total_entries_check
#'
#' @return Returns a data frame containing a selection of meta information of
#' all Baupublikationen of the current page including the `id`.
#' @export
#'
extract_url <- function(page, base_url, params) {
  params$pageRequest.page <- page

  # Perform the request and parse the XML content
  response <- httr2::request(base_url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_perform()

  xml_content <- httr2::resp_body_xml(response)

  publications <- xml2::xml_find_all(xml_content, ".//publication")

  # Iterate through publications and extract data
  publication_data <- purrr::map(publications, ~ {
    # Extract metadata fields
    meta <- xml2::xml_find_first(.x, "meta")

    data <- list(
      id = xml2::xml_text(xml2::xml_find_first(meta, "id")),
      rubric = xml2::xml_text(xml2::xml_find_first(meta, "rubric")),
      subRubric = xml2::xml_text(xml2::xml_find_first(meta, "subRubric")),
      publicationNumber = xml2::xml_text(xml2::xml_find_first(meta, "publicationNumber")),
      publicationDate = xml2::xml_text(xml2::xml_find_first(meta, "publicationDate")),
      title_de = xml2::xml_text(xml2::xml_find_first(meta, "title/de"))
    )

    return(data)
  })

  df <- dplyr::bind_rows(publication_data)
  
  return(df)
}


#' Determine the maximal number of available Baupublikationen
#'
#' Sends a GET request to a specified URL with query parameters, parses the XML
#' response, and extracts the total number of entries. If the number of entries
#' exceeds 9,999, an error is raised, prompting the user to select a narrower 
#' time frame.
#'
#' @param url Skeleton url which has to be enriched with parameters
#' @param params A `list` with a selection of parameters that refers to the 
#' publication in a given Amtsblatt (also the one of other cantons than Zurich).
#' For the parameters for the Baupaublikationen, see the example.
#' For all parameters, see the
#' [API-Documentation](https://www.amtsblattportal.ch/docs/api/#_api_reference)
#' of the Amtsblattportal.
#' 
#' 
#' 
#' @details
#' It is also possible to determine the number of publications for a certain
#' period by adding the parameter `publicationDate.start` and `publicationDate.end`.
#' If so, at least `publicationDate.end` in the following format "YYYY-MM-DD" is
#' required. For more information regarding the parameters see the
#' [API-documentation](https://www.amtsblattportal.ch/docs/api/#_core_concepts).
#'
#' @return A numeric value of the number of the available Baupublikationen
#' @export
#' 
#' @examples
#' \dontrun{
#'   params <- list(
#'   publicationStates = "PUBLISHED",
#'   rubrics = "BP-ZH",
#'   subrubrics = "BP-ZH01",
#'   publicationDate.start = "2025-06-06",
#'   publicationDate.end = as.character(Sys.Date()),
#'   cantons = "ZH",
#'   pageRequest.size = 100
#'   )
#'   
#'   url <- "https://amtsblattportal.ch/api/v1/publications/xml?"
#'   
#'   total_entries_check(url, params) 
#' }
#'
total_entries_check <- function(url, params) {
  # Send the GET request with parameters
  xml_content <- httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_perform() |>
    httr2::resp_body_xml()
  
  # get the number of publications for that period
  n_pub <- as.numeric(xml2::xml_text(
    xml2::xml_find_first(xml_content, "//total")
  ))
  
  if (n_pub > 9999) {
    stop("Select a different time frame such that you have less than 10'000
         observations.")
  }
  
  return(n_pub)
}



