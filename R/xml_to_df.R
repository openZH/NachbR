#' A single Baupublikation-xml to data frame
#'
#' This function flattens a single Baupublikation-xml by extracting its leaves
#' and expanding the data if there are, for example, multiple locations.
#'
#' @param xml A xml-file representing a single Baupublikation
#'
#' @return A qubic data frame in wide format
#' @export
#'
xml_to_df <- function(xml) {
  df <- xml |>
    extract_leaves()


  index_vars <- setdiff(names(df), c("path", "value"))

  df_exp <- df

  # if there are indexes, then expand the data frame
  if (length(index_vars != 0)) {
    # FIXME: something else the "for i in..."?
    for (i in index_vars) {
      df_exp <- df_exp |>
        expand_df(i)
    }
  }

  df_exp_wide <- df_exp |>
    # Replace column names to make them easier to understand
    tidyr::pivot_wider(names_from = path, values_from = value) |>
    dplyr::rename_with(~ gsub(pattern = "_meta_",  replacement = "", x = .)) |>
    dplyr::rename_with(~ gsub(pattern = "_content_", replacement = "", x = .)) |>
    dplyr::rename_with(~ gsub(pattern = "_legalEntity_multi_companies_", replacement = "_", x = .)) |>
    dplyr::rename_with(~ gsub(pattern = "_multi_companies_", replacement = "_", x = .))

  return(df_exp_wide)
}


#' Extract leafs from xml and transform to data frame
#'
#' Function to recursively extract all leaf nodes with their full paths.
#'
#' @param node An arbitrary node which should be flattend, for the
#' Baupublikationen-case it is the root node
#' @param path An empty path that is enriched by iterating through the nodes.
#' A single path is equivalent to variable name
#' @param index_list An empty list which is updated if there multiple nodes/leafs
#' with the same name
#'
#' @return A data frame in long format
#' @export
#'
extract_leaves <- function(node, path = "", index_list = list()) {
  # get the children of the of the top-most node
  children <- xml2::xml_children(node)

  if (length(children) == 0) {
    # If no children, return the value as a dataframe
    value <- xml2::xml_text(node)
    tibble::tibble(path = path, value = value, !!!index_list)
  } else {
    # If there are children, recurse through them
    purrr::map_dfr(seq_along(children), function(i) {
      child <- children[[i]]
      child_name <- xml2::xml_name(child)

      # Update path and index list
      new_path <- paste0(path, "_", child_name)
      new_index_list <- index_list

      # If multiple children have the same name, add an index
      if (sum(xml2::xml_name(children) == child_name) > 1) {
        new_index_list[[new_path]] <- i
      }
      # start again at the beginning of the function
      extract_leaves(child, new_path, new_index_list)
    })
  }
}



#' Expand data frame
#'
#' This function expands the data frame based on an index variable. In the input
#' data frame, the index variable is already expanded (e.g., location 1,
#' location 2, ...) while all other variables (here rows) only occur once. The
#' purpose of this function is therefore to expand all non-index variables by
#' the number of unique index values.
#'
#' @param df A data frame to be expanded
#' @param index_var A variable based on which the expansion should be conducted
#'
#' @return An expanded data frame
#' @export
#'
expand_df <- function(df, index_var) {

  # determine the unique index values
  unique_index_var <- df |>
    dplyr::select(tidyselect::any_of(index_var)) |>
    dplyr::filter(!is.na(!!rlang::sym(index_var))) |>
    dplyr::pull() |>
    unique()

  # preparing the index variable name for the output data frame
  index_name <- rlang::sym(paste0(index_var, "_index"))
  index_var <- rlang::sym(index_var)

  expanded_df <- df |>
    # create the expansion factor for all all rows that are not indexed
    dplyr::mutate(
      expansion_factor =
        ifelse(is.na(!!index_var), length(unique_index_var), 1)
    ) |>
    # expand every row by its designated expansion factor
    dplyr::slice(rep(seq_len(dplyr::n()), expansion_factor)) |>
    dplyr::group_by(path) |>
    # complete all NA in the index_var by path
    dplyr::mutate(!!index_var := ifelse(
      is.na(!!index_var),
      # repeat the unique index values until the a given path is "index"
      rep(unique_index_var, length.out = dplyr::n()),
      !!index_var
    )) |>
    dplyr::ungroup() |>
    dplyr::rename(!!index_name := !!index_var) |>
    dplyr::select(-expansion_factor)

  return(expanded_df)
}
