
############################################################################
# sequential data preparation

df_bp <- df_bp_raw |>
  clean_df_bp()

# check how many bfs-nr are available
table(!is.na(df_bp$bfs_nr))


# Check if the bfs-nr has only been introduced in 2025
df_2025 <- df_bp |>
  dplyr::mutate(publicationDate = as.POSIXct(publicationDate)) |>
  dplyr::filter(publicationDate > as.POSIXct("2024-12-31"))

x <- table(!is.na(df_2025$bfs_nr))
x

x[2] / sum(x)
# it seems to be the case that the bfs-nr was only introduced in 2025
# note that the true ratio is slightly different since we already unnest the
# cadastre-numbers --> increases total observations but ratio should approx. remain the same


df_bp_geo <- df_bp |>
  get_geo_info() |>
  get_legal_entity_info() |>
  dplyr::mutate(publicationDate = as.POSIXct(publicationDate)) |>
  dplyr::filter(publicationDate > as.POSIXct("2024-12-31"))




# Check for how many BauPubs we have data
table(!is.na(df_bp_geo$geo_match)) / nrow(df_bp_geo)


sf_bp_geo <- sf::st_as_sf(df_bp_geo, geometry = df_bp_geo$geometry, crs = 2056)


mapview::mapview(sf_bp_geo)

###########################################################################

# check how many polygons could be matched in 2025 based on bfs-nr. and cadastre-number
df_bp_geo_2025 <- df_bp |>
  dplyr::mutate(publicationDate = as.POSIXct(publicationDate)) |>
  dplyr::filter(publicationDate > as.POSIXct("2025-01-01"))

sum(sf::st_is_empty(df_bp_geo_2025$geometry)) / nrow(df_bp_geo_2025)

table(sf::st_is_empty(df_bp_geo_2025)) / nrow(df_bp_geo_2025)


###########################################################################

# BauPub, wo es Mehrfacheinträge gibt

# nur Personen
ls_bp[[3]]

# mehrere Einträge bei Personen
ls_bp[[93]]


# mehrere projectLocations
ls_bp[[140]]

# mehere districtCadastre
ls_bp[[736]]


# example IDs where multiple Cadastra numbers were correctly filled in:

"040720c4-9565-4875-b0d5-934a1032b4e9" # --> 3 Grundstücke 2024-07-12

"02b50d96-212b-4806-9efe-957f283e94db" # --> 2 Grundstücke 2024-04-19

"04a2a7e8-c670-47a8-bd68-5c623033b15f" # example where it is not correctly filled in 2025-01-31
