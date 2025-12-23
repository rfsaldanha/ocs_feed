res <- geobr::read_municipality(year = 2024, cache = FALSE)

df <- res |>
  sf::st_drop_geometry() |>
  dplyr::select(code_muni, name_muni, abbrev_state) |>
  dplyr::mutate(code_muni = as.character(code_muni)) |>
  dplyr::mutate(title = paste0(name_muni, ", ", abbrev_state)) |>
  dplyr::mutate(
    name_muni = stringi::stri_trans_general(
      str = name_muni,
      id = "Latin-ASCII"
    ),
    name_muni = stringi::stri_trans_tolower(name_muni),
    name_muni = stringr::str_replace_all(
      string = name_muni,
      pattern = " ",
      replacement = "_"
    ),
    name_muni = paste0(
      name_muni,
      "_",
      stringi::stri_trans_tolower(abbrev_state)
    ),
    name_muni = paste0("ocs_", name_muni)
  ) |>
  dplyr::select(code_muni, title, feed = name_muni)

saveRDS(object = df, file = "mun_feeds.rds")
